/* ----------------------------------------------------------------------------
GHC RTS Integration for mimalloc
Implementation of GHC RTS capability-based heap management.

Copyright (c) 2025 Ian Duncan
This is free software; you can redistribute it and/or modify it under the
terms of the BSD-3-Clause license.
-----------------------------------------------------------------------------*/

#ifdef MI_USE_GHC_CAPABILITIES

#include "mimalloc.h"
#include "mimalloc/internal.h"
#include "ghc-rts-integration.h"

// Forward declaration of _mi_heap_main (defined in init.c)
extern mi_heap_t _mi_heap_main;

// Note: We don't directly include GHC RTS headers here.
// Instead, Haskell code will call C functions to set/get capability info.
// This avoids complex RTS header dependencies during C compilation.

#include <string.h>
#include <stdbool.h>

/* -----------------------------------------------------------
  Global state for capability-based heaps
----------------------------------------------------------- */

// Array of heaps indexed by capability number
mi_heap_t* _mi_heaps_by_capability[MI_MAX_CAPABILITIES];

// Current known capability count (atomic for thread-safety during reinitialization)
_Atomic(unsigned int) _mi_capability_count = 0;

// Flag to track if we've initialized the RTS integration
static _Atomic(bool) _mi_ghc_initialized = false;

/* -----------------------------------------------------------
  GHC RTS API wrappers
----------------------------------------------------------- */

// Storage for capability information passed from Haskell
static _Atomic(unsigned int) _mi_current_capability_tls = 0;

bool _mi_ghc_rts_is_initialized(void) {
  return mi_atomic_load_relaxed(&_mi_ghc_initialized);
}

unsigned int _mi_ghc_get_num_capabilities(void) {
  // The capability count is set by Haskell code via _mi_set_num_capabilities
  unsigned int count = mi_atomic_load_relaxed(&_mi_capability_count);
  return (count > 0) ? count : 1;
}

unsigned int _mi_ghc_get_capability(void) {
  // The current capability is set by Haskell before each allocation
  // using _mi_set_current_capability (called from Haskell FFI)
  unsigned int cap = mi_atomic_load_relaxed(&_mi_current_capability_tls);
  
  // Bounds check
  if (cap >= MI_MAX_CAPABILITIES) {
    cap = 0;
  }
  
  return cap;
}

// Called from Haskell to set the number of capabilities
void _mi_set_num_capabilities(unsigned int num_caps) {
  if (num_caps > 0 && num_caps <= MI_MAX_CAPABILITIES) {
    mi_atomic_store_release(&_mi_capability_count, num_caps);
    if (!_mi_ghc_rts_is_initialized()) {
      _mi_init_capability_heaps(num_caps);
    }
  }
}

// Called from Haskell FFI to set current capability
// This should be called on the hot path (inlined in Haskell)
void _mi_set_current_capability(unsigned int cap) {
  if (cap < MI_MAX_CAPABILITIES) {
    mi_atomic_store_relaxed(&_mi_current_capability_tls, cap);
  }
}

/* -----------------------------------------------------------
  Capability heap initialization and management
----------------------------------------------------------- */

void _mi_init_capability_heaps(unsigned int num_capabilities) {
  if (num_capabilities == 0 || num_capabilities > MI_MAX_CAPABILITIES) {
    num_capabilities = 1;
  }
  
  // Initialize all heap pointers to the empty heap
  for (unsigned int i = 0; i < MI_MAX_CAPABILITIES; i++) {
    if (_mi_heaps_by_capability[i] == NULL) {
      _mi_heaps_by_capability[i] = (mi_heap_t*)&_mi_heap_empty;
    }
  }
  
  // For capability 0, we use the main heap (already statically allocated)
  _mi_heaps_by_capability[0] = &_mi_heap_main;
  
  // For other capabilities, we'll lazily initialize heaps on first access
  // This is handled in _mi_thread_heap_init() in init.c
  
  // Store the capability count
  mi_atomic_store_release(&_mi_capability_count, num_capabilities);
  mi_atomic_store_release(&_mi_ghc_initialized, true);
}

void _mi_reinitialize_for_capabilities(unsigned int new_cap_count) {
  if (new_cap_count == 0 || new_cap_count > MI_MAX_CAPABILITIES) {
    return;
  }
  
  unsigned int old_cap_count = mi_atomic_load_acquire(&_mi_capability_count);
  
  if (new_cap_count == old_cap_count) {
    return; // No change needed
  }
  
  // Update the capability count atomically
  mi_atomic_store_release(&_mi_capability_count, new_cap_count);
  
  if (new_cap_count > old_cap_count) {
    // Growing: initialize new capability slots to empty heap
    // They'll be lazily initialized on first access
    for (unsigned int i = old_cap_count; i < new_cap_count; i++) {
      if (i < MI_MAX_CAPABILITIES) {
        // Only set to empty if not already initialized
        mi_heap_t* current = _mi_heaps_by_capability[i];
        if (current == NULL || current == &_mi_heap_empty) {
          _mi_heaps_by_capability[i] = (mi_heap_t*)&_mi_heap_empty;
        }
      }
    }
  }
  else {
    // Shrinking: mark excess heaps for eventual cleanup
    // We don't immediately destroy them as there might be outstanding allocations
    // The heaps will be cleaned up when the process exits or when explicitly collected
    for (unsigned int i = new_cap_count; i < old_cap_count && i < MI_MAX_CAPABILITIES; i++) {
      mi_heap_t* heap = _mi_heaps_by_capability[i];
      if (heap != NULL && heap != &_mi_heap_empty && heap != &_mi_heap_main) {
        // Collect any free memory in this heap
        mi_heap_collect(heap, true /* force */);
        // Note: We don't delete the heap here as it might still be in use
        // It will be cleaned up during process shutdown
      }
    }
  }
}

#endif // MI_USE_GHC_CAPABILITIES

