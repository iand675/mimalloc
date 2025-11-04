/* ----------------------------------------------------------------------------
GHC RTS Integration for mimalloc
This file provides integration with the GHC Runtime System for capability-based
heap management instead of traditional thread-local storage.

Copyright (c) 2025 Ian Duncan
This is free software; you can redistribute it and/or modify it under the
terms of the BSD-3-Clause license.
-----------------------------------------------------------------------------*/
#pragma once
#ifndef MIMALLOC_GHC_RTS_INTEGRATION_H
#define MIMALLOC_GHC_RTS_INTEGRATION_H

#ifdef MI_USE_GHC_CAPABILITIES

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// Maximum number of capabilities supported
// GHC typically supports up to 256 capabilities
#define MI_MAX_CAPABILITIES 256

// Forward declarations
typedef struct mi_heap_s mi_heap_t;

/* -----------------------------------------------------------
  GHC RTS API wrappers
  
  These functions provide a safe wrapper around GHC RTS APIs
  for getting capability information.
----------------------------------------------------------- */

// Get the current capability number (0-based index)
// Returns 0 if not set or called before initialization
unsigned int _mi_ghc_get_capability(void);

// Get the total number of capabilities
// Returns 1 if called before RTS initialization
unsigned int _mi_ghc_get_num_capabilities(void);

// Check if the GHC RTS has been initialized
bool _mi_ghc_rts_is_initialized(void);

// Called from Haskell to set the number of capabilities
void _mi_set_num_capabilities(unsigned int num_caps);

// Called from Haskell to set the current capability
// This is called frequently, should be fast
void _mi_set_current_capability(unsigned int cap);

/* -----------------------------------------------------------
  Capability-based heap management
----------------------------------------------------------- */

// Array of heaps indexed by capability number
extern mi_heap_t* _mi_heaps_by_capability[MI_MAX_CAPABILITIES];

// Current known capability count (atomic)
extern _Atomic(unsigned int) _mi_capability_count;

// Reinitialize heaps for a new capability count
// This should be called after setNumCapabilities in Haskell
void _mi_reinitialize_for_capabilities(unsigned int new_cap_count);

// Initialize the capability heap array
void _mi_init_capability_heaps(unsigned int num_capabilities);

#endif // MI_USE_GHC_CAPABILITIES

#endif // MIMALLOC_GHC_RTS_INTEGRATION_H

