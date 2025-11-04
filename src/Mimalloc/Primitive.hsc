{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Mimalloc.Primitive
Description : Low-level FFI bindings to mimalloc
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

This module provides direct, low-level FFI bindings to the mimalloc memory allocator.
For most use cases, prefer the high-level interface in "Mimalloc" instead.

/Important/: Many functions in this module use thread-local storage and must be called
from OS-bound threads (created with 'Control.Concurrent.forkOS'). Functions operating
on explicit heap handles are safer for use with green threads.
-}
module Mimalloc.Primitive
  ( -- * Core Allocation
    mi_malloc
  , mi_calloc
  , mi_realloc
  , mi_expand
  , mi_free
  , mi_strdup
  , mi_strndup
  , mi_realpath

    -- * Small Allocations
  , mi_malloc_small
  , mi_zalloc_small
  , mi_zalloc

    -- * Counting Allocations
  , mi_mallocn
  , mi_reallocn
  , mi_reallocf

    -- * Size Queries
  , mi_usable_size
  , mi_good_size

    -- * Aligned Allocation
  , mi_malloc_aligned
  , mi_malloc_aligned_at
  , mi_zalloc_aligned
  , mi_zalloc_aligned_at
  , mi_calloc_aligned
  , mi_calloc_aligned_at
  , mi_realloc_aligned
  , mi_realloc_aligned_at

    -- * Zero-initialized Reallocation
  , mi_rezalloc
  , mi_recalloc
  , mi_rezalloc_aligned
  , mi_rezalloc_aligned_at
  , mi_recalloc_aligned
  , mi_recalloc_aligned_at

    -- * Heap Management
  , MiHeap
  , mi_heap_new
  , mi_heap_delete
  , mi_heap_destroy
  , mi_heap_set_default
  , mi_heap_get_default
  , mi_heap_get_backing
  , mi_heap_collect

    -- * Heap-specific Allocation
  , mi_heap_malloc
  , mi_heap_zalloc
  , mi_heap_calloc
  , mi_heap_mallocn
  , mi_heap_malloc_small
  , mi_heap_realloc
  , mi_heap_reallocn
  , mi_heap_reallocf
  , mi_heap_strdup
  , mi_heap_strndup
  , mi_heap_realpath

    -- * Heap-specific Aligned Allocation
  , mi_heap_malloc_aligned
  , mi_heap_malloc_aligned_at
  , mi_heap_zalloc_aligned
  , mi_heap_zalloc_aligned_at
  , mi_heap_calloc_aligned
  , mi_heap_calloc_aligned_at
  , mi_heap_realloc_aligned
  , mi_heap_realloc_aligned_at

    -- * Heap-specific Zero-initialized Reallocation
  , mi_heap_rezalloc
  , mi_heap_recalloc
  , mi_heap_rezalloc_aligned
  , mi_heap_rezalloc_aligned_at
  , mi_heap_recalloc_aligned
  , mi_heap_recalloc_aligned_at

    -- * Heap Analysis
  , mi_heap_contains_block
  , mi_heap_check_owned
  , mi_check_owned

    -- * Block Visiting
  , MiHeapArea(..)
  , MiBlockVisitFun
  , mi_heap_visit_blocks

    -- * Statistics and Introspection
  , mi_collect
  , mi_version
  , mi_stats_reset
  , mi_stats_merge
  , mi_stats_print
  , mi_process_info

    -- * Thread Management
  , mi_process_init
  , mi_process_done
  , mi_thread_init
  , mi_thread_done

    -- * Options
  , MiOption(..)
  , mi_option_is_enabled
  , mi_option_enable
  , mi_option_disable
  , mi_option_set_enabled
  , mi_option_set_enabled_default
  , mi_option_get
  , mi_option_get_clamp
  , mi_option_get_size
  , mi_option_set
  , mi_option_set_default

    -- * Posix-style Functions
  , mi_posix_memalign
  , mi_memalign
  , mi_valloc
  , mi_pvalloc
  , mi_aligned_alloc
  , mi_reallocarray
  , mi_cfree
  , mi_malloc_size
  , mi_malloc_usable_size
  , mi_free_size
  , mi_free_size_aligned
  , mi_free_aligned

    -- * Advanced Features
  , mi_is_in_heap_region
  , mi_is_redirected
  , mi_reserve_huge_os_pages_interleave
  , mi_reserve_huge_os_pages_at
  , mi_reserve_os_memory
  , mi_manage_os_memory

    -- * Arena Management
  , MiArenaId
  , mi_arena_area
  , mi_reserve_huge_os_pages_at_ex
  , mi_reserve_os_memory_ex
  , mi_manage_os_memory_ex
  , mi_heap_new_in_arena
  , mi_heap_new_ex

    -- * Experimental: Sub-processes
  , MiSubprocId
  , mi_subproc_main
  , mi_subproc_new
  , mi_subproc_delete
  , mi_subproc_add_current_thread

  ) where

#include "mimalloc.h"

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

-- -----------------------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------------------

-- | Opaque handle to a mimalloc heap
data MiHeap

-- | Arena identifier
type MiArenaId = CInt

-- | Sub-process identifier
type MiSubprocId = Ptr ()

-- | Heap area information
data MiHeapArea = MiHeapArea
  { heapAreaBlocks :: !(Ptr ())        -- ^ Start of the area containing heap blocks
  , heapAreaReserved :: !CSize         -- ^ Bytes reserved for this area (virtual)
  , heapAreaCommitted :: !CSize        -- ^ Current available bytes for this area
  , heapAreaUsed :: !CSize             -- ^ Number of allocated blocks
  , heapAreaBlockSize :: !CSize        -- ^ Size in bytes of each block
  , heapAreaFullBlockSize :: !CSize    -- ^ Size in bytes of a full block including padding and metadata
  , heapAreaHeapTag :: !CInt           -- ^ Heap tag associated with this area
  } deriving (Show, Eq)

instance Storable MiHeapArea where
  sizeOf _ = #{size mi_heap_area_t}
  alignment _ = #{alignment mi_heap_area_t}
  peek ptr = do
    blocks <- #{peek mi_heap_area_t, blocks} ptr
    reserved <- #{peek mi_heap_area_t, reserved} ptr
    committed <- #{peek mi_heap_area_t, committed} ptr
    used <- #{peek mi_heap_area_t, used} ptr
    blockSize <- #{peek mi_heap_area_t, block_size} ptr
    fullBlockSize <- #{peek mi_heap_area_t, full_block_size} ptr
    heapTag <- #{peek mi_heap_area_t, heap_tag} ptr
    return $ MiHeapArea blocks reserved committed used blockSize fullBlockSize heapTag
  poke ptr (MiHeapArea blocks reserved committed used blockSize fullBlockSize heapTag) = do
    #{poke mi_heap_area_t, blocks} ptr blocks
    #{poke mi_heap_area_t, reserved} ptr reserved
    #{poke mi_heap_area_t, committed} ptr committed
    #{poke mi_heap_area_t, used} ptr used
    #{poke mi_heap_area_t, block_size} ptr blockSize
    #{poke mi_heap_area_t, full_block_size} ptr fullBlockSize
    #{poke mi_heap_area_t, heap_tag} ptr heapTag

-- | Block visitor function type
type MiBlockVisitFun = Ptr MiHeap -> Ptr MiHeapArea -> Ptr () -> CSize -> Ptr () -> IO CBool

-- | Mimalloc configuration options
data MiOption
  = MiOptionShowErrors
  | MiOptionShowStats
  | MiOptionVerbose
  | MiOptionEagerCommit
  | MiOptionArenaEagerCommit
  | MiOptionPurgeDecommits
  | MiOptionAllowLargeOsPages
  | MiOptionReserveHugeOsPages
  | MiOptionReserveHugeOsPagesAt
  | MiOptionReserveOsMemory
  | MiOptionAbandonedPagePurge
  | MiOptionEagerCommitDelay
  | MiOptionPurgeDelay
  | MiOptionUseNumaNodes
  | MiOptionDisallowOsAlloc
  | MiOptionOsTag
  | MiOptionMaxErrors
  | MiOptionMaxWarnings
  | MiOptionMaxSegmentReclaim
  | MiOptionDestroyOnExit
  | MiOptionArenaReserve
  | MiOptionArenaPurgeMult
  | MiOptionPurgeExtendDelay
  | MiOptionAbandonedReclaimOnFree
  | MiOptionDisallowArenaAlloc
  | MiOptionRetryOnOom
  | MiOptionVisitAbandoned
  | MiOptionGuardedMin
  | MiOptionGuardedMax
  | MiOptionGuardedPrecise
  | MiOptionGuardedSampleRate
  | MiOptionGuardedSampleSeed
  | MiOptionTargetSegmentsPerThread
  | MiOptionGenericCollect
  deriving (Show, Eq, Bounded, Enum)

miOptionToC :: MiOption -> CInt
miOptionToC = fromIntegral . fromEnum

-- -----------------------------------------------------------------------------
-- Core Allocation
-- -----------------------------------------------------------------------------

-- | Allocate @size@ bytes.
-- Returns a unique pointer if successful, @NULL@ otherwise.
foreign import capi unsafe "mimalloc.h mi_malloc"
  mi_malloc :: CSize -> IO (Ptr a)

-- | Allocate zero-initialized @count@ * @size@ bytes.
foreign import capi unsafe "mimalloc.h mi_calloc"
  mi_calloc :: CSize -> CSize -> IO (Ptr a)

-- | Re-allocate memory to @newsize@ bytes.
foreign import capi unsafe "mimalloc.h mi_realloc"
  mi_realloc :: Ptr a -> CSize -> IO (Ptr b)

-- | Try to re-allocate memory in-place to @newsize@ bytes.
-- Returns the same pointer if successful, @NULL@ otherwise.
foreign import capi unsafe "mimalloc.h mi_expand"
  mi_expand :: Ptr a -> CSize -> IO (Ptr a)

-- | Free previously allocated memory.
foreign import capi unsafe "mimalloc.h mi_free"
  mi_free :: Ptr a -> IO ()

-- | Allocate and duplicate a C string.
foreign import capi unsafe "mimalloc.h mi_strdup"
  mi_strdup :: CString -> IO CString

-- | Allocate and duplicate at most @n@ bytes of a C string.
foreign import capi unsafe "mimalloc.h mi_strndup"
  mi_strndup :: CString -> CSize -> IO CString

-- | Allocate and resolve a file path.
foreign import capi unsafe "mimalloc.h mi_realpath"
  mi_realpath :: CString -> CString -> IO CString

-- -----------------------------------------------------------------------------
-- Small Allocations
-- -----------------------------------------------------------------------------

-- | Allocate @size@ bytes (optimized for small sizes).
foreign import capi unsafe "mimalloc.h mi_malloc_small"
  mi_malloc_small :: CSize -> IO (Ptr a)

-- | Allocate zero-initialized @size@ bytes (optimized for small sizes).
foreign import capi unsafe "mimalloc.h mi_zalloc_small"
  mi_zalloc_small :: CSize -> IO (Ptr a)

-- | Allocate zero-initialized @size@ bytes.
foreign import capi unsafe "mimalloc.h mi_zalloc"
  mi_zalloc :: CSize -> IO (Ptr a)

-- -----------------------------------------------------------------------------
-- Counting Allocations
-- -----------------------------------------------------------------------------

-- | Allocate @count@ * @size@ bytes.
foreign import capi unsafe "mimalloc.h mi_mallocn"
  mi_mallocn :: CSize -> CSize -> IO (Ptr a)

-- | Re-allocate memory to @count@ * @size@ bytes.
foreign import capi unsafe "mimalloc.h mi_reallocn"
  mi_reallocn :: Ptr a -> CSize -> CSize -> IO (Ptr b)

-- | Re-allocate memory to @newsize@ bytes, or free on failure.
foreign import capi unsafe "mimalloc.h mi_reallocf"
  mi_reallocf :: Ptr a -> CSize -> IO (Ptr a)

-- -----------------------------------------------------------------------------
-- Size Queries
-- -----------------------------------------------------------------------------

-- | Return the available bytes in a memory block.
foreign import capi unsafe "mimalloc.h mi_usable_size"
  mi_usable_size :: Ptr a -> IO CSize

-- | Return a good allocation size for @size@ (typically larger).
foreign import capi unsafe "mimalloc.h mi_good_size"
  mi_good_size :: CSize -> CSize

-- -----------------------------------------------------------------------------
-- Aligned Allocation
-- -----------------------------------------------------------------------------

-- | Allocate @size@ bytes aligned to @alignment@.
foreign import capi unsafe "mimalloc.h mi_malloc_aligned"
  mi_malloc_aligned :: CSize -> CSize -> IO (Ptr a)

-- | Allocate @size@ bytes aligned to @alignment@ at @offset@.
foreign import capi unsafe "mimalloc.h mi_malloc_aligned_at"
  mi_malloc_aligned_at :: CSize -> CSize -> CSize -> IO (Ptr a)

-- | Allocate zero-initialized @size@ bytes aligned to @alignment@.
foreign import capi unsafe "mimalloc.h mi_zalloc_aligned"
  mi_zalloc_aligned :: CSize -> CSize -> IO (Ptr a)

-- | Allocate zero-initialized @size@ bytes aligned to @alignment@ at @offset@.
foreign import capi unsafe "mimalloc.h mi_zalloc_aligned_at"
  mi_zalloc_aligned_at :: CSize -> CSize -> CSize -> IO (Ptr a)

-- | Allocate zero-initialized @count@ * @size@ bytes aligned to @alignment@.
foreign import capi unsafe "mimalloc.h mi_calloc_aligned"
  mi_calloc_aligned :: CSize -> CSize -> CSize -> IO (Ptr a)

-- | Allocate zero-initialized @count@ * @size@ bytes aligned to @alignment@ at @offset@.
foreign import capi unsafe "mimalloc.h mi_calloc_aligned_at"
  mi_calloc_aligned_at :: CSize -> CSize -> CSize -> CSize -> IO (Ptr a)

-- | Re-allocate memory to @newsize@ bytes aligned to @alignment@.
foreign import capi unsafe "mimalloc.h mi_realloc_aligned"
  mi_realloc_aligned :: Ptr a -> CSize -> CSize -> IO (Ptr b)

-- | Re-allocate memory to @newsize@ bytes aligned to @alignment@ at @offset@.
foreign import capi unsafe "mimalloc.h mi_realloc_aligned_at"
  mi_realloc_aligned_at :: Ptr a -> CSize -> CSize -> CSize -> IO (Ptr b)

-- -----------------------------------------------------------------------------
-- Zero-initialized Reallocation
-- -----------------------------------------------------------------------------

-- | Re-allocate zero-initialized memory to @newsize@ bytes.
foreign import capi unsafe "mimalloc.h mi_rezalloc"
  mi_rezalloc :: Ptr a -> CSize -> IO (Ptr a)

-- | Re-allocate zero-initialized memory to @newcount@ * @size@ bytes.
foreign import capi unsafe "mimalloc.h mi_recalloc"
  mi_recalloc :: Ptr a -> CSize -> CSize -> IO (Ptr a)

-- | Re-allocate zero-initialized memory to @newsize@ bytes aligned to @alignment@.
foreign import capi unsafe "mimalloc.h mi_rezalloc_aligned"
  mi_rezalloc_aligned :: Ptr a -> CSize -> CSize -> IO (Ptr a)

-- | Re-allocate zero-initialized memory to @newsize@ bytes aligned to @alignment@ at @offset@.
foreign import capi unsafe "mimalloc.h mi_rezalloc_aligned_at"
  mi_rezalloc_aligned_at :: Ptr a -> CSize -> CSize -> CSize -> IO (Ptr a)

-- | Re-allocate zero-initialized memory to @newcount@ * @size@ bytes aligned to @alignment@.
foreign import capi unsafe "mimalloc.h mi_recalloc_aligned"
  mi_recalloc_aligned :: Ptr a -> CSize -> CSize -> CSize -> IO (Ptr a)

-- | Re-allocate zero-initialized memory to @newcount@ * @size@ bytes aligned to @alignment@ at @offset@.
foreign import capi unsafe "mimalloc.h mi_recalloc_aligned_at"
  mi_recalloc_aligned_at :: Ptr a -> CSize -> CSize -> CSize -> CSize -> IO (Ptr a)

-- -----------------------------------------------------------------------------
-- Heap Management
-- -----------------------------------------------------------------------------

-- | Create a new heap.
--
-- /Important/: A heap can only be used for (re)allocation in the thread that
-- created it! Any allocated blocks can be freed by any other thread though.
foreign import capi unsafe "mimalloc.h mi_heap_new"
  mi_heap_new :: IO (Ptr MiHeap)

-- | Delete a heap (must be done by the same thread that created it).
foreign import capi unsafe "mimalloc.h mi_heap_delete"
  mi_heap_delete :: Ptr MiHeap -> IO ()

-- | Destroy a heap and free all its still-allocated blocks.
foreign import capi unsafe "mimalloc.h mi_heap_destroy"
  mi_heap_destroy :: Ptr MiHeap -> IO ()

-- | Set the default heap for the current thread.
foreign import capi unsafe "mimalloc.h mi_heap_set_default"
  mi_heap_set_default :: Ptr MiHeap -> IO (Ptr MiHeap)

-- | Get the default heap for the current thread.
foreign import capi unsafe "mimalloc.h mi_heap_get_default"
  mi_heap_get_default :: IO (Ptr MiHeap)

-- | Get the backing heap.
foreign import capi unsafe "mimalloc.h mi_heap_get_backing"
  mi_heap_get_backing :: IO (Ptr MiHeap)

-- | Collect and free memory in a heap.
foreign import capi unsafe "mimalloc.h mi_heap_collect"
  mi_heap_collect :: Ptr MiHeap -> CBool -> IO ()

-- -----------------------------------------------------------------------------
-- Heap-specific Allocation
-- -----------------------------------------------------------------------------

foreign import capi unsafe "mimalloc.h mi_heap_malloc"
  mi_heap_malloc :: Ptr MiHeap -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_zalloc"
  mi_heap_zalloc :: Ptr MiHeap -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_calloc"
  mi_heap_calloc :: Ptr MiHeap -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_mallocn"
  mi_heap_mallocn :: Ptr MiHeap -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_malloc_small"
  mi_heap_malloc_small :: Ptr MiHeap -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_realloc"
  mi_heap_realloc :: Ptr MiHeap -> Ptr a -> CSize -> IO (Ptr b)

foreign import capi unsafe "mimalloc.h mi_heap_reallocn"
  mi_heap_reallocn :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> IO (Ptr b)

foreign import capi unsafe "mimalloc.h mi_heap_reallocf"
  mi_heap_reallocf :: Ptr MiHeap -> Ptr a -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_strdup"
  mi_heap_strdup :: Ptr MiHeap -> CString -> IO CString

foreign import capi unsafe "mimalloc.h mi_heap_strndup"
  mi_heap_strndup :: Ptr MiHeap -> CString -> CSize -> IO CString

foreign import capi unsafe "mimalloc.h mi_heap_realpath"
  mi_heap_realpath :: Ptr MiHeap -> CString -> CString -> IO CString

-- -----------------------------------------------------------------------------
-- Heap-specific Aligned Allocation
-- -----------------------------------------------------------------------------

foreign import capi unsafe "mimalloc.h mi_heap_malloc_aligned"
  mi_heap_malloc_aligned :: Ptr MiHeap -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_malloc_aligned_at"
  mi_heap_malloc_aligned_at :: Ptr MiHeap -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_zalloc_aligned"
  mi_heap_zalloc_aligned :: Ptr MiHeap -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_zalloc_aligned_at"
  mi_heap_zalloc_aligned_at :: Ptr MiHeap -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_calloc_aligned"
  mi_heap_calloc_aligned :: Ptr MiHeap -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_calloc_aligned_at"
  mi_heap_calloc_aligned_at :: Ptr MiHeap -> CSize -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_realloc_aligned"
  mi_heap_realloc_aligned :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> IO (Ptr b)

foreign import capi unsafe "mimalloc.h mi_heap_realloc_aligned_at"
  mi_heap_realloc_aligned_at :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> CSize -> IO (Ptr b)

-- -----------------------------------------------------------------------------
-- Heap-specific Zero-initialized Reallocation
-- -----------------------------------------------------------------------------

foreign import capi unsafe "mimalloc.h mi_heap_rezalloc"
  mi_heap_rezalloc :: Ptr MiHeap -> Ptr a -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_recalloc"
  mi_heap_recalloc :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_rezalloc_aligned"
  mi_heap_rezalloc_aligned :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_rezalloc_aligned_at"
  mi_heap_rezalloc_aligned_at :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_recalloc_aligned"
  mi_heap_recalloc_aligned :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import capi unsafe "mimalloc.h mi_heap_recalloc_aligned_at"
  mi_heap_recalloc_aligned_at :: Ptr MiHeap -> Ptr a -> CSize -> CSize -> CSize -> CSize -> IO (Ptr a)

-- -----------------------------------------------------------------------------
-- Heap Analysis
-- -----------------------------------------------------------------------------

-- | Check if a pointer belongs to a specific heap.
foreign import capi unsafe "mimalloc.h mi_heap_contains_block"
  mi_heap_contains_block :: Ptr MiHeap -> Ptr a -> IO CBool

-- | Check if a pointer is owned by a specific heap.
foreign import capi unsafe "mimalloc.h mi_heap_check_owned"
  mi_heap_check_owned :: Ptr MiHeap -> Ptr a -> IO CBool

-- | Check if a pointer is owned by any heap.
foreign import capi unsafe "mimalloc.h mi_check_owned"
  mi_check_owned :: Ptr a -> IO CBool

-- -----------------------------------------------------------------------------
-- Block Visiting
-- -----------------------------------------------------------------------------

-- | Visit all blocks in a heap.
foreign import capi unsafe "mimalloc.h mi_heap_visit_blocks"
  mi_heap_visit_blocks :: Ptr MiHeap -> CBool -> FunPtr MiBlockVisitFun -> Ptr a -> IO CBool

-- -----------------------------------------------------------------------------
-- Statistics and Introspection
-- -----------------------------------------------------------------------------

-- | Eagerly free memory.
foreign import capi unsafe "mimalloc.h mi_collect"
  mi_collect :: CBool -> IO ()

-- | Return the mimalloc version number.
foreign import capi unsafe "mimalloc.h mi_version"
  mi_version :: IO CInt

-- | Reset statistics.
foreign import capi unsafe "mimalloc.h mi_stats_reset"
  mi_stats_reset :: IO ()

-- | Merge thread-local statistics with the main statistics.
foreign import capi unsafe "mimalloc.h mi_stats_merge"
  mi_stats_merge :: IO ()

-- | Print statistics.
foreign import capi unsafe "mimalloc.h mi_stats_print"
  mi_stats_print :: Ptr () -> IO ()

-- | Get process information.
foreign import capi unsafe "mimalloc.h mi_process_info"
  mi_process_info
    :: Ptr CSize  -- ^ elapsed_msecs
    -> Ptr CSize  -- ^ user_msecs
    -> Ptr CSize  -- ^ system_msecs
    -> Ptr CSize  -- ^ current_rss
    -> Ptr CSize  -- ^ peak_rss
    -> Ptr CSize  -- ^ current_commit
    -> Ptr CSize  -- ^ peak_commit
    -> Ptr CSize  -- ^ page_faults
    -> IO ()

-- -----------------------------------------------------------------------------
-- Thread Management
-- -----------------------------------------------------------------------------

-- | Initialize the process (usually called automatically).
foreign import capi unsafe "mimalloc.h mi_process_init"
  mi_process_init :: IO ()

-- | Done with the process (usually called automatically).
foreign import capi unsafe "mimalloc.h mi_process_done"
  mi_process_done :: IO ()

-- | Initialize the current thread (usually called automatically).
foreign import capi unsafe "mimalloc.h mi_thread_init"
  mi_thread_init :: IO ()

-- | Done with the current thread (usually called automatically).
foreign import capi unsafe "mimalloc.h mi_thread_done"
  mi_thread_done :: IO ()

-- -----------------------------------------------------------------------------
-- Options
-- -----------------------------------------------------------------------------

-- | Check if an option is enabled.
foreign import capi unsafe "mimalloc.h mi_option_is_enabled"
  mi_option_is_enabled_raw :: CInt -> IO CBool

mi_option_is_enabled :: MiOption -> IO Bool
mi_option_is_enabled opt = (/= 0) <$> mi_option_is_enabled_raw (miOptionToC opt)

-- | Enable an option.
foreign import capi unsafe "mimalloc.h mi_option_enable"
  mi_option_enable_raw :: CInt -> IO ()

mi_option_enable :: MiOption -> IO ()
mi_option_enable = mi_option_enable_raw . miOptionToC

-- | Disable an option.
foreign import capi unsafe "mimalloc.h mi_option_disable"
  mi_option_disable_raw :: CInt -> IO ()

mi_option_disable :: MiOption -> IO ()
mi_option_disable = mi_option_disable_raw . miOptionToC

-- | Set an option to enabled or disabled.
foreign import capi unsafe "mimalloc.h mi_option_set_enabled"
  mi_option_set_enabled_raw :: CInt -> CBool -> IO ()

mi_option_set_enabled :: MiOption -> Bool -> IO ()
mi_option_set_enabled opt enabled =
  mi_option_set_enabled_raw (miOptionToC opt) (if enabled then 1 else 0)

-- | Set an option's default to enabled or disabled.
foreign import capi unsafe "mimalloc.h mi_option_set_enabled_default"
  mi_option_set_enabled_default_raw :: CInt -> CBool -> IO ()

mi_option_set_enabled_default :: MiOption -> Bool -> IO ()
mi_option_set_enabled_default opt enabled =
  mi_option_set_enabled_default_raw (miOptionToC opt) (if enabled then 1 else 0)

-- | Get an option value.
foreign import capi unsafe "mimalloc.h mi_option_get"
  mi_option_get_raw :: CInt -> IO CLong

mi_option_get :: MiOption -> IO CLong
mi_option_get = mi_option_get_raw . miOptionToC

-- | Get an option value clamped to a range.
foreign import capi unsafe "mimalloc.h mi_option_get_clamp"
  mi_option_get_clamp_raw :: CInt -> CLong -> CLong -> IO CLong

mi_option_get_clamp :: MiOption -> CLong -> CLong -> IO CLong
mi_option_get_clamp opt minVal maxVal =
  mi_option_get_clamp_raw (miOptionToC opt) minVal maxVal

-- | Get an option value as a size.
foreign import capi unsafe "mimalloc.h mi_option_get_size"
  mi_option_get_size_raw :: CInt -> IO CSize

mi_option_get_size :: MiOption -> IO CSize
mi_option_get_size = mi_option_get_size_raw . miOptionToC

-- | Set an option value.
foreign import capi unsafe "mimalloc.h mi_option_set"
  mi_option_set_raw :: CInt -> CLong -> IO ()

mi_option_set :: MiOption -> CLong -> IO ()
mi_option_set opt val = mi_option_set_raw (miOptionToC opt) val

-- | Set an option's default value.
foreign import capi unsafe "mimalloc.h mi_option_set_default"
  mi_option_set_default_raw :: CInt -> CLong -> IO ()

mi_option_set_default :: MiOption -> CLong -> IO ()
mi_option_set_default opt val = mi_option_set_default_raw (miOptionToC opt) val

-- -----------------------------------------------------------------------------
-- Posix-style Functions
-- -----------------------------------------------------------------------------

-- | POSIX-style aligned allocation.
foreign import capi unsafe "mimalloc.h mi_posix_memalign"
  mi_posix_memalign :: Ptr (Ptr a) -> CSize -> CSize -> IO CInt

-- | Allocate aligned memory.
foreign import capi unsafe "mimalloc.h mi_memalign"
  mi_memalign :: CSize -> CSize -> IO (Ptr a)

-- | Allocate page-aligned memory.
foreign import capi unsafe "mimalloc.h mi_valloc"
  mi_valloc :: CSize -> IO (Ptr a)

-- | Allocate page-aligned memory (rounded up to page size).
foreign import capi unsafe "mimalloc.h mi_pvalloc"
  mi_pvalloc :: CSize -> IO (Ptr a)

-- | C11 aligned allocation.
foreign import capi unsafe "mimalloc.h mi_aligned_alloc"
  mi_aligned_alloc :: CSize -> CSize -> IO (Ptr a)

-- | Reallocate array.
foreign import capi unsafe "mimalloc.h mi_reallocarray"
  mi_reallocarray :: Ptr a -> CSize -> CSize -> IO (Ptr b)

-- | Checked free (checks if pointer is in mimalloc heap).
foreign import capi unsafe "mimalloc.h mi_cfree"
  mi_cfree :: Ptr a -> IO ()

-- | Get the usable size of a memory block.
foreign import capi unsafe "mimalloc.h mi_malloc_size"
  mi_malloc_size :: Ptr a -> IO CSize

-- | Get the usable size of a memory block.
foreign import capi unsafe "mimalloc.h mi_malloc_usable_size"
  mi_malloc_usable_size :: Ptr a -> IO CSize

-- | Free memory with a known size hint.
foreign import capi unsafe "mimalloc.h mi_free_size"
  mi_free_size :: Ptr a -> CSize -> IO ()

-- | Free aligned memory with a known size hint.
foreign import capi unsafe "mimalloc.h mi_free_size_aligned"
  mi_free_size_aligned :: Ptr a -> CSize -> CSize -> IO ()

-- | Free aligned memory with a known alignment.
foreign import capi unsafe "mimalloc.h mi_free_aligned"
  mi_free_aligned :: Ptr a -> CSize -> IO ()

-- -----------------------------------------------------------------------------
-- Advanced Features
-- -----------------------------------------------------------------------------

-- | Check if a pointer is in a mimalloc heap region.
foreign import capi unsafe "mimalloc.h mi_is_in_heap_region"
  mi_is_in_heap_region :: Ptr a -> IO CBool

-- | Check if malloc/free have been redirected to mimalloc.
foreign import capi unsafe "mimalloc.h mi_is_redirected"
  mi_is_redirected :: IO CBool

-- | Reserve huge OS pages interleaved across NUMA nodes.
foreign import capi unsafe "mimalloc.h mi_reserve_huge_os_pages_interleave"
  mi_reserve_huge_os_pages_interleave :: CSize -> CSize -> CSize -> IO CInt

-- | Reserve huge OS pages at a specific NUMA node.
foreign import capi unsafe "mimalloc.h mi_reserve_huge_os_pages_at"
  mi_reserve_huge_os_pages_at :: CSize -> CInt -> CSize -> IO CInt

-- | Reserve OS memory.
foreign import capi unsafe "mimalloc.h mi_reserve_os_memory"
  mi_reserve_os_memory :: CSize -> CBool -> CBool -> IO CInt

-- | Let mimalloc manage externally allocated OS memory.
foreign import capi unsafe "mimalloc.h mi_manage_os_memory"
  mi_manage_os_memory :: Ptr () -> CSize -> CBool -> CBool -> CBool -> CInt -> IO CBool

-- -----------------------------------------------------------------------------
-- Arena Management
-- -----------------------------------------------------------------------------

-- | Get the memory area associated with an arena.
foreign import capi unsafe "mimalloc.h mi_arena_area"
  mi_arena_area :: MiArenaId -> Ptr CSize -> IO (Ptr ())

-- | Reserve huge OS pages at a specific NUMA node, with arena options.
foreign import capi unsafe "mimalloc.h mi_reserve_huge_os_pages_at_ex"
  mi_reserve_huge_os_pages_at_ex :: CSize -> CInt -> CSize -> CBool -> Ptr MiArenaId -> IO CInt

-- | Reserve OS memory with arena options.
foreign import capi unsafe "mimalloc.h mi_reserve_os_memory_ex"
  mi_reserve_os_memory_ex :: CSize -> CBool -> CBool -> CBool -> Ptr MiArenaId -> IO CInt

-- | Let mimalloc manage externally allocated OS memory with arena options.
foreign import capi unsafe "mimalloc.h mi_manage_os_memory_ex"
  mi_manage_os_memory_ex :: Ptr () -> CSize -> CBool -> CBool -> CBool -> CInt -> CBool -> Ptr MiArenaId -> IO CBool

-- | Create a heap that only allocates in a specific arena.
foreign import capi unsafe "mimalloc.h mi_heap_new_in_arena"
  mi_heap_new_in_arena :: MiArenaId -> IO (Ptr MiHeap)

-- | Create a heap with extended options.
foreign import capi unsafe "mimalloc.h mi_heap_new_ex"
  mi_heap_new_ex :: CInt -> CBool -> MiArenaId -> IO (Ptr MiHeap)

-- -----------------------------------------------------------------------------
-- Experimental: Sub-processes
-- -----------------------------------------------------------------------------

-- | Get the main sub-process identifier.
foreign import capi unsafe "mimalloc.h mi_subproc_main"
  mi_subproc_main :: IO MiSubprocId

-- | Create a new sub-process.
foreign import capi unsafe "mimalloc.h mi_subproc_new"
  mi_subproc_new :: IO MiSubprocId

-- | Delete a sub-process.
foreign import capi unsafe "mimalloc.h mi_subproc_delete"
  mi_subproc_delete :: MiSubprocId -> IO ()

-- | Add the current thread to a sub-process.
foreign import capi unsafe "mimalloc.h mi_subproc_add_current_thread"
  mi_subproc_add_current_thread :: MiSubprocId -> IO ()

