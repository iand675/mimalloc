{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-|
Module      : Mimalloc.Heap
Description : Scoped heap management for mimalloc
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

This module provides scoped heap management using RankNTypes to ensure
heap-allocated memory cannot escape its scope. This provides better safety
for working with custom mimalloc heaps.

/⚠️  EXPERIMENTAL: This library uses a modified version of mimalloc adapted for GHC./
/Custom heaps in this implementation are capability-local, not thread-local./
/See "Mimalloc" module documentation for details./

== Thread Safety and Capability-Based Heaps

In this library, custom heaps created with 'mi_heap_new' are /capability-local/
rather than thread-local. This means:

=== Capability-Local Behavior

* A heap created on capability N can only be used for allocation on capability N
* Multiple green threads on the same capability can share the heap
* Thread migration between capabilities means you cannot use the heap on the new capability
* Freed blocks can be freed from any capability (thread-safe)

=== Usage Recommendations

* For simple allocations, use the default heap from "Mimalloc" (works on all capabilities)
* For custom heaps: create one heap per capability if needed
* 'forkIO' is safe but be aware the thread may migrate to different capabilities
* If you need heap-specific allocation with 'forkIO', use 'runOnCapability' to ensure
  execution on the correct capability
* Custom heaps are most useful for:
  - Isolating allocation pools by object type
  - Custom collection/destruction strategies
  - Arena-style allocation patterns

=== Migration from Thread-Local Model

If migrating code that assumed thread-local heaps:

* Replace 'forkOS' with 'forkIO' for better performance
* Use capability-aware heap management from "Mimalloc.Capability"
* Consider whether you actually need custom heaps (default heap may suffice)

== Basic Usage

>>> withHeap $ \heap -> do
...   ptr <- heapMallocForeignPtr heap :: IO (ForeignPtr Int)
...   withForeignPtr ptr $ \p -> poke p 42

>>> withHeap $ \heap -> do
...   withHeapMalloc heap $ \ptr -> do
...     poke ptr (123 :: Int)
...     peek ptr
123

-}
module Mimalloc.Heap
  ( -- * Scoped Heap Management
    Heap(..)
  , withHeap
  , withHeapEx
  , withHeapInArena

    -- * ForeignPtr Allocation
  , heapMallocForeignPtr
  , heapMallocForeignPtrBytes
  , heapMallocForeignPtrArray
  , heapMallocForeignPtrAligned
  , heapCallocForeignPtr
  , heapCallocForeignPtrArray
  , heapZallocForeignPtrBytes

    -- * Bracketed Allocation
  , withHeapMalloc
  , withHeapMallocBytes
  , withHeapMallocArray
  , withHeapMallocAligned
  , withHeapCalloc
  , withHeapCallocArray
  , withHeapZallocBytes

    -- * Heap Operations
  , heapCollect
  , heapContainsBlock
  , heapCheckOwned

  ) where

import Mimalloc.Primitive
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Control.Exception (bracket)
import Data.Word
import Data.Kind (Type)

foreign import ccall "&mi_free"
  p_mi_free :: FinalizerPtr a

-- | A phantom-typed heap handle that prevents allocated memory from escaping its scope.
-- The phantom type parameter ensures that allocations from different heap scopes
-- cannot be mixed.
newtype Heap (s :: Type) = Heap (Ptr MiHeap)
type role Heap nominal

-- -----------------------------------------------------------------------------
-- Scoped Heap Management
-- -----------------------------------------------------------------------------

-- | Create a new heap and run a computation with it.
-- The heap is automatically deleted when the computation completes.
--
-- /Important/: A heap can only be used for (re)allocation in the thread that
-- created it! Any allocated blocks can be freed by any other thread though.
-- For multi-threaded scenarios, create a separate heap per OS thread:
-- @forkOS $ withHeap $ \\heap -> ...@
--
-- >>> withHeap $ \heap -> heapMallocForeignPtr heap :: IO (ForeignPtr Int)
withHeap :: (forall s. Heap s -> IO a) -> IO a
withHeap action =
  bracket mi_heap_new mi_heap_delete $ \heapPtr ->
    if heapPtr == nullPtr
      then error "mimalloc: heap creation failed"
      else action (Heap heapPtr)
{-# INLINE withHeap #-}

-- | Create a new heap with extended options.
--
-- Parameters:
--   [@heap_tag@]: Tag for the heap (for identification)
--   [@allow_destroy@]: Whether to allow destroying the heap (vs just deleting)
--   [@arena_id@]: Arena to allocate from (-1 for default)
withHeapEx :: Int -> Bool -> MiArenaId -> (forall s. Heap s -> IO a) -> IO a
withHeapEx heapTag allowDestroy arenaId action =
  let tag = fromIntegral heapTag
      allow = if allowDestroy then 1 else 0
  in bracket (mi_heap_new_ex tag allow arenaId) mi_heap_delete $ \heapPtr ->
       if heapPtr == nullPtr
         then error "mimalloc: heap creation failed"
         else action (Heap heapPtr)
{-# INLINE withHeapEx #-}

-- | Create a new heap that only allocates from a specific arena.
withHeapInArena :: MiArenaId -> (forall s. Heap s -> IO a) -> IO a
withHeapInArena arenaId action =
  bracket (mi_heap_new_in_arena arenaId) mi_heap_delete $ \heapPtr ->
    if heapPtr == nullPtr
      then error "mimalloc: heap creation failed"
      else action (Heap heapPtr)
{-# INLINE withHeapInArena #-}

-- -----------------------------------------------------------------------------
-- ForeignPtr Allocation
-- -----------------------------------------------------------------------------

-- | Allocate memory for a Storable value from a specific heap.
heapMallocForeignPtr :: forall s a. Storable a => Heap s -> IO (ForeignPtr a)
heapMallocForeignPtr (Heap heap) = do
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_malloc heap size
  if ptr == nullPtr
    then error "mimalloc: heap allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE heapMallocForeignPtr #-}

-- | Allocate a specified number of bytes from a specific heap.
heapMallocForeignPtrBytes :: Heap s -> Int -> IO (ForeignPtr Word8)
heapMallocForeignPtrBytes (Heap heap) bytes = do
  let size = fromIntegral bytes
  ptr <- mi_heap_malloc heap size
  if ptr == nullPtr
    then error "mimalloc: heap allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE heapMallocForeignPtrBytes #-}

-- | Allocate memory for an array from a specific heap.
heapMallocForeignPtrArray :: forall s a. Storable a => Heap s -> Int -> IO (ForeignPtr a)
heapMallocForeignPtrArray (Heap heap) n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_mallocn heap count size
  if ptr == nullPtr
    then error "mimalloc: heap allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE heapMallocForeignPtrArray #-}

-- | Allocate aligned memory from a specific heap.
heapMallocForeignPtrAligned :: forall s a. Storable a => Heap s -> Int -> IO (ForeignPtr a)
heapMallocForeignPtrAligned (Heap heap) align = do
  let size = fromIntegral $ sizeOf (undefined :: a)
  let align' = fromIntegral align
  ptr <- mi_heap_malloc_aligned heap size align'
  if ptr == nullPtr
    then error "mimalloc: heap aligned allocation failed"
    else newForeignPtr p_mi_free ptr  -- mi_free works for aligned allocations too
{-# INLINE heapMallocForeignPtrAligned #-}

-- | Allocate zero-initialized memory for a Storable value from a specific heap.
heapCallocForeignPtr :: forall s a. Storable a => Heap s -> IO (ForeignPtr a)
heapCallocForeignPtr (Heap heap) = do
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_zalloc heap size
  if ptr == nullPtr
    then error "mimalloc: heap zero allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE heapCallocForeignPtr #-}

-- | Allocate zero-initialized memory for an array from a specific heap.
heapCallocForeignPtrArray :: forall s a. Storable a => Heap s -> Int -> IO (ForeignPtr a)
heapCallocForeignPtrArray (Heap heap) n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_calloc heap count size
  if ptr == nullPtr
    then error "mimalloc: heap zero allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE heapCallocForeignPtrArray #-}

-- | Allocate zero-initialized bytes from a specific heap.
heapZallocForeignPtrBytes :: Heap s -> Int -> IO (ForeignPtr Word8)
heapZallocForeignPtrBytes (Heap heap) bytes = do
  let size = fromIntegral bytes
  ptr <- mi_heap_zalloc heap size
  if ptr == nullPtr
    then error "mimalloc: heap zero allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE heapZallocForeignPtrBytes #-}

-- -----------------------------------------------------------------------------
-- Bracketed Allocation
-- -----------------------------------------------------------------------------

-- | Allocate memory from a heap for the duration of a computation.
withHeapMalloc :: forall s a b. Storable a => Heap s -> (Ptr a -> IO b) -> IO b
withHeapMalloc (Heap heap) action =
  let size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_heap_malloc heap size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap allocation failed"
         else action ptr
{-# INLINE withHeapMalloc #-}

-- | Allocate bytes from a heap for the duration of a computation.
withHeapMallocBytes :: Heap s -> Int -> (Ptr Word8 -> IO b) -> IO b
withHeapMallocBytes (Heap heap) bytes action =
  let size = fromIntegral bytes
  in bracket (mi_heap_malloc heap size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap allocation failed"
         else action ptr
{-# INLINE withHeapMallocBytes #-}

-- | Allocate an array from a heap for the duration of a computation.
withHeapMallocArray :: forall s a b. Storable a => Heap s -> Int -> (Ptr a -> IO b) -> IO b
withHeapMallocArray (Heap heap) n action =
  let count = fromIntegral n
      size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_heap_mallocn heap count size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap allocation failed"
         else action ptr
{-# INLINE withHeapMallocArray #-}

-- | Allocate aligned memory from a heap for the duration of a computation.
withHeapMallocAligned :: forall s a b. Storable a => Heap s -> Int -> (Ptr a -> IO b) -> IO b
withHeapMallocAligned (Heap heap) align action =
  let size = fromIntegral $ sizeOf (undefined :: a)
      align' = fromIntegral align
  in bracket (mi_heap_malloc_aligned heap size align') (`mi_free_aligned` align') $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap aligned allocation failed"
         else action ptr
{-# INLINE withHeapMallocAligned #-}

-- | Allocate zero-initialized memory from a heap for the duration of a computation.
withHeapCalloc :: forall s a b. Storable a => Heap s -> (Ptr a -> IO b) -> IO b
withHeapCalloc (Heap heap) action =
  let size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_heap_zalloc heap size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap zero allocation failed"
         else action ptr
{-# INLINE withHeapCalloc #-}

-- | Allocate zero-initialized array from a heap for the duration of a computation.
withHeapCallocArray :: forall s a b. Storable a => Heap s -> Int -> (Ptr a -> IO b) -> IO b
withHeapCallocArray (Heap heap) n action =
  let count = fromIntegral n
      size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_heap_calloc heap count size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap zero allocation failed"
         else action ptr
{-# INLINE withHeapCallocArray #-}

-- | Allocate zero-initialized bytes from a heap for the duration of a computation.
withHeapZallocBytes :: Heap s -> Int -> (Ptr Word8 -> IO b) -> IO b
withHeapZallocBytes (Heap heap) bytes action =
  let size = fromIntegral bytes
  in bracket (mi_heap_zalloc heap size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: heap zero allocation failed"
         else action ptr
{-# INLINE withHeapZallocBytes #-}

-- -----------------------------------------------------------------------------
-- Heap Operations
-- -----------------------------------------------------------------------------

-- | Eagerly collect and free memory in a heap.
heapCollect :: Heap s -> Bool -> IO ()
heapCollect (Heap heap) force =
  mi_heap_collect heap (if force then 1 else 0)
{-# INLINE heapCollect #-}

-- | Check if a pointer belongs to a specific heap.
heapContainsBlock :: Heap s -> Ptr a -> IO Bool
heapContainsBlock (Heap heap) ptr =
  (/= 0) <$> mi_heap_contains_block heap ptr
{-# INLINE heapContainsBlock #-}

-- | Check if a pointer is owned by a specific heap.
heapCheckOwned :: Heap s -> Ptr a -> IO Bool
heapCheckOwned (Heap heap) ptr =
  (/= 0) <$> mi_heap_check_owned heap ptr
{-# INLINE heapCheckOwned #-}


