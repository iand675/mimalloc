{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Mimalloc
Description : High-level interface to mimalloc memory allocator
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

This module provides a high-level, type-safe interface to the mimalloc memory allocator.
It offers ForeignPtr-based automatic memory management, alloca-style bracketing, and
integration with common Haskell data structures.

== Thread Safety

The functions in this module use mimalloc's /default heap/, which is thread-local.
However, in most cases, /you do NOT need to use forkOS/ with these functions:

* Allocations and deallocations from the default heap are thread-safe
* The default heap is automatically managed per OS thread
* 'forkIO' green threads that happen to run on the same OS thread will share
  that thread's default heap safely

/You only need 'forkOS' when/:

* Using custom heaps from "Mimalloc.Heap" (heaps are bound to specific OS threads)
* Calling thread-specific initialization ('mi_thread_init', 'mi_thread_done')
* You need guaranteed isolation between Haskell threads' allocations

For most use cases, the functions in this module work correctly with 'forkIO'.

== Basic Usage

>>> ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
>>> withForeignPtr ptr $ \p -> poke p 42

>>> withMalloc $ \p -> do
...   poke p (123 :: Int)
...   peek p
123

-}
module Mimalloc
  ( -- * ForeignPtr Allocation
    -- $foreignptr
    mallocForeignPtr
  , mallocForeignPtrBytes
  , mallocForeignPtrArray
  , mallocForeignPtrAligned
  , mallocForeignPtrAlignedBytes

    -- * Bracketed Allocation
    -- $bracketed
  , withMalloc
  , withMallocBytes
  , withMallocArray
  , withMallocAligned
  , withMallocAlignedBytes

    -- * Zero-initialized Allocation
  , callocForeignPtr
  , callocForeignPtrArray
  , zallocForeignPtrBytes
  , withCalloc
  , withCallocArray
  , withZallocBytes

    -- * Size Queries
  , goodSize
  , usableSize

    -- * Re-exports
  , module Mimalloc.Primitive
  ) where

import Mimalloc.Primitive
import Foreign.ForeignPtr hiding (mallocForeignPtr, mallocForeignPtrBytes, mallocForeignPtrArray)
import Foreign.Ptr
import Foreign.Storable
import Control.Exception (bracket)
import Data.Word

-- $foreignptr
-- ForeignPtr-based allocation provides automatic memory management through
-- garbage collection. The memory is allocated using mimalloc and will be
-- automatically freed when the ForeignPtr is no longer reachable.

-- $bracketed
-- Bracketed allocation functions follow the @alloca@ style, allocating
-- memory for the duration of a computation and automatically freeing it
-- when the computation completes (even in the presence of exceptions).

-- -----------------------------------------------------------------------------
-- Foreign Imports for Finalizers
-- -----------------------------------------------------------------------------

foreign import ccall "&mi_free"
  p_mi_free :: FinalizerPtr a

-- -----------------------------------------------------------------------------
-- ForeignPtr Allocation
-- -----------------------------------------------------------------------------

-- | Allocate memory for a single Storable value using mimalloc.
-- The memory is managed by a ForeignPtr and will be automatically freed.
--
-- >>> ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
-- >>> withForeignPtr ptr $ \p -> poke p 42
mallocForeignPtr :: forall a. Storable a => IO (ForeignPtr a)
mallocForeignPtr = do
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_malloc size
  if ptr == nullPtr
    then error "mimalloc: allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE mallocForeignPtr #-}

-- | Allocate a specified number of bytes using mimalloc.
-- Returns a ForeignPtr to Word8.
--
-- >>> ptr <- mallocForeignPtrBytes 1024
mallocForeignPtrBytes :: Int -> IO (ForeignPtr Word8)
mallocForeignPtrBytes bytes = do
  let size = fromIntegral bytes
  ptr <- mi_malloc size
  if ptr == nullPtr
    then error "mimalloc: allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE mallocForeignPtrBytes #-}

-- | Allocate memory for an array of Storable values using mimalloc.
--
-- >>> ptr <- mallocForeignPtrArray 10 :: IO (ForeignPtr Int)
mallocForeignPtrArray :: forall a. Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray n = do
  let size = fromIntegral $ n * sizeOf (undefined :: a)
  ptr <- mi_malloc size
  if ptr == nullPtr
    then error "mimalloc: allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE mallocForeignPtrArray #-}

-- | Allocate aligned memory for a Storable value using mimalloc.
-- The alignment must be a power of two.
--
-- >>> ptr <- mallocForeignPtrAligned 64 :: IO (ForeignPtr Int)
mallocForeignPtrAligned :: forall a. Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrAligned align = do
  let size = fromIntegral $ sizeOf (undefined :: a)
  let align' = fromIntegral align
  ptr <- mi_malloc_aligned size align'
  if ptr == nullPtr
    then error "mimalloc: aligned allocation failed"
    else newForeignPtr p_mi_free ptr  -- mi_free works for aligned allocations too
{-# INLINE mallocForeignPtrAligned #-}

-- | Allocate a specified number of bytes with alignment using mimalloc.
--
-- >>> ptr <- mallocForeignPtrAlignedBytes 1024 64
mallocForeignPtrAlignedBytes :: Int -> Int -> IO (ForeignPtr Word8)
mallocForeignPtrAlignedBytes bytes align = do
  let size = fromIntegral bytes
  let align' = fromIntegral align
  ptr <- mi_malloc_aligned size align'
  if ptr == nullPtr
    then error "mimalloc: aligned allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE mallocForeignPtrAlignedBytes #-}

-- -----------------------------------------------------------------------------
-- Zero-initialized ForeignPtr Allocation
-- -----------------------------------------------------------------------------

-- | Allocate zero-initialized memory for a single Storable value using mimalloc.
--
-- >>> ptr <- callocForeignPtr :: IO (ForeignPtr Int)
callocForeignPtr :: forall a. Storable a => IO (ForeignPtr a)
callocForeignPtr = do
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_zalloc size
  if ptr == nullPtr
    then error "mimalloc: zero allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE callocForeignPtr #-}

-- | Allocate zero-initialized memory for an array of Storable values using mimalloc.
--
-- >>> ptr <- callocForeignPtrArray 10 :: IO (ForeignPtr Int)
callocForeignPtrArray :: forall a. Storable a => Int -> IO (ForeignPtr a)
callocForeignPtrArray n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_calloc count size
  if ptr == nullPtr
    then error "mimalloc: zero allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE callocForeignPtrArray #-}

-- | Allocate zero-initialized bytes using mimalloc.
--
-- >>> ptr <- zallocForeignPtrBytes 1024
zallocForeignPtrBytes :: Int -> IO (ForeignPtr Word8)
zallocForeignPtrBytes bytes = do
  let size = fromIntegral bytes
  ptr <- mi_zalloc size
  if ptr == nullPtr
    then error "mimalloc: zero allocation failed"
    else newForeignPtr p_mi_free ptr
{-# INLINE zallocForeignPtrBytes #-}

-- -----------------------------------------------------------------------------
-- Bracketed Allocation
-- -----------------------------------------------------------------------------

-- | Allocate memory for a Storable value for the duration of a computation.
-- The memory is automatically freed when the computation completes.
--
-- >>> withMalloc $ \ptr -> poke ptr (42 :: Int) >> peek ptr
-- 42
withMalloc :: forall a b. Storable a => (Ptr a -> IO b) -> IO b
withMalloc action =
  let size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_malloc size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: allocation failed"
         else action ptr
{-# INLINE withMalloc #-}

-- | Allocate a specified number of bytes for the duration of a computation.
--
-- >>> withMallocBytes 1024 $ \ptr -> ...
withMallocBytes :: Int -> (Ptr Word8 -> IO b) -> IO b
withMallocBytes bytes action =
  let size = fromIntegral bytes
  in bracket (mi_malloc size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: allocation failed"
         else action ptr
{-# INLINE withMallocBytes #-}

-- | Allocate memory for an array of Storable values for the duration of a computation.
--
-- >>> withMallocArray 10 $ \(ptr :: Ptr Int) -> ...
withMallocArray :: forall a b. Storable a => Int -> (Ptr a -> IO b) -> IO b
withMallocArray n action =
  let size = fromIntegral $ n * sizeOf (undefined :: a)
  in bracket (mi_malloc size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: allocation failed"
         else action ptr
{-# INLINE withMallocArray #-}

-- | Allocate aligned memory for a Storable value for the duration of a computation.
--
-- >>> withMallocAligned 64 $ \(ptr :: Ptr Int) -> ...
withMallocAligned :: forall a b. Storable a => Int -> (Ptr a -> IO b) -> IO b
withMallocAligned align action =
  let size = fromIntegral $ sizeOf (undefined :: a)
      align' = fromIntegral align
  in bracket (mi_malloc_aligned size align') (`mi_free_aligned` align') $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: aligned allocation failed"
         else action ptr
{-# INLINE withMallocAligned #-}

-- | Allocate aligned bytes for the duration of a computation.
--
-- >>> withMallocAlignedBytes 1024 64 $ \ptr -> ...
withMallocAlignedBytes :: Int -> Int -> (Ptr Word8 -> IO b) -> IO b
withMallocAlignedBytes bytes align action =
  let size = fromIntegral bytes
      align' = fromIntegral align
  in bracket (mi_malloc_aligned size align') (`mi_free_aligned` align') $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: aligned allocation failed"
         else action ptr
{-# INLINE withMallocAlignedBytes #-}

-- -----------------------------------------------------------------------------
-- Zero-initialized Bracketed Allocation
-- -----------------------------------------------------------------------------

-- | Allocate zero-initialized memory for a Storable value for the duration of a computation.
--
-- >>> withCalloc $ \(ptr :: Ptr Int) -> peek ptr
-- 0
withCalloc :: forall a b. Storable a => (Ptr a -> IO b) -> IO b
withCalloc action =
  let size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_zalloc size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: zero allocation failed"
         else action ptr
{-# INLINE withCalloc #-}

-- | Allocate zero-initialized memory for an array for the duration of a computation.
--
-- >>> withCallocArray 10 $ \(ptr :: Ptr Int) -> peekElemOff ptr 0
-- 0
withCallocArray :: forall a b. Storable a => Int -> (Ptr a -> IO b) -> IO b
withCallocArray n action =
  let count = fromIntegral n
      size = fromIntegral $ sizeOf (undefined :: a)
  in bracket (mi_calloc count size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: zero allocation failed"
         else action ptr
{-# INLINE withCallocArray #-}

-- | Allocate zero-initialized bytes for the duration of a computation.
--
-- >>> withZallocBytes 1024 $ \ptr -> ...
withZallocBytes :: Int -> (Ptr Word8 -> IO b) -> IO b
withZallocBytes bytes action =
  let size = fromIntegral bytes
  in bracket (mi_zalloc size) mi_free $ \ptr ->
       if ptr == nullPtr
         then error "mimalloc: zero allocation failed"
         else action ptr
{-# INLINE withZallocBytes #-}

-- -----------------------------------------------------------------------------
-- Size Queries
-- -----------------------------------------------------------------------------

-- | Return a good allocation size for the given size.
-- Mimalloc typically rounds up to internal size classes for efficiency.
--
-- >>> goodSize 100
goodSize :: Int -> Int
goodSize = fromIntegral . mi_good_size . fromIntegral
{-# INLINE goodSize #-}

-- | Return the usable size of an allocated memory block.
-- This may be larger than the requested size due to allocator rounding.
--
-- >>> ptr <- mallocForeignPtrBytes 100
-- >>> withForeignPtr ptr $ \p -> usableSize p
usableSize :: Ptr a -> IO Int
usableSize ptr = fromIntegral <$> mi_usable_size ptr
{-# INLINE usableSize #-}


