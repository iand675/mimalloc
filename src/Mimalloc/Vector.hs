{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Mimalloc.Vector
Description : Vector integration for mimalloc
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

This module provides integration between mimalloc and Data.Vector,
allowing you to create Vectors backed by mimalloc-allocated memory.

== Thread Safety

* Functions like 'mallocVector' and 'mallocMVector' use the default heap
  and work correctly with 'forkIO' (no 'forkOS' required)
* Functions like 'heapMallocVector' use custom heaps and /require 'forkOS'/
  (see "Mimalloc.Heap" for details on heap thread-safety requirements)

== Usage

>>> vec <- mallocVector 100 :: IO (Vector Int)
>>> mvec <- mallocMVector 100 :: IO (MVector RealWorld Int)

-}
module Mimalloc.Vector
  ( -- * Immutable Vector Creation
    mallocVector
  , zallocVector
  , heapMallocVector
  , heapZallocVector
  
    -- * Mutable Vector Creation
  , mallocMVector
  , zallocMVector
  , heapMallocMVector
  , heapZallocMVector
  
    -- * Unsafe Operations
  , unsafeFromForeignPtr
  , unsafeFromForeignPtr0
  ) where

import Mimalloc.Primitive
import Mimalloc.Heap (Heap(Heap))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.Primitive (RealWorld)

foreign import ccall "&mi_free"
  p_mi_free :: FinalizerPtr a

-- -----------------------------------------------------------------------------
-- Immutable Vector Creation
-- -----------------------------------------------------------------------------

-- | Create an uninitialized immutable Vector using mimalloc.
--
-- >>> vec <- mallocVector 100 :: IO (Vector Int)
mallocVector :: forall a. Storable a => Int -> IO (VS.Vector a)
mallocVector n = do
  let size = fromIntegral $ n * sizeOf (undefined :: a)
  ptr <- mi_malloc size
  if ptr == nullPtr
    then error "mimalloc: Vector allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! unsafeFromForeignPtr0 fp n
{-# INLINE mallocVector #-}

-- | Create a zero-initialized immutable Vector using mimalloc.
--
-- >>> vec <- zallocVector 100 :: IO (Vector Int)
zallocVector :: forall a. Storable a => Int -> IO (VS.Vector a)
zallocVector n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_calloc count size
  if ptr == nullPtr
    then error "mimalloc: Vector zero allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! unsafeFromForeignPtr0 fp n
{-# INLINE zallocVector #-}

-- | Create an uninitialized immutable Vector from a specific heap.
--
-- >>> withHeap $ \heap -> heapMallocVector heap 100 :: IO (Vector Int)
heapMallocVector :: forall s a. Storable a => Heap s -> Int -> IO (VS.Vector a)
heapMallocVector (Heap heap) n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_mallocn heap count size
  if ptr == nullPtr
    then error "mimalloc: Vector heap allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! unsafeFromForeignPtr0 fp n
{-# INLINE heapMallocVector #-}

-- | Create a zero-initialized immutable Vector from a specific heap.
--
-- >>> withHeap $ \heap -> heapZallocVector heap 100 :: IO (Vector Int)
heapZallocVector :: forall s a. Storable a => Heap s -> Int -> IO (VS.Vector a)
heapZallocVector (Heap heap) n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_calloc heap count size
  if ptr == nullPtr
    then error "mimalloc: Vector heap zero allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! unsafeFromForeignPtr0 fp n
{-# INLINE heapZallocVector #-}

-- -----------------------------------------------------------------------------
-- Mutable Vector Creation
-- -----------------------------------------------------------------------------

-- | Create an uninitialized mutable Vector using mimalloc.
--
-- >>> mvec <- mallocMVector 100 :: IO (MVector RealWorld Int)
mallocMVector :: forall a. Storable a => Int -> IO (VSM.MVector RealWorld a)
mallocMVector n = do
  let size = fromIntegral $ n * sizeOf (undefined :: a)
  ptr <- mi_malloc size
  if ptr == nullPtr
    then error "mimalloc: MVector allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! VSM.unsafeFromForeignPtr0 fp n
{-# INLINE mallocMVector #-}

-- | Create a zero-initialized mutable Vector using mimalloc.
--
-- >>> mvec <- zallocMVector 100 :: IO (MVector RealWorld Int)
zallocMVector :: forall a. Storable a => Int -> IO (VSM.MVector RealWorld a)
zallocMVector n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_calloc count size
  if ptr == nullPtr
    then error "mimalloc: MVector zero allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! VSM.unsafeFromForeignPtr0 fp n
{-# INLINE zallocMVector #-}

-- | Create an uninitialized mutable Vector from a specific heap.
--
-- >>> withHeap $ \heap -> heapMallocMVector heap 100 :: IO (MVector RealWorld Int)
heapMallocMVector :: forall s a. Storable a => Heap s -> Int -> IO (VSM.MVector RealWorld a)
heapMallocMVector (Heap heap) n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_mallocn heap count size
  if ptr == nullPtr
    then error "mimalloc: MVector heap allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! VSM.unsafeFromForeignPtr0 fp n
{-# INLINE heapMallocMVector #-}

-- | Create a zero-initialized mutable Vector from a specific heap.
--
-- >>> withHeap $ \heap -> heapZallocMVector heap 100 :: IO (MVector RealWorld Int)
heapZallocMVector :: forall s a. Storable a => Heap s -> Int -> IO (VSM.MVector RealWorld a)
heapZallocMVector (Heap heap) n = do
  let count = fromIntegral n
  let size = fromIntegral $ sizeOf (undefined :: a)
  ptr <- mi_heap_calloc heap count size
  if ptr == nullPtr
    then error "mimalloc: MVector heap zero allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! VSM.unsafeFromForeignPtr0 fp n
{-# INLINE heapZallocMVector #-}

-- -----------------------------------------------------------------------------
-- Unsafe Operations
-- -----------------------------------------------------------------------------

-- | Create an immutable Vector from a ForeignPtr with offset.
--
-- /UNSAFE/: This is a low-level function. The ForeignPtr must point to
-- valid memory with the correct size.
unsafeFromForeignPtr :: Storable a => ForeignPtr a -> Int -> Int -> VS.Vector a
unsafeFromForeignPtr = VS.unsafeFromForeignPtr
{-# INLINE unsafeFromForeignPtr #-}

-- | Create an immutable Vector from a ForeignPtr without offset.
--
-- /UNSAFE/: This is a low-level function. The ForeignPtr must point to
-- valid memory with the correct size.
unsafeFromForeignPtr0 :: ForeignPtr a -> Int -> VS.Vector a
unsafeFromForeignPtr0 = VS.unsafeFromForeignPtr0
{-# INLINE unsafeFromForeignPtr0 #-}


