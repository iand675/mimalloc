{-# LANGUAGE BangPatterns #-}
{-|
Module      : Mimalloc.ByteString
Description : ByteString integration for mimalloc
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

This module provides integration between mimalloc and ByteString,
allowing you to create ByteStrings backed by mimalloc-allocated memory.

== Thread Safety

* Functions like 'mallocByteString' and 'zallocByteString' use the default heap
  and work correctly with 'forkIO' (no 'forkOS' required)
* Functions like 'heapMallocByteString' use custom heaps and /require 'forkOS'/
  (see "Mimalloc.Heap" for details on heap thread-safety requirements)

== Usage

>>> bs <- mallocByteString 1024
>>> bs <- heapMallocByteString heap 2048

-}
module Mimalloc.ByteString
  ( -- * ByteString Creation
    mallocByteString
  , zallocByteString
  , heapMallocByteString
  , heapZallocByteString
  
    -- * Unsafe Operations
  , unsafePackMallocByteString
  , unsafeUseAsByteString
  ) where

import Mimalloc.Primitive
import Mimalloc.Heap (Heap(Heap))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word

foreign import ccall "&mi_free"
  p_mi_free :: FinalizerPtr a

-- -----------------------------------------------------------------------------
-- ByteString Creation
-- -----------------------------------------------------------------------------

-- | Create an uninitialized ByteString of the specified size using mimalloc.
-- The memory is automatically managed and will be freed when the ByteString
-- is garbage collected.
--
-- >>> bs <- mallocByteString 1024
mallocByteString :: Int -> IO BS.ByteString
mallocByteString size = do
  let csize = fromIntegral size
  ptr <- mi_malloc csize
  if ptr == nullPtr
    then error "mimalloc: ByteString allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! BSI.PS fp 0 size
{-# INLINE mallocByteString #-}

-- | Create a zero-initialized ByteString of the specified size using mimalloc.
--
-- >>> bs <- zallocByteString 1024
zallocByteString :: Int -> IO BS.ByteString
zallocByteString size = do
  let csize = fromIntegral size
  ptr <- mi_zalloc csize
  if ptr == nullPtr
    then error "mimalloc: ByteString zero allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! BSI.PS fp 0 size
{-# INLINE zallocByteString #-}

-- | Create an uninitialized ByteString from a specific heap.
--
-- >>> withHeap $ \heap -> heapMallocByteString heap 1024
heapMallocByteString :: Heap s -> Int -> IO BS.ByteString
heapMallocByteString (Heap heap) size = do
  let csize = fromIntegral size
  ptr <- mi_heap_malloc heap csize
  if ptr == nullPtr
    then error "mimalloc: ByteString heap allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! BSI.PS fp 0 size
{-# INLINE heapMallocByteString #-}

-- | Create a zero-initialized ByteString from a specific heap.
--
-- >>> withHeap $ \heap -> heapZallocByteString heap 1024
heapZallocByteString :: Heap s -> Int -> IO BS.ByteString
heapZallocByteString (Heap heap) size = do
  let csize = fromIntegral size
  ptr <- mi_heap_zalloc heap csize
  if ptr == nullPtr
    then error "mimalloc: ByteString heap zero allocation failed"
    else do
      fp <- newForeignPtr p_mi_free ptr
      return $! BSI.PS fp 0 size
{-# INLINE heapZallocByteString #-}

-- -----------------------------------------------------------------------------
-- Unsafe Operations
-- -----------------------------------------------------------------------------

-- | Wrap a mimalloc-allocated pointer as a ByteString.
-- 
-- /UNSAFE/: The pointer must have been allocated with mimalloc and must
-- remain valid for the given length. This function takes ownership of
-- the pointer and will free it when the ByteString is garbage collected.
unsafePackMallocByteString :: Ptr Word8 -> Int -> IO BS.ByteString
unsafePackMallocByteString ptr size = do
  fp <- newForeignPtr p_mi_free ptr
  return $! BSI.PS fp 0 size
{-# INLINE unsafePackMallocByteString #-}

-- | Use a ByteString's underlying pointer without copying.
--
-- /UNSAFE/: The pointer is only valid during the action. Do not let
-- it escape the action's scope.
unsafeUseAsByteString :: BS.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
unsafeUseAsByteString bs action =
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    action (castPtr ptr) len
{-# INLINE unsafeUseAsByteString #-}


