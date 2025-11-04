{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-|
Module      : Mimalloc.Capability
Description : GHC capability integration for mimalloc
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

/⚠️  EXPERIMENTAL: This module is part of the GHC-specific modifications to mimalloc./
/The capability-based heap system is an experimental approach to making mimalloc work/
/correctly with GHC's green threads. APIs may change as we learn more about optimal usage./

== Background

Standard mimalloc uses OS thread-local storage, which doesn't work correctly with GHC's
green threads (they can migrate between OS threads). This library modifies mimalloc to
use GHC capability numbers as heap identifiers instead, ensuring each capability (virtual
processor) has its own heap rather than each OS thread.

This module provides capability-aware interfaces for working with mimalloc's
capability-based heap system. Each GHC capability (virtual processor) has its
own mimalloc heap for efficient, contention-free allocation.

== Understanding Capabilities

GHC capabilities represent virtual processors that execute Haskell threads.
The number of capabilities is controlled by the @-N@ RTS flag:

* @-N1@: Single capability (sequential execution)
* @-N4@: Four capabilities (good for 4-core CPU)
* @-N@: Auto-detect (uses all available cores)

== How This Works

* Each capability has a dedicated mimalloc heap
* Green threads on the same capability share that capability's heap
* Thread migration is safe (heaps are independent)
* No locks needed between capabilities

== Basic Usage

Most code doesn't need this module - the default allocator handles everything.
This module is useful when you need:

* To reinitialize after changing capability count
* To understand which capability your code is running on
* To implement capability-aware data structures

== Dynamic Capability Changes

If you change the number of capabilities at runtime:

>>> import GHC.Conc (setNumCapabilities)
>>> setNumCapabilities 8  -- Change from 4 to 8 capabilities
>>> reinitializeCapabilities  -- Update mimalloc heaps

-}
module Mimalloc.Capability
  ( -- * Capability Information
    getCurrentCapability
  , getNumCapabilities
  
    -- * Capability Management
  , reinitializeCapabilities
  , initializeCapabilities
  
    -- * Running on Specific Capabilities
  , runOnCapability
  , withCapabilityLocal
  
    -- * Advanced
  , setCurrentCapability
  ) where

import Foreign.C.Types
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import GHC.Conc (getNumCapabilities, myThreadId, forkOn, threadCapability)

-- -----------------------------------------------------------------------------
-- FFI imports
-- -----------------------------------------------------------------------------

foreign import ccall unsafe "_mi_set_num_capabilities"
  c_mi_set_num_capabilities :: CUInt -> IO ()

foreign import ccall unsafe "_mi_set_current_capability"
  c_mi_set_current_capability :: CUInt -> IO ()

foreign import ccall unsafe "_mi_reinitialize_for_capabilities"
  c_mi_reinitialize_for_capabilities :: CUInt -> IO ()

-- -----------------------------------------------------------------------------
-- Capability Information
-- -----------------------------------------------------------------------------

-- | Get the capability number the current thread is running on.
--
-- This returns the GHC capability number (0-based), which determines which
-- mimalloc heap is used for allocations.
--
-- Note: The thread may migrate to a different capability at any time.
-- This function returns the /current/ capability at the moment it's called.
--
-- >>> cap <- getCurrentCapability
-- >>> putStrLn $ "Running on capability " ++ show cap
-- Running on capability 2
getCurrentCapability :: IO Int
getCurrentCapability = do
  tid <- myThreadId
  (cap, _pinned) <- threadCapability tid
  return cap

-- | Set the current capability for mimalloc.
--
-- This is called automatically by the allocator, but can be called explicitly
-- if needed. Most code should not need to call this directly.
--
-- This is a low-level function. Prefer 'withCapabilityLocal' for managing
-- capability-aware allocations.
setCurrentCapability :: Int -> IO ()
setCurrentCapability cap = c_mi_set_current_capability (fromIntegral cap)

-- -----------------------------------------------------------------------------
-- Capability Management
-- -----------------------------------------------------------------------------

-- | Reinitialize mimalloc heaps after changing the number of capabilities.
--
-- Call this after using 'GHC.Conc.setNumCapabilities' to ensure mimalloc's
-- heap array matches the new capability count.
--
-- Example workflow:
--
-- >>> import GHC.Conc (setNumCapabilities)
-- >>> setNumCapabilities 8
-- >>> reinitializeCapabilities
--
-- This is safe to call at any time, even if the capability count hasn't changed.
-- It will:
--
-- * Initialize new heaps when growing capability count
-- * Collect and prepare excess heaps for cleanup when shrinking
-- * Handle thread safety during the transition
reinitializeCapabilities :: IO ()
reinitializeCapabilities = do
  numCaps <- getNumCapabilities
  c_mi_reinitialize_for_capabilities (fromIntegral numCaps)

-- | Initialize mimalloc for the current number of capabilities.
--
-- This is called automatically during library initialization, but can be
-- called explicitly if needed (e.g., after forking).
--
-- Most code should not need to call this directly.
initializeCapabilities :: IO ()
initializeCapabilities = do
  numCaps <- getNumCapabilities
  c_mi_set_num_capabilities (fromIntegral numCaps)

-- -----------------------------------------------------------------------------
-- Running on Specific Capabilities
-- -----------------------------------------------------------------------------

-- | Run an action on a specific capability.
--
-- This ensures the action runs on the specified capability, which means
-- it will use that capability's mimalloc heap.
--
-- >>> runOnCapability 0 $ do
-- >>>   ptr <- mallocForeignPtr  -- Allocates from capability 0's heap
-- >>>   -- ... use ptr ...
--
-- Note: This uses 'forkOn' internally, so there's some overhead. For
-- hot paths, consider restructuring code to run naturally on the desired
-- capability rather than forcing it.
runOnCapability :: Int -> IO a -> IO a
runOnCapability cap action = do
  resultMVar <- newEmptyMVar
  _ <- forkOn cap $ do
    result <- action
    putMVar resultMVar result
  takeMVar resultMVar

-- | Execute an action with capability-local guarantees.
--
-- This ensures the action runs without migrating to another capability,
-- by updating mimalloc's current capability before and after potential
-- migration points.
--
-- This is useful when you need consistent behavior with capability-local heaps:
--
-- >>> withCapabilityLocal $ do
-- >>>   heap <- mi_heap_new  -- Create heap on current capability
-- >>>   ptr <- mi_heap_malloc heap size  -- Allocate from it
-- >>>   -- ... use ptr ...
-- >>>   mi_heap_delete heap  -- Must be on same capability
--
-- Note: This doesn't prevent migration, but it updates mimalloc's capability
-- tracking if migration occurs. For guaranteed non-migration, use 'runOnCapability'.
withCapabilityLocal :: IO a -> IO a
withCapabilityLocal action = do
  -- Get and set capability at start
  cap <- getCurrentCapability
  setCurrentCapability cap
  
  -- Run action
  result <- action
  
  -- Update capability at end (in case of migration)
  cap' <- getCurrentCapability
  setCurrentCapability cap'
  
  return result

