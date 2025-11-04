{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Mimalloc.Stats
Description : Statistics and introspection for mimalloc
Copyright   : (c) 2025 Ian Duncan
License     : BSD-3-Clause
Maintainer  : ian@iankduncan.com

This module provides functions for querying mimalloc statistics and
inspecting heap state.

== Usage

>>> stats <- getProcessInfo
>>> print $ processInfoCurrentRSS stats

>>> printStats
>>> resetStats

-}
module Mimalloc.Stats
  ( -- * Process Information
    ProcessInfo(..)
  , getProcessInfo
  
    -- * Statistics Operations
  , printStats
  , resetStats
  , mergeStats
  
    -- * Memory Operations
  , collect
  , collectForce
  
    -- * Version Information
  , getMimallocVersion
  , mimallocVersion
  
  ) where

import Mimalloc.Primitive
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

-- -----------------------------------------------------------------------------
-- Process Information
-- -----------------------------------------------------------------------------

-- | Information about the process's memory usage.
data ProcessInfo = ProcessInfo
  { processInfoElapsedMsecs :: !Int    -- ^ Elapsed time in milliseconds
  , processInfoUserMsecs :: !Int       -- ^ User CPU time in milliseconds
  , processInfoSystemMsecs :: !Int     -- ^ System CPU time in milliseconds
  , processInfoCurrentRSS :: !Int      -- ^ Current resident set size in bytes
  , processInfoPeakRSS :: !Int         -- ^ Peak resident set size in bytes
  , processInfoCurrentCommit :: !Int   -- ^ Current committed memory in bytes
  , processInfoPeakCommit :: !Int      -- ^ Peak committed memory in bytes
  , processInfoPageFaults :: !Int      -- ^ Number of page faults
  } deriving (Show, Eq)

-- | Get current process information from mimalloc.
--
-- >>> info <- getProcessInfo
-- >>> print $ processInfoCurrentRSS info
getProcessInfo :: IO ProcessInfo
getProcessInfo =
  alloca $ \elapsedPtr ->
  alloca $ \userPtr ->
  alloca $ \systemPtr ->
  alloca $ \currentRssPtr ->
  alloca $ \peakRssPtr ->
  alloca $ \currentCommitPtr ->
  alloca $ \peakCommitPtr ->
  alloca $ \pageFaultsPtr -> do
    mi_process_info elapsedPtr userPtr systemPtr currentRssPtr peakRssPtr currentCommitPtr peakCommitPtr pageFaultsPtr
    elapsed <- fromIntegral <$> peek elapsedPtr
    user <- fromIntegral <$> peek userPtr
    system <- fromIntegral <$> peek systemPtr
    currentRss <- fromIntegral <$> peek currentRssPtr
    peakRss <- fromIntegral <$> peek peakRssPtr
    currentCommit <- fromIntegral <$> peek currentCommitPtr
    peakCommit <- fromIntegral <$> peek peakCommitPtr
    pageFaults <- fromIntegral <$> peek pageFaultsPtr
    return ProcessInfo
      { processInfoElapsedMsecs = elapsed
      , processInfoUserMsecs = user
      , processInfoSystemMsecs = system
      , processInfoCurrentRSS = currentRss
      , processInfoPeakRSS = peakRss
      , processInfoCurrentCommit = currentCommit
      , processInfoPeakCommit = peakCommit
      , processInfoPageFaults = pageFaults
      }

-- -----------------------------------------------------------------------------
-- Statistics Operations
-- -----------------------------------------------------------------------------

-- | Print mimalloc statistics to stdout.
--
-- >>> printStats
printStats :: IO ()
printStats = mi_stats_print nullPtr

-- | Reset all statistics counters.
--
-- >>> resetStats
resetStats :: IO ()
resetStats = mi_stats_reset

-- | Merge thread-local statistics into the main statistics.
-- This is useful for getting accurate statistics in multi-threaded programs.
--
-- >>> mergeStats
mergeStats :: IO ()
mergeStats = mi_stats_merge

-- -----------------------------------------------------------------------------
-- Memory Operations
-- -----------------------------------------------------------------------------

-- | Trigger garbage collection of unused memory.
-- This will free memory back to the OS opportunistically.
--
-- >>> collect
collect :: IO ()
collect = mi_collect 0

-- | Force garbage collection of all unused memory.
-- This will free all possible memory back to the OS.
--
-- >>> collectForce
collectForce :: IO ()
collectForce = mi_collect 1

-- -----------------------------------------------------------------------------
-- Version Information
-- -----------------------------------------------------------------------------

-- | Get the mimalloc library version at runtime.
--
-- >>> ver <- getMimallocVersion
-- >>> print ver
getMimallocVersion :: IO Int
getMimallocVersion = fromIntegral <$> mi_version

-- | The mimalloc library version (pure).
--
-- >>> mimallocVersion
-- 225
mimallocVersion :: Int
mimallocVersion = unsafePerformIO getMimallocVersion
{-# NOINLINE mimallocVersion #-}

