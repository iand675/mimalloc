{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (replicateM, forM, forM_, replicateM_, when)
import Control.Exception (evaluate, try, SomeException, bracket)
import Data.IORef
import Data.List (sort)
import Data.Word
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, finalizeForeignPtr)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, WordPtr, IntPtr, nullPtr, wordPtrToPtr, intPtrToPtr, ptrToWordPtr, ptrToIntPtr)
import Foreign.Storable (Storable(..))
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import System.Mem (performGC, performMinorGC)
import qualified Data.ByteString as BS

import Mimalloc
import Mimalloc.Capability
import Mimalloc.ByteString
import Mimalloc.Primitive

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Mimalloc Capability-Based Tests"
  [ basicTests
  , concurrencyTests
  , stressTests
  , capabilityManagementTests
  , memoryLifecycleTests
  , alignmentTests
  , integrationTests
  , propertyTests
  ]

-- =============================================================================
-- Basic Allocation Tests
-- =============================================================================

basicTests :: TestTree
basicTests = testGroup "Basic Allocation"
  [ testCase "Single allocation" $ do
      ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
      withForeignPtr ptr $ \p -> do
        poke p (42 :: Int)
        val <- peek p
        val @?= 42

  , testCase "Multiple sequential allocations" $ do
      ptrs <- replicateM 1000 (mallocForeignPtr :: IO (ForeignPtr Int))
      forM_ (zip [0..] ptrs) $ \(i :: Int, ptr) -> do
        withForeignPtr ptr $ \p -> do
          poke p i
          val <- peek p
          val @?= i

  , testCase "Different types" $ do
      intPtr <- mallocForeignPtr :: IO (ForeignPtr Int)
      doublePtr <- mallocForeignPtr :: IO (ForeignPtr Double)
      word64Ptr <- mallocForeignPtr :: IO (ForeignPtr Word64)
      
      withForeignPtr intPtr $ \p -> poke p (123 :: Int)
      withForeignPtr doublePtr $ \p -> poke p (3.14 :: Double)
      withForeignPtr word64Ptr $ \p -> poke p (99999 :: Word64)
      
      v1 <- withForeignPtr intPtr peek
      v2 <- withForeignPtr doublePtr peek
      v3 <- withForeignPtr word64Ptr peek
      
      v1 @?= 123
      v2 @?= 3.14
      v3 @?= 99999

  , testCase "Array allocation" $ do
      arr <- mallocForeignPtrArray 100 :: IO (ForeignPtr Int)
      withForeignPtr arr $ \p -> do
        pokeArray p [0..99]
        vals <- peekArray 100 p
        vals @?= [0..99]

  , testCase "Zero-initialized allocation" $ do
      ptr <- callocForeignPtr :: IO (ForeignPtr Word64)
      withForeignPtr ptr $ \p -> do
        val <- peek p
        val @?= 0

  , testCase "Aligned allocation" $ do
      ptr <- mallocForeignPtr :: IO (ForeignPtr Word64)
      withForeignPtr ptr $ \p -> do
        -- Note: default allocation should be at least word-aligned
        let addr = fromIntegral (ptrToWordPtr p)
        (addr `mod` 8) @?= 0
        poke p (0xDEADBEEF :: Word64)
        val <- peek p
        val @?= 0xDEADBEEF
  ]

-- =============================================================================
-- Concurrency Tests
-- =============================================================================

concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency"
  [ testCase "100 concurrent threads allocating" $ do
      results <- newEmptyMVar
      let numThreads = 100
      
      replicateM_ numThreads $ forkIO $ do
        ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
        withForeignPtr ptr $ \p -> do
          poke p 42
          val <- peek p
          when (val /= 42) $ putMVar results (Left "Value mismatch")
        putMVar results (Right ())
      
      outcomes <- replicateM numThreads (takeMVar results)
      let errors = [e | Left e <- outcomes]
      assertBool ("Concurrent allocation errors: " ++ show errors) (null errors)

  , testCase "1000 threads rapid allocation/deallocation" $ do
      counter <- newIORef (0 :: Int)
      let numThreads = 1000
      
      threads <- replicateM numThreads $ async $ do
        replicateM_ 10 $ do
          ptr <- mallocForeignPtr :: IO (ForeignPtr Word64)
          withForeignPtr ptr $ \p -> poke p 123
          atomicModifyIORef' counter (\x -> (x+1, ()))
      
      mapM_ wait threads
      count <- readIORef counter
      count @?= (numThreads * 10)

  , testCase "Cross-thread sharing" $ do
      ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
      withForeignPtr ptr $ \p -> poke p 100
      
      resultMVar <- newEmptyMVar
      _ <- forkIO $ do
        val <- withForeignPtr ptr peek
        putMVar resultMVar val
      
      result <- takeMVar resultMVar
      result @?= 100

  , testCase "Multiple capabilities allocation pattern" $ do
      numCaps <- getNumCapabilities
      resultMVar <- newEmptyMVar
      
      -- Allocate from each capability
      forM_ [0..(numCaps-1)] $ \cap -> do
        _ <- async $ runOnCapability cap $ do
          ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
          withForeignPtr ptr $ \p -> do
            poke p cap
            val <- peek p
            putMVar resultMVar (cap, val)
        return ()
      
      -- Verify all capabilities allocated successfully
      results <- replicateM numCaps (takeMVar resultMVar)
      let sorted = sort results
      forM_ (zip [0..] sorted) $ \(expected, (cap, val)) -> do
        cap @?= expected
        val @?= expected

  , testCase "Thread migration safety (extended)" $ do
      resultMVar <- newEmptyMVar
      
      -- Spawn thread that will likely migrate
      _ <- forkIO $ do
        allocations <- replicateM 50 $ do
          ptr <- mallocForeignPtr :: IO (ForeignPtr Word64)
          withForeignPtr ptr $ \p -> do
            poke p 0xCAFEBABE
            -- Force some work that might trigger migration
            _ <- evaluate (sum [1..100 :: Int])
            peek p
        
        putMVar resultMVar allocations
      
      results <- takeMVar resultMVar
      assertBool "All allocations valid after migration" 
        (all (== 0xCAFEBABE) results)
  ]

-- =============================================================================
-- Stress Tests
-- =============================================================================

stressTests :: TestTree
stressTests = testGroup "Stress Tests"
  [ testCase "10000 allocations" $ do
      ptrs <- replicateM 10000 (mallocForeignPtr :: IO (ForeignPtr Word64))
      forM_ (zip [0..] ptrs) $ \(i, ptr) -> do
        withForeignPtr ptr $ \p -> poke p i
      performGC
      
      -- Verify all still accessible
      forM_ (zip [0..] ptrs) $ \(i, ptr) -> do
        val <- withForeignPtr ptr peek
        val @?= i

  , testCase "Large array allocations" $ do
      ptrs <- replicateM 100 (mallocForeignPtrArray 10000 :: IO (ForeignPtr Int))
      forM_ (zip [0..] ptrs) $ \(i, ptr) -> do
        withForeignPtr ptr $ \p -> pokeArray p (replicate 10000 i)
      performGC
      
      forM_ (zip [0..] ptrs) $ \(i, ptr) -> do
        vals <- withForeignPtr ptr $ \p -> peekArray 10000 p
        assertBool "Array values intact" (all (== i) vals)

  , testCase "Mixed size allocations" $ do
      let sizes = cycle [1, 10, 100, 1000, 10000]
      ptrs <- forM (take 1000 sizes) $ \size ->
        mallocForeignPtrArray size :: IO (ForeignPtr Word8)
      
      forM_ ptrs $ \ptr -> do
        withForeignPtr ptr $ \p -> poke p (42 :: Word8)
      
      performGC
      
      forM_ ptrs $ \ptr -> do
        val <- withForeignPtr ptr peek
        val @?= (42 :: Word8)

  , testCase "Parallel stress test" $ do
      numThreads <- getNumCapabilities
      let allocsPerThread = 1000
      
      threads <- replicateM numThreads $ async $ do
        ptrs <- replicateM allocsPerThread (mallocForeignPtr :: IO (ForeignPtr Int))
        forM_ (zip [0..] ptrs) $ \(i, ptr) -> do
          withForeignPtr ptr $ \p -> do
            poke p i
            val <- peek p
            when (val /= i) $ error "Value corruption"
        return (length ptrs)
      
      counts <- mapM wait threads
      sum counts @?= (numThreads * allocsPerThread)

  , testCase "Rapid alloc/free cycles" $ do
      replicateM_ 1000 $ do
        ptr <- mallocForeignPtr :: IO (ForeignPtr Word64)
        withForeignPtr ptr $ \p -> poke p 123
        finalizeForeignPtr ptr
        performMinorGC
  ]

-- =============================================================================
-- Capability Management Tests
-- =============================================================================

capabilityManagementTests :: TestTree
capabilityManagementTests = testGroup "Capability Management"
  [ testCase "Get current capability" $ do
      cap <- getCurrentCapability
      numCaps <- getNumCapabilities
      assertBool "Capability in valid range" (cap >= 0 && cap < numCaps)

  , testCase "Capability reinit after setNumCapabilities" $ do
      initialCaps <- getNumCapabilities
      
      -- Allocate before change
      ptr1 <- mallocForeignPtr :: IO (ForeignPtr Int)
      withForeignPtr ptr1 $ \p -> poke p 100
      
      -- Change capabilities (if possible)
      let newCaps = max 2 (initialCaps `div` 2)
      setNumCapabilities newCaps
      threadDelay 10000  -- Give RTS time to adjust
      reinitializeCapabilities
      
      -- Allocate after change
      ptr2 <- mallocForeignPtr :: IO (ForeignPtr Int)
      withForeignPtr ptr2 $ \p -> poke p 200
      
      -- Verify both allocations still work
      v1 <- withForeignPtr ptr1 peek
      v2 <- withForeignPtr ptr2 peek
      
      v1 @?= 100
      v2 @?= 200
      
      -- Restore original
      setNumCapabilities initialCaps
      reinitializeCapabilities

  , testCase "Multiple capability changes" $ do
      initialCaps <- getNumCapabilities
      
      let capSeq = [2, 4, 1, 4, 2]
      forM_ capSeq $ \n -> do
        setNumCapabilities n
        threadDelay 5000
        reinitializeCapabilities
        
        -- Test allocation works
        ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
        withForeignPtr ptr $ \p -> do
          poke p n
          val <- peek p
          val @?= n
      
      setNumCapabilities initialCaps
      reinitializeCapabilities

  , testCase "Capability-specific operations" $ do
      numCaps <- getNumCapabilities
      when (numCaps > 1) $ do
        -- Allocate on specific capability
        resultMVar <- newEmptyMVar
        runOnCapability 0 $ do
          ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
          withForeignPtr ptr $ \p -> do
            poke p 999
            val <- peek p
            putMVar resultMVar val
        
        result <- takeMVar resultMVar
        result @?= 999
  ]

-- =============================================================================
-- Memory Lifecycle Tests
-- =============================================================================

memoryLifecycleTests :: TestTree
memoryLifecycleTests = testGroup "Memory Lifecycle"
  [ testCase "GC interaction" $ do
      ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
      withForeignPtr ptr $ \p -> poke p 42
      
      performGC
      performGC
      performGC
      
      val <- withForeignPtr ptr peek
      val @?= 42

  , testCase "Finalizer execution" $ do
      counter <- newIORef (0 :: Int)
      
      replicateM_ 100 $ do
        ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
        withForeignPtr ptr $ \p -> poke p 1
        finalizeForeignPtr ptr
        atomicModifyIORef' counter (\x -> (x+1, ()))
      
      count <- readIORef counter
      count @?= 100

  , testCase "Large allocation lifecycle" $ do
      -- Allocate large block
      largePtr <- mallocForeignPtrArray 1000000 :: IO (ForeignPtr Word8)
      withForeignPtr largePtr $ \p -> poke p 255
      
      performGC
      
      -- Should still be accessible
      val <- withForeignPtr largePtr peek
      val @?= 255
      
      finalizeForeignPtr largePtr
      performGC

  , testCase "Allocation after explicit finalization" $ do
      -- Allocate and finalize
      ptr1 <- mallocForeignPtr :: IO (ForeignPtr Int)
      finalizeForeignPtr ptr1
      
      -- Should be able to allocate more
      ptr2 <- mallocForeignPtr :: IO (ForeignPtr Int)
      withForeignPtr ptr2 $ \p -> do
        poke p 123
        val <- peek p
        val @?= 123
  ]

-- =============================================================================
-- Alignment Tests
-- =============================================================================

alignmentTests :: TestTree
alignmentTests = testGroup "Alignment"
  [ testCase "8-byte alignment" $ testAlignment 8
  , testCase "16-byte alignment" $ testAlignment 16
  , testCase "32-byte alignment" $ testAlignment 32
  , testCase "64-byte alignment" $ testAlignment 64
  , testCase "128-byte alignment" $ testAlignment 128
  , testCase "256-byte alignment" $ testAlignment 256
  
  ]
  where
    testAlignment :: Int -> Assertion
    testAlignment align = do
      ptr <- mallocForeignPtrAligned (fromIntegral align) :: IO (ForeignPtr Word64)
      withForeignPtr ptr $ \p -> do
        let addr = fromIntegral (ptrToWordPtr p)
        assertBool (show align ++ "-byte aligned") 
          ((addr `mod` fromIntegral align) == 0)

-- =============================================================================
-- Integration Tests (ByteString, etc.)
-- =============================================================================

integrationTests :: TestTree
integrationTests = testGroup "Integration"
  [ testCase "ByteString creation" $ do
      bs <- mallocByteString 1000
      BS.length bs @?= 1000

  , testCase "Multiple ByteString allocations" $ do
      bss <- replicateM 100 (mallocByteString 100)
      forM_ bss $ \bs -> do
        BS.length bs @?= 100

  , testCase "ByteString lifecycle" $ do
      bs <- mallocByteString 1
      performGC
      BS.length bs @?= 1
  ]

-- =============================================================================
-- Property-Based Tests
-- =============================================================================

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ QC.testProperty "Allocation roundtrip (Int)" $ \(val :: Int) ->
      QCM.monadicIO $ do
        ptr <- QCM.run $ mallocForeignPtr
        QCM.run $ withForeignPtr ptr $ \p -> poke p val
        result <- QCM.run $ withForeignPtr ptr peek
        QCM.assert (result == val)

  , QC.testProperty "Allocation roundtrip (Word64)" $ \(val :: Word64) ->
      QCM.monadicIO $ do
        ptr <- QCM.run $ mallocForeignPtr
        QCM.run $ withForeignPtr ptr $ \p -> poke p val
        result <- QCM.run $ withForeignPtr ptr peek
        QCM.assert (result == val)

  , QC.testProperty "Array roundtrip" $ \(vals :: [Int]) ->
      not (null vals) QC.==> QCM.monadicIO $ do
        ptr <- QCM.run $ mallocForeignPtrArray (length vals)
        QCM.run $ withForeignPtr ptr $ \p -> pokeArray p vals
        result <- QCM.run $ withForeignPtr ptr $ \p -> peekArray (length vals) p
        QCM.assert (result == vals)

  , QC.testProperty "Word alignment maintained" $ QCM.monadicIO $ do
      ptr <- QCM.run $ (mallocForeignPtr :: IO (ForeignPtr Word64))
      isAligned <- QCM.run $ withForeignPtr ptr $ \p -> do
        let addr = fromIntegral (ptrToWordPtr p)
        return ((addr `mod` 8) == 0)
      QCM.assert isAligned

  , QC.testProperty "Concurrent allocations don't interfere" $ 
      \(vals :: [(Int, Word64)]) ->
        length vals > 0 && length vals <= 100 QC.==> QCM.monadicIO $ do
          results <- QCM.run $ do
            mvars <- replicateM (length vals) newEmptyMVar
            forM_ (zip vals mvars) $ \((i, w), mvar) -> do
              _ <- forkIO $ do
                ptr <- mallocForeignPtr
                withForeignPtr ptr $ \p -> do
                  poke p w
                  val <- peek p
                  putMVar mvar (i, val)
              return ()
            mapM takeMVar mvars
          
          let expected = sort vals
          let actual = sort results
          QCM.assert (actual == expected)
  ]

-- ptrToWordPtr and related functions are imported from Foreign.Ptr
