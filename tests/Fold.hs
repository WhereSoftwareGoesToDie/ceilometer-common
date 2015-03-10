{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Fold where

import qualified Control.Foldl         as L
import qualified Data.Map              as M
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Ceilometer.Fold
import           Ceilometer.Types

import           SampleData

suite :: Spec
suite = do
  -- "Cumulative Pollster" resources
  describe "Folding points for CUMULATIVE resource: CPU" $ do
    it "ok for monotonically decreasing points"
      $ L.fold foldCPU cpuDecreasing `shouldBe` cumulativeDecreasingResult

    it "ok for monotonically increasing points"
      $ L.fold foldCPU cpuIncreasing `shouldBe` cumulativeIncreasingResult

    it "ok for non-monotonic points"
      $ L.fold foldCPU cpuAny `shouldBe` cumulativeAnyResult

  describe "Folding points for CUMULATIVE resource: NeutronRx" $ do
    it "ok for monotonically decreasing points"
      $ L.fold foldNeutronRx neutronRxDecreasing `shouldBe` cumulativeDecreasingResult

    it "ok for monotonically increasing points"
      $ L.fold foldNeutronRx neutronRxIncreasing `shouldBe` cumulativeIncreasingResult

    it "ok for non-monotonic points"
      $ L.fold foldNeutronRx neutronRxAny `shouldBe` cumulativeAnyResult

  describe "Folding points for CUMULATIVE resource: NeutronTx" $ do
    it "ok for monotonically decreasing points"
      $ L.fold foldNeutronTx neutronTxDecreasing `shouldBe` cumulativeDecreasingResult

    it "ok for monotonically increasing points"
      $ L.fold foldNeutronTx neutronTxIncreasing `shouldBe` cumulativeIncreasingResult

    it "ok for non-monotonic points"
      $ L.fold foldNeutronTx neutronTxAny `shouldBe` cumulativeAnyResult

  describe "Folding points for CUMULATIVE resource: DiskWrite" $ do
    it "ok for monotonically decreasing points"
      $ L.fold foldDiskWrite diskWriteDecreasing `shouldBe` cumulativeDecreasingResult

    it "ok for monotonically increasing points"
      $ L.fold foldDiskWrite diskWriteIncreasing `shouldBe` cumulativeIncreasingResult

    it "ok for non-monotonic points"
      $ L.fold foldDiskWrite diskWriteAny `shouldBe` cumulativeAnyResult

  describe "Folding points for CUMULATIVE resource: DiskRead" $ do
    it "ok for monotonically decreasing points"
      $ L.fold foldDiskRead diskReadDecreasing `shouldBe` cumulativeDecreasingResult

    it "ok for monotonically increasing points"
      $ L.fold foldDiskRead diskReadIncreasing `shouldBe` cumulativeIncreasingResult

    it "ok for non-monotonic points"
      $ L.fold foldDiskRead diskReadAny `shouldBe` cumulativeAnyResult

  -- "Gauge Event" resources
  describe "Folding points for EVENT resource: VOLUME" $
    it "ok for example payload"
      $ L.fold foldVolumeAll volumeTimedPDs `shouldBe` volumeTimedPDsResult

  describe "Folding points for EVENT resource: SSD" $
    it "ok for example payload"
      $ L.fold foldSSDAll ssdTimedPDs `shouldBe` ssdTimedPDsResult

  describe "Folding points for EVENT resource: IMAGE" $
    it "ok for example payload"
      $ L.fold foldImageAll imageTimedPDs `shouldBe` imageTimedPDsResult

  describe "Folding points for EVENT resource: SNAPSHOT" $
    it "ok for example payload"
      $ L.fold foldSnapshotAll snapshotTimedPDs `shouldBe` snapshotTimedPDsResult

  -- "Gauge Pollster" resources
  describe "Folding points for POLLSTER resource: INSTANCE FLAVOR" $
    it "ok for example payload"
      $ L.fold (foldInstanceFlavor $ const True) flavorTimedPDs `shouldBe` M.fromList flavorTimedPDsResult

  describe "Filtering instance statuses" $
      prop "is sane" propSafetyInstance

  describe "Folding points for POLLSTER resource: IMAGE POLLSTER" $
    it "ok for example payload"
      $ L.fold foldImagePollster imagePTimedPDs `shouldBe` imagePTimedPDsResult

{- This test no longer applies since we assume no more event after delete
  
  describe "Folding points on edge cases:" $
    prop "discards the rest after VOLUME DELETE" $ property $ do
      vs0 <- listOf volumeNonDelete
      vs1 <- listOf volumeNonDelete
      let bomb = PDVolume VolumeAvailable VolumeDelete Start 10
      let xs0  = zipWith Timed [testS..] $ vs0 ++ [bomb]
      let xs1  = zipWith Timed [testS..] $ vs0 ++ [bomb] ++ vs1
      return $ L.fold foldVolumeAll xs0 == L.fold foldVolumeAll xs1
-}

  where foldVolumeAll   = foldVolume   (testS, testE)
        foldSSDAll      = foldSSD      (testS, testE)
        foldImageAll    = foldImage    (testS, testE)
        foldSnapshotAll = foldSnapshot (testS, testE)
