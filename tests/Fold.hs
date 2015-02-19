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
      $ L.fold foldCPU cpuDecreasing `shouldBe` cpuDecreasingResult

    it "ok for monotonically increasing points"
      $ L.fold foldCPU cpuIncreasing `shouldBe` cpuIncreasingResult

    it "ok for non-monotonic points"
      $ L.fold foldCPU cpuAny `shouldBe` cpuAnyResult

  -- "Gauge Event" resources
  describe "Folding points for EVENT resource: VOLUME" $
    it "ok for example payload"
      $ pFold foldVolumeAll volumeTimedPDs `shouldBe` volumeTimedPDsResult

  describe "Folding points for EVENT resource: SSD" $
    it "ok for example payload"
      $ pFold foldSSDAll ssdTimedPDs `shouldBe` ssdTimedPDsResult

  describe "Folding points for EVENT resource: IMAGE" $
    it "ok for example payload"
      $ pFold foldImageAll imageTimedPDs `shouldBe` imageTimedPDsResult

  describe "Folding points for EVENT resource: SNAPSHOT" $
    it "ok for example payload"
      $ pFold foldSnapshotAll snapshotTimedPDs `shouldBe` snapshotTimedPDsResult

  -- "Gauge Pollster" resources
  describe "Folding points for POLLSTER resource: INSTANCE FLAVOR" $
    it "ok for example payload"
      $ L.fold foldInstanceFlavor flavorTimedPDs `shouldBe` M.fromList flavorTimedPDsResult

  describe "Folding points for POLLSTER resource: IMAGE POLLSTER" $
    it "ok for example payload"
      $ L.fold foldImagePollster imagePTimedPDs `shouldBe` imagePTimedPDsResult

  describe "Folding points on edge cases:" $
    prop "discards the rest after VOLUME DELETE" $ property $ do
      vs0 <- listOf volumeNonDelete
      vs1 <- listOf volumeNonDelete
      let bomb = PDVolume VolumeAvailable VolumeDelete Start 10
      let xs0  = zipWith Timed [testS..] $ vs0 ++ [bomb]
      let xs1  = zipWith Timed [testS..] $ vs0 ++ [bomb] ++ vs1
      return $ pFold foldVolumeAll xs0 == pFold foldVolumeAll xs1

  where foldVolumeAll   = foldVolume   (testS, testE)
        foldSSDAll      = foldSSD      (testS, testE)
        foldImageAll    = foldImage    (testS, testE)
        foldSnapshotAll = foldSnapshot (testS, testE)
