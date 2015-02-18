-- This module defines hard-coded sample data
-- for parsing/printing and folding.
--
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS -fno-warn-orphans#-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module SampleData where

import           Control.Applicative
import           Control.Lens              hiding (elements)
import qualified Data.Bimap                as BM
import           Data.Binary               (Word64)
import           Data.Bits
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Ceilometer.Types
import           Ceilometer.Types.Instance (siphashID)
import           Ceilometer.Types.Volume   (volumeVerb)


-- Instances (DO NOT DEFNE NEW ONES IN TESTS) ----------------------------------

instance Function PFEndpoint       where function = functionShow
instance Function PFVolumeStatus   where function = functionShow
instance Function PFVolumeVerb     where function = functionShow
instance Function PDCPU            where function = functionShow
instance Function PDVolume         where function = functionShow
instance Function PDSSD            where function = functionShow
instance Function PDInstanceVCPU   where function = functionShow
instance Function PDInstanceRAM    where function = functionShow
instance Function PDInstanceDisk   where function = functionShow
instance Function PDInstanceFlavor where function = functionShow

instance Arbitrary PRSimple        where
  arbitrary = PRSimple <$> arbitrary
instance Arbitrary PRCompoundEvent where
  arbitrary =   PRCompoundEvent
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
instance Arbitrary PRCompoundPollster where
  arbitrary =   PRCompoundPollster
            <$> arbitrary
            <*> arbitrary

instance Arbitrary PFEndpoint       where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFVolumeVerb     where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFVolumeStatus   where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFInstanceStatus where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFValueText      where arbitrary = elements $ BM.keys testFlavors

instance Arbitrary PDCPU      where arbitrary =   PDCPU <$> arbitrary
instance Arbitrary PDVolume   where arbitrary =   PDVolume
                                            <$> arbitrary
                                            <*> arbitrary
                                            <*> arbitrary
                                            <*> arbitrary
instance Arbitrary PDSSD      where arbitrary =   PDSSD
                                            <$> arbitrary
                                            <*> arbitrary
                                            <*> arbitrary
                                            <*> arbitrary
instance Arbitrary PDInstanceVCPU   where arbitrary =   PDInstanceVCPU
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceRAM   where arbitrary =   PDInstanceRAM
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceDisk   where arbitrary =   PDInstanceDisk
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceFlavor where arbitrary =   PDInstanceFlavor
                                                  <$> arbitrary
                                                  <*> arbitrary

instance CoArbitrary PFEndpoint     where coarbitrary = variant . fromEnum
instance CoArbitrary PFVolumeStatus where coarbitrary = variant . fromEnum
instance CoArbitrary PFVolumeVerb   where coarbitrary = variant . fromEnum

instance CoArbitrary PDCPU          where
  coarbitrary x = variant $ x ^. re pdCPU    . re prSimple
instance CoArbitrary PDVolume       where
  coarbitrary x = variant $ x ^. re pdVolume . re prCompoundEvent
instance CoArbitrary PDSSD          where
  coarbitrary x = variant $ x ^. re pdSSD    . re prCompoundEvent
instance CoArbitrary PDInstanceFlavor where
  coarbitrary x = variant $ x ^. re (pdInstanceFlavor testFlavors) . re prCompoundPollster
instance CoArbitrary PDInstanceVCPU where
  coarbitrary x = variant $ x ^. re pdInstanceVCPU . re prCompoundPollster
instance CoArbitrary PDInstanceRAM  where
  coarbitrary x = variant $ x ^. re pdInstanceRAM . re prCompoundPollster
instance CoArbitrary PDInstanceDisk where
  coarbitrary x = variant $ x ^. re pdInstanceDisk . re prCompoundPollster

testS, testE :: Word64
testS = 1300000000000000000
testE = 1400000000000000000



-- INSTANCES -------------------------------------------------------------------

testFlavors :: FlavorMap
testFlavors = BM.fromList [ (flavorID0, flavorSP0)
                          , (flavorID1, flavorSP1)
                          , (flavorID2, flavorSP2) ]

flavorID0   = "deadbeef"
flavorSP0   = siphashID flavorID0
flavorID1   = "koolaid"
flavorSP1   = siphashID flavorID1
flavorID2   = "chicken"
flavorSP2   = siphashID flavorID2

flavorPR0, flavorPR1, flavorPR2 :: Word64
flavorPR0 = 1  + (0 `shift` 8) + (0 `shift` 16) + (fromIntegral flavorSP0 `shift` 32)
flavorPR1 = 9  + (0 `shift` 8) + (0 `shift` 16) + (fromIntegral flavorSP1 `shift` 32)
flavorPR2 = 15 + (0 `shift` 8) + (0 `shift` 16) + (fromIntegral flavorSP2 `shift` 32)

flavorPRs = [ flavorPR0, flavorPR1, flavorPR2 ]

flavorPD0 = PDInstanceFlavor InstanceActive flavorID0
flavorPD1 = PDInstanceFlavor InstanceReboot flavorID1
flavorPD2 = PDInstanceFlavor InstancePaused flavorID2

flavorPDs = [ flavorPD0, flavorPD1, flavorPD2 ]

flavorTimedPDs = [ Timed testS flavorPD1
                 , Timed (testS + 2) flavorPD0
                 , Timed (testS + 7) flavorPD2
                 , Timed (testS + 11) flavorPD0
                 , Timed (testS + 13) flavorPD0 ]

flavorTimedPDsResult :: [(PFValue PDInstanceFlavor, Word64)]
flavorTimedPDsResult = [ (flavorID0, 7), (flavorID1, 2) ]

-- CPU -------------------------------------------------------------------------

cpuIncreasing = map PDCPU [30, 40, 50, 60]
cpuDecreasing = map PDCPU [40, 30, 20, 10]
cpuAny        = map PDCPU [20, 30, 10, 20, 30, 40, 10]

cpuDecreasingResult, cpuIncreasingResult, cpuAnyResult :: Word64
cpuDecreasingResult = 60
cpuIncreasingResult = 30
cpuAnyResult        = 60


-- VOLUME ----------------------------------------------------------------------

volumeNonDelete :: Gen PDVolume
volumeNonDelete = (arbitrary :: Gen PDVolume) `suchThat` ((/= VolumeDelete) . view volumeVerb)

-- Raw payloads for volume points
volumePRs = [volumePR0, volumePR1, volumePR2 ]
ssdPRs = volumePRs

volumePR0, volumePR1, volumePR2 :: Word64
volumePR0 = 2 + (1 `shift` 8) + (2 `shift` 16) + (10 `shift` 32)
volumePR1 = 2 + (1 `shift` 8) + (2 `shift` 16) + (30 `shift` 32)
volumePR2 = 4 + (3 `shift` 8) + (2 `shift` 16) + (30 `shift` 32)

-- Decoded payloads for volume points
volumePDs = [volumePD0, volumePD1, volumePD2 ]

volumePD0, volumePD1, volumePD2 :: PDVolume
volumePD0 = PDVolume VolumeCreating VolumeCreate End 10
volumePD1 = PDVolume VolumeCreating VolumeCreate End 30
volumePD2 = PDVolume VolumeDeleting VolumeDelete End 30

ssdPDs = [ssdPD0, ssdPD1, ssdPD2 ]

ssdPD0, ssdPD1, ssdPD2 :: PDSSD
ssdPD0 = PDSSD VolumeCreating VolumeCreate End 10
ssdPD1 = PDSSD VolumeCreating VolumeCreate End 30
ssdPD2 = PDSSD VolumeDeleting VolumeDelete End 30

volumeTimedPDs :: [Timed PDVolume]
volumeTimedPDs = [ Timed testS        volumePD0
                 , Timed (testS + 2)  volumePD1
                 , Timed (testS + 7)  volumePD1
                 , Timed (testS + 11) volumePD0
                 , Timed (testS + 21) volumePD2 ]

ssdTimedPDs :: [Timed PDSSD]
ssdTimedPDs = [ Timed testS        ssdPD0
              , Timed (testS + 2)  ssdPD1
              , Timed (testS + 7)  ssdPD1
              , Timed (testS + 11) ssdPD0
              , Timed (testS + 21) ssdPD2 ]

-- Volume events
-- Expected = 2 * 10 + 5 * 30 + 4 * 30 + 10 * 10
--          = 20 + 150 + 120 + 100
--          = 390
-- from borel-core
--
volumeTimedPDsResult :: Word64
volumeTimedPDsResult = 390

ssdTimedPDsResult :: Word64
ssdTimedPDsResult = volumeTimedPDsResult
