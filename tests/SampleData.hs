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
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Word
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Ceilometer.Types
import           Ceilometer.Types.Image    (imageVerb)
import           Ceilometer.Types.Instance (siphashID)
import           Ceilometer.Types.IP       (ipVerb)
import           Ceilometer.Types.Snapshot (snapshotVerb)
import           Ceilometer.Types.Volume   (volumeVerb)


-- Instances (DO NOT DEFNE NEW ONES IN TESTS) ----------------------------------

instance Function PFEndpoint       where function = functionShow
instance Function PFVolumeStatus   where function = functionShow
instance Function PFVolumeVerb     where function = functionShow
instance Function PFImageStatus    where function = functionShow
instance Function PFImageVerb      where function = functionShow
instance Function PFSnapshotStatus where function = functionShow
instance Function PFSnapshotVerb   where function = functionShow
instance Function PFIPStatus       where function = functionShow
instance Function PFIPVerb         where function = functionShow
instance Function PDCPU            where function = functionShow
instance Function PDVolume         where function = functionShow
instance Function PDSSD            where function = functionShow
instance Function PDImage          where function = functionShow
instance Function PDImagePollster  where function = functionShow
instance Function PDIP             where function = functionShow
instance Function PDSnapshot       where function = functionShow
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
instance Arbitrary PFImageVerb      where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFImageStatus    where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFSnapshotVerb   where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFSnapshotStatus where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFIPVerb         where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFIPStatus       where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFIPAlloc        where arbitrary = arbitraryBoundedEnum
instance Arbitrary PFValueText      where arbitrary = elements $ BM.keys testFlavors

instance Arbitrary PDCPU            where arbitrary =   PDCPU <$> arbitrary
instance Arbitrary PDVolume         where arbitrary =   PDVolume
                                                  <$> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDSSD            where arbitrary =   PDSSD
                                                  <$> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceVCPU   where arbitrary =   PDInstanceVCPU
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceRAM    where arbitrary =   PDInstanceRAM
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceDisk   where arbitrary =   PDInstanceDisk
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDInstanceFlavor where arbitrary =   PDInstanceFlavor
                                                  <$> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDImage          where arbitrary =   PDImage
                                                  <$> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDImagePollster  where arbitrary =   PDImagePollster <$> arbitrary
instance Arbitrary PDSnapshot       where arbitrary =   PDSnapshot
                                                  <$> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
instance Arbitrary PDIP             where arbitrary =   PDIP
                                                  <$> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary
                                                  <*> arbitrary

instance CoArbitrary PFEndpoint       where coarbitrary = variant . fromEnum
instance CoArbitrary PFVolumeStatus   where coarbitrary = variant . fromEnum
instance CoArbitrary PFVolumeVerb     where coarbitrary = variant . fromEnum
instance CoArbitrary PFImageVerb      where coarbitrary = variant . fromEnum
instance CoArbitrary PFImageStatus    where coarbitrary = variant . fromEnum
instance CoArbitrary PFSnapshotVerb   where coarbitrary = variant . fromEnum
instance CoArbitrary PFSnapshotStatus where coarbitrary = variant . fromEnum
instance CoArbitrary PFIPVerb         where coarbitrary = variant . fromEnum
instance CoArbitrary PFIPStatus       where coarbitrary = variant . fromEnum

instance CoArbitrary PDCPU            where
  coarbitrary x = variant $ x ^. re pdCPU      . re prSimple
instance CoArbitrary PDVolume         where
  coarbitrary x = variant $ x ^. re pdVolume   . re prCompoundEvent
instance CoArbitrary PDSSD            where
  coarbitrary x = variant $ x ^. re pdSSD      . re prCompoundEvent
instance CoArbitrary PDImage          where
  coarbitrary x = variant $ x ^. re pdImage    . re prCompoundEvent
instance CoArbitrary PDImagePollster  where
  coarbitrary x = variant $ x ^. re pdImagePollster   . re prSimple
instance CoArbitrary PDSnapshot       where
  coarbitrary x = variant $ x ^. re pdSnapshot . re prCompoundEvent
instance CoArbitrary PDIP             where
  coarbitrary x = variant $ x ^. re pdIP       . re prCompoundEvent
instance CoArbitrary PDInstanceFlavor where
  coarbitrary x = variant $ x ^. re (pdInstanceFlavor testFlavors) . re prCompoundPollster
instance CoArbitrary PDInstanceVCPU   where
  coarbitrary x = variant $ x ^. re pdInstanceVCPU . re prCompoundPollster
instance CoArbitrary PDInstanceRAM    where
  coarbitrary x = variant $ x ^. re pdInstanceRAM  . re prCompoundPollster
instance CoArbitrary PDInstanceDisk   where
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
                 , Timed (testS + 2) flavorPD1
                 , Timed (testS + 7) flavorPD2
                 , Timed (testS + 11) flavorPD1
                 , Timed (testS + 13) flavorPD1 ]

flavorTimedPDsResult :: [(PFValue PDInstanceFlavor, Word64)]
flavorTimedPDsResult = [ (flavorID1, 9)
                       , (flavorID2, 4) ]

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
--          = (2 + 10) * 10 + (5 + 4) * 30
--          = 12 * 10 + 9 * 30
--          = 390
-- from borel-core
--
volumeTimedPDsResult :: Word64
volumeTimedPDsResult = 390

ssdTimedPDsResult :: Word64
ssdTimedPDsResult = volumeTimedPDsResult

-- IMAGE ----------------------------------------------------------------------

imageNonDelete :: Gen PDImage
imageNonDelete = (arbitrary :: Gen PDImage) `suchThat` ((/= ImageDelete) . view imageVerb)

-- Raw payloads for volume points
imagePRs = [imagePR0, imagePR1, imagePR2 ]

imagePR0, imagePR1, imagePR2 :: Word64
imagePR0 = 1 + (3 `shift` 8) + (0 `shift` 16) + (200000 `shift` 32)
imagePR1 = 1 + (1 `shift` 8) + (0 `shift` 16) + (400000 `shift` 32)
imagePR2 = 3 + (5 `shift` 8) + (0 `shift` 16) + (100000 `shift` 32)

-- Decoded payloads for image points
imagePDs = [imagePD0, imagePD1, imagePD2 ]

imagePD0, imagePD1, imagePD2 :: PDImage
imagePD0 = PDImage ImageActive  ImageUpload Instant 200000
imagePD1 = PDImage ImageActive  ImageServe  Instant 400000
imagePD2 = PDImage ImageDeleted ImageDelete Instant 100000

imageTimedPDs :: [Timed PDImage]
imageTimedPDs = [ Timed testS        imagePD0
                , Timed (testS + 5)  imagePD1
                , Timed (testS + 12) imagePD1
                , Timed (testS + 21) imagePD0
                , Timed (testS + 32) imagePD2 ]


-- Image events
-- Expected = 5  * 200000
--          + 7  * 400000
--          + 9  * 400000
--          + 11 * 200000
--          = 16 * 200000
--          + 16 * 400000
--          = 9600000
--
imageTimedPDsResult :: Word64
imageTimedPDsResult = 9600000

imagePPD0, imagePPD1, imagePPD2 :: PDImagePollster
imagePPD0 = PDImagePollster 200000
imagePPD1 = PDImagePollster 400000
imagePPD2 = PDImagePollster 100000

imagePTimedPDs :: [Timed PDImagePollster]
imagePTimedPDs = [ Timed testS        imagePPD0
                 , Timed (testS + 5)  imagePPD1
                 , Timed (testS + 12) imagePPD2
                 , Timed (testS + 21) imagePPD1
                 , Timed (testS + 32) imagePPD2 ]

imagePTimedPDsResult :: Map Word64 Word64
imagePTimedPDsResult = M.fromList [ (200000, 5)
                                  , (400000, 18)
                                  , (100000, 9)
                                  ]

-- SNAPSHOT ----------------------------------------------------------------------

snapshotNonDelete :: Gen PDSnapshot
snapshotNonDelete = (arbitrary :: Gen PDSnapshot) `suchThat` ((/= SnapshotDelete) . view snapshotVerb)

-- Raw payloads for volume points
snapshotPRs = [snapshotPR0, snapshotPR1, snapshotPR2 ]

snapshotPR0, snapshotPR1, snapshotPR2 :: Word64
snapshotPR0 = 1 + (2 `shift` 8) + (2 `shift` 16) + (200 `shift` 32)
snapshotPR1 = 1 + (2 `shift` 8) + (2 `shift` 16) + (300 `shift` 32)
snapshotPR2 = 3 + (3 `shift` 8) + (2 `shift` 16) + (100 `shift` 32)

-- Decoded payloads for snapshot points
snapshotPDs = [snapshotPD0, snapshotPD1, snapshotPD2 ]

snapshotPD0, snapshotPD1, snapshotPD2 :: PDSnapshot
snapshotPD0 = PDSnapshot SnapshotAvailable SnapshotUpdate End 200
snapshotPD1 = PDSnapshot SnapshotAvailable SnapshotUpdate End 300
snapshotPD2 = PDSnapshot SnapshotDeleting  SnapshotDelete End 100

snapshotTimedPDs :: [Timed PDSnapshot]
snapshotTimedPDs = [ Timed testS        snapshotPD0
                , Timed (testS + 5)  snapshotPD1
                , Timed (testS + 12) snapshotPD1
                , Timed (testS + 21) snapshotPD0
                , Timed (testS + 32) snapshotPD2 ]


-- Snapshot events
-- Expected = 5  * 200
--          + 7  * 300
--          + 9  * 300
--          + 11 * 200
--          = 16 * 200
--          + 16 * 300
--          = 8000
--
snapshotTimedPDsResult :: Word64
snapshotTimedPDsResult = 8000

-- IP ----------------------------------------------------------------------

-- Raw payloads for volume points
ipPRs = [ipPR0, ipPR1, ipPR2 ]

ipPR0, ipPR1, ipPR2 :: Word64
ipPR0 = 1 + (2 `shift` 8) + (2 `shift` 16) + (1 `shift` 32)
ipPR1 = 2 + (2 `shift` 8) + (2 `shift` 16) + (1 `shift` 32)
ipPR2 = 0 + (3 `shift` 8) + (2 `shift` 16) + (1 `shift` 32)

-- Decoded payloads for ip points
ipPDs = [ipPD0, ipPD1, ipPD2 ]

ipPD0, ipPD1, ipPD2 :: PDIP
ipPD0 = PDIP IPActive IPUpdate End IPAlloc
ipPD1 = PDIP IPDown   IPUpdate End IPAlloc
ipPD2 = PDIP IPNone   IPDelete End IPAlloc
