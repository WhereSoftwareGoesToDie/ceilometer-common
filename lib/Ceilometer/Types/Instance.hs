--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer Instance-related types.
--
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Ceilometer.Types.Instance
  ( PFInstanceStatus(..), pfInstanceStatus
  , PDInstanceVCPU(..), pdInstanceVCPU
  , PDInstanceRAM(..), pdInstanceRAM
  , PDInstanceDisk(..), pdInstanceDisk
  , PDInstanceFlavor(..), pdInstanceFlavor
  , siphashID, isInstanceStatusBillable
  , isInstanceFlavorBillable, isInstanceVCPUBillable
  , isInstanceRAMBillable, isInstanceDiskBillable
  ) where

import           Control.Applicative
import           Control.Lens
import           Crypto.MAC.SipHash
import qualified Data.Bimap            as BM
import           Data.Binary           (Word32, Word64, Word8)
import           Data.Bits
import           Data.ByteString       (ByteString)
import qualified Data.Text.Encoding    as T

import           Ceilometer.Types.Base
import           Ceilometer.Types.TH

-- These instance metrics are the same but reported separately and differently.

$(declarePF    "Instance"
              ("Status", ''Word8)
            [ ("Error",            0 )
            , ("Active",           1 )
            , ("Shutoff",          2 )
            , ("Build",            3 )
            , ("Rebuild",          4 )
            , ("Deleted",          5 )
            , ("SoftDeleted",      6 )
            , ("Shelved",          7 )
            , ("ShelvedOffloaded", 8 )
            , ("Reboot",           9 )
            , ("HardReboot",       10)
            , ("Password",         11)
            , ("Resize",           12)
            , ("VerifyResize",     13)
            , ("RevertResize",     14)
            , ("Paused",           15)
            , ("Suspended",        16)
            , ("Rescue",           17)
            , ("Migrating",        18) ]
            [''Show, ''Read, ''Eq, ''Enum, ''Bounded] )

-- BOILERPLATE GALORE

isInstanceFlavorBillable :: PDInstanceFlavor -> Bool
isInstanceVCPUBillable   :: PDInstanceVCPU   -> Bool
isInstanceRAMBillable    :: PDInstanceRAM    -> Bool
isInstanceDiskBillable   :: PDInstanceDisk   -> Bool
isInstanceFlavorBillable (PDInstanceFlavor status _) = isInstanceStatusBillable status
isInstanceVCPUBillable   (PDInstanceVCPU   status _) = isInstanceStatusBillable status
isInstanceRAMBillable    (PDInstanceRAM    status _) = isInstanceStatusBillable status
isInstanceDiskBillable   (PDInstanceDisk   status _) = isInstanceStatusBillable status

isInstanceStatusBillable :: PFInstanceStatus -> Bool
isInstanceStatusBillable InstanceError            = False
isInstanceStatusBillable InstanceActive           = True
isInstanceStatusBillable InstanceShutoff          = False
isInstanceStatusBillable InstanceBuild            = False
isInstanceStatusBillable InstanceRebuild          = False
isInstanceStatusBillable InstanceDeleted          = False
isInstanceStatusBillable InstanceSoftDeleted      = False
isInstanceStatusBillable InstanceShelved          = False
isInstanceStatusBillable InstanceShelvedOffloaded = False
isInstanceStatusBillable InstanceReboot           = True
isInstanceStatusBillable InstanceHardReboot       = True
isInstanceStatusBillable InstancePassword         = True
isInstanceStatusBillable InstanceResize           = True
isInstanceStatusBillable InstanceVerifyResize     = True
isInstanceStatusBillable InstanceRevertResize     = True
isInstanceStatusBillable InstancePaused           = False
isInstanceStatusBillable InstanceSuspended        = False
isInstanceStatusBillable InstanceRescue           = False
isInstanceStatusBillable InstanceMigrating        = False

data PDInstanceVCPU = PDInstanceVCPU PFInstanceStatus PFValue32
     deriving (Eq, Show, Read)

pdInstanceVCPU :: Prism' PRCompoundPollster PDInstanceVCPU
pdInstanceVCPU = prism' pretty parse
  where parse raw
          =   PDInstanceVCPU
          <$> (raw ^? pollsterStatus . pfInstanceStatus)
          <*> (raw ^? pollsterVal )
        pretty (PDInstanceVCPU status val)
          = PRCompoundPollster
            val
            (status ^. re pfInstanceStatus)

data PDInstanceRAM = PDInstanceRAM PFInstanceStatus PFValue32
     deriving (Eq, Show, Read)

pdInstanceRAM :: Prism' PRCompoundPollster PDInstanceRAM
pdInstanceRAM = prism' pretty parse
  where parse raw
          =   PDInstanceRAM
          <$> (raw ^? pollsterStatus . pfInstanceStatus)
          <*> (raw ^? pollsterVal )
        pretty (PDInstanceRAM status val)
          = PRCompoundPollster
            val
            (status ^. re pfInstanceStatus)

data PDInstanceDisk = PDInstanceDisk PFInstanceStatus PFValue32
     deriving (Eq, Show, Read)

pdInstanceDisk :: Prism' PRCompoundPollster PDInstanceDisk
pdInstanceDisk = prism' pretty parse
  where parse raw
          =   PDInstanceDisk
          <$> (raw ^? pollsterStatus . pfInstanceStatus)
          <*> (raw ^? pollsterVal )
        pretty (PDInstanceDisk status val)
          = PRCompoundPollster
            val
            (status ^. re pfInstanceStatus)

data PDInstanceFlavor = PDInstanceFlavor PFInstanceStatus PFValueText
     deriving (Eq, Show, Read)

pdInstanceFlavor :: FlavorMap -> Prism' PRCompoundPollster PDInstanceFlavor
pdInstanceFlavor fm = prism' pretty parse
  where parse raw
          =   PDInstanceFlavor
          <$> (raw ^? pollsterStatus . pfInstanceStatus)
          <*> (raw ^? pollsterVal >>= flip BM.lookupR fm)
        pretty (PDInstanceFlavor status val)
          = PRCompoundPollster
            (siphashID val)
            (status ^. re pfInstanceStatus)


-- FROM COLLECTOR CODE

-- | Canonical siphash with key = 0
siphash :: ByteString -> Word64
siphash x = let (SipHash h) = hash (SipKey 0 0) x in h

-- | Canonical siphash with key = 0, truncated to 32 bits
siphash32 :: ByteString -> Word32
siphash32 = fromIntegral . (`shift` (-32)) . siphash

siphashID :: PFValueText -> Word32
siphashID = siphash32 . T.encodeUtf8
