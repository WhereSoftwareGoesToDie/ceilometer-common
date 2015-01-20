{-# LANGUAGE OverloadedStrings #-}

module Ceilometer.Types where

import           Control.Isomorphism.Partial.Iso
import           Data.Bimap                      (Bimap)
import qualified Data.Bimap                      as B
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as H
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Word

import           Marquise.Client
import           Vaultaire.Types

import           Ceilometer.Types.Base
import           Ceilometer.Types.IP
import           Ceilometer.Types.Volume

-- Snapshots
data SnapshotStatus
data SnapshotVerb

-- Images
data ImageStatus
data ImageVerb

-- Instances
--
newtype Flavor = Flavor String
  deriving (Show, Eq, Ord)

type FlavorMap = Bimap Flavor Word64

data InstanceStatus
data InstanceVerb

data InstancePayload = InstanceFlavor Flavor
                     | InstanceRam    Word64
                     | InstanceDisk   Word64
                     | InstanceVCpu   Word64
  deriving (Show, Eq, Ord)


data CeilometerPoint
  = InstancePoint       Address TimeStamp InstanceStatus                       InstancePayload
  | IPEventPoint        Address TimeStamp IPStatus       IPVerb       Endpoint
  | VolumeEventPoint    Address TimeStamp VolumeStatus   VolumeVerb   Endpoint Word64
  | SnapshotEventPoint  Address TimeStamp SnapshotStatus SnapshotVerb Endpoint Word64
  | ImageEventPoint     Address TimeStamp ImageStatus    ImageVerb             Word64
  | StandardPoint       Address TimeStamp                                      Word64

type VaultIso = Iso SimplePoint

isEvent :: SourceDict -> Bool
isEvent sd = case lookupSource "_event" sd of
    Just "1" -> True
    Just "0" -> False
    Nothing  -> False

metricName :: SourceDict -> Maybe Text
metricName = lookupSource "metric_name"

ceilometerIso :: FlavorMap -> SourceDict -> Iso SimplePoint CeilometerPoint
ceilometerIso fm sd = unsafeMakeIso apply unapply
  where
    apply (SimplePoint addr ts v) = case (metricName sd, isEvent sd) of
        (Nothing, _) -> Nothing
        (Just "disk.write.bytes", False) -> Just $ StandardPoint addr ts v
    unapply = undefined
