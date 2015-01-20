module Ceilometer.Types.Volume where

import           Data.Bimap (Bimap)
import qualified Data.Bimap as B
import           Data.Word

-- Volumes
data VolumeStatus = VolumeError
                  | VolumeAvailable
                  | VolumeCreating
                  | VolumeExtending
                  | VolumeDeleting
                  | VolumeAttaching
                  | VolumeDetaching
                  | VolumeInUse
                  | VolumeRetyping
                  | VolumeUploading
  deriving (Show, Eq, Ord)

data VolumeVerb
