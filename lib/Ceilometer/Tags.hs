-- | Copyright 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the hard-coded tags and values used by the
-- Ceilometer collector as well as clients of Ceilometer.
--
{-# OPTIONS -fno-warn-missing-signatures #-}

module Ceilometer.Tags where

import Data.Text
import Vaultaire.Types

keyMetricName = pack "metric_name"
keyTenancyID  = pack "project_id"
keyResourceID = pack "resource_id"
keyEvent      = pack "_event"
keyVolumeType = pack "volume_type"

valTrue       = pack "1"
valFalse      = pack "0"

volumeTypeBlockId = pack "7a522201-7c27-4eaa-9d95-d70cfaaeb16a"
volumeTypeFastId  = pack "f7797fba-2ce2-4d19-a607-29f4bc2acb3f"

lookupMetricName = lookupSource keyMetricName
lookupEvent      = lookupSource keyEvent
lookupVolumeType = lookupSource keyVolumeType

sourceIsBlock, sourceIsFast :: SourceDict -> Bool
sourceIsBlock sd
  | Just v <- lookupVolumeType sd, v == volumeTypeBlockId = True
  | otherwise                                             = False
sourceIsFast sd
  | Just v <- lookupVolumeType sd, v == volumeTypeFastId = True
  | otherwise                                            = False
