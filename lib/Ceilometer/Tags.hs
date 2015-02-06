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

keyMetricName = pack "metric_name"
keyTenancyID  = pack "project_id"
keyResourceID = pack "resource_id"
keyEvent      = pack "_event"

valTrue       = pack "1"
valFalse      = pack "0"
