--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer CPU type.

{-# LANGUAGE DeriveDataTypeable #-}
module Ceilometer.Types.CPU
  ( PDCPU(..), pdCPU
  ) where

import           Control.Lens
import           Data.Typeable

import           Ceilometer.Types.Base

newtype PDCPU = PDCPU { _pdCPUVal :: PFValue64 }
     deriving (Show, Read, Eq, Typeable)

pdCPU :: Iso' PRSimple PDCPU
pdCPU = iso (PDCPU . _prSimpleVal) (PRSimple . _pdCPUVal)
