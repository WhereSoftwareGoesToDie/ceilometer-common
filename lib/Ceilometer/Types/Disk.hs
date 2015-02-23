--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer Disk Traffic type.

module Ceilometer.Types.Disk
  ( PDDiskRead(..), pdDiskRead
  , PDDiskWrite(..), pdDiskWrite
  ) where

import           Control.Lens

import           Ceilometer.Types.Base

newtype PDDiskRead = PDDiskRead { _pdDiskReadVal :: PFValue64 }
    deriving (Show, Read, Eq)

newtype PDDiskWrite = PDDiskWrite { _pdDiskWriteVal :: PFValue64 }
    deriving (Show, Read, Eq)

pdDiskRead :: Iso' PRSimple PDDiskRead
pdDiskRead = iso (PDDiskRead . _prSimpleVal) (PRSimple . _pdDiskReadVal)

pdDiskWrite :: Iso' PRSimple PDDiskWrite
pdDiskWrite = iso (PDDiskWrite . _prSimpleVal) (PRSimple . _pdDiskWriteVal)
