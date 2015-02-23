--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer Neutron Traffic type.

module Ceilometer.Types.Neutron
  ( PDNeutronTx(..), pdNeutronTx
  , PDNeutronRx(..), pdNeutronRx
  ) where

import           Control.Lens

import           Ceilometer.Types.Base

newtype PDNeutronTx = PDNeutronTx { _pdNeutronTxVal :: PFValue64 }
    deriving (Show, Read, Eq)

newtype PDNeutronRx = PDNeutronRx { _pdNeutronRxVal :: PFValue64 }
    deriving (Show, Read, Eq)

pdNeutronTx :: Iso' PRSimple PDNeutronTx
pdNeutronTx = iso (PDNeutronTx . _prSimpleVal) (PRSimple . _pdNeutronTxVal)

pdNeutronRx :: Iso' PRSimple PDNeutronRx
pdNeutronRx = iso (PDNeutronRx . _prSimpleVal) (PRSimple . _pdNeutronRxVal)
