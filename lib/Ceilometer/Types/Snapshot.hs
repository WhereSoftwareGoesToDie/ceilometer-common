--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer Snapshot type.
--
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ceilometer.Types.Snapshot
  ( -- * Fields
    PFSnapshotStatus(..), pfSnapshotStatus
  , PFSnapshotVerb(..), pfSnapshotVerb
  , PDSnapshot(..), pdSnapshot
  , snapshotStatus, snapshotVerb, snapshotEndpoint, snapshotVal
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Binary           (Word8)
import           Data.Text             (Text)

import           Ceilometer.Types.Base
import           Ceilometer.Types.TH
import           Vaultaire.Types

$(declarePF    "Snapshot"
              ("Status", ''Word8)
            [ ("Error",     0)
            , ("Available", 1)
            , ("Creating",  2)
            , ("Deleting",  3) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

$(declarePF    "Snapshot"
              ("Verb", ''Word8)
            [ ("Create", 1)
            , ("Update", 2)
            , ("Delete", 3) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

data PDSnapshot = PDSnapshot
  { _snapshotStatus   :: PFSnapshotStatus
  , _snapshotVerb     :: PFSnapshotVerb
  , _snapshotEndpoint :: PFEndpoint
  , _snapshotVal      :: PFValue32 }
  deriving (Eq, Show, Read)

$(makeLenses ''PDSnapshot)

pdSnapshot :: Prism' PRCompoundEvent PDSnapshot
pdSnapshot = prism' pretty parse
  where parse raw
          =   PDSnapshot
          <$> (raw ^? eventStatus   . pfSnapshotStatus)
          <*> (raw ^? eventVerb     . pfSnapshotVerb)
          <*> (raw ^? eventEndpoint . pfEndpoint)
          <*> (raw ^? eventVal )
        pretty (PDSnapshot status verb ep val)
          = PRCompoundEvent
            val
            (ep     ^. re pfEndpoint)
            (verb   ^. re pfSnapshotVerb)
            (status ^. re pfSnapshotStatus)
