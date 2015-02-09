--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer IP type.
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ceilometer.Types.IP
  ( -- * Fields
    PFIPStatus(..), pfIPStatus
  , PFIPVerb(..), pfIPVerb
  , PFIPAlloc(..), pfIPAlloc
  , PDIP(..), pdIP
  , ipStatus, ipVerb, ipEndpoint, ipVal
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Binary           (Word8, Word32)
import           Data.Text             (Text)
import           Data.Typeable

import           Ceilometer.Types.Base
import           Ceilometer.Types.TH
import           Vaultaire.Types


$(declarePF    "IP"
              ("Status", ''Word8)
            [ ("None"   , 0)
            , ("Active" , 1)
            , ("Down"   , 2) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

$(declarePF    "IP"
              ("Verb", ''Word8)
            [ ("Create" , 1)
            , ("Update" , 2)
            , ("Delete" , 3) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

$(declarePF    "IP"
              ("Alloc", ''Word32)
            [ ("Alloc", 1) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum, ''Ord ])


data PDIP = PDIP
  { _ipStatus   :: PFIPStatus
  , _ipVerb     :: PFIPVerb
  , _ipEndpoint :: PFEndpoint
  , _ipVal      :: PFIPAlloc
  }
  deriving (Eq, Show, Read, Typeable)

$(makeLenses ''PDIP)

pdIP :: Prism' PRCompoundEvent PDIP
pdIP = prism' pretty parse
  where parse raw
          =   PDIP 
          <$> (raw ^? eventStatus   . pfIPStatus)
          <*> (raw ^? eventVerb     . pfIPVerb)
          <*> (raw ^? eventEndpoint . pfEndpoint)
          <*> (raw ^? eventVal      . pfIPAlloc)
        pretty (PDIP status verb ep x)
          = PRCompoundEvent
            (x       ^. re pfIPAlloc)
            (ep      ^. re pfEndpoint)
            (verb    ^. re pfIPVerb)
            (status  ^. re pfIPStatus)
