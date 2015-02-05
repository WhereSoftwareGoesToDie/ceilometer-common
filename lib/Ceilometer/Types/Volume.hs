--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer Volume type.
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ceilometer.Types.Volume
  ( -- * Fields
    PFVolumeStatus(..), pfVolumeStatus
  , PFVolumeVerb(..), pfVolumeVerb
  , PDVolume(..), pdVolume
  , PDSSD(..), pdSSD
  , volumeStatus, volumeVerb, volumeEndpoint, volumeVal
  , volumeTypeBlockId, volumeTypeFastId
  , lookupVolumeType, sourceIsBlock, sourceIsFast
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Binary           (Word8)
import           Data.Text             (Text)
import           Data.Typeable

import           Ceilometer.Types.Base
import           Ceilometer.Types.TH
import           Vaultaire.Types

volumeTypeBlockId, volumeTypeFastId :: Text
volumeTypeBlockId = "7a522201-7c27-4eaa-9d95-d70cfaaeb16a"
volumeTypeFastId  = "f7797fba-2ce2-4d19-a607-29f4bc2acb3f"

lookupVolumeType :: SourceDict -> Maybe Text
lookupVolumeType = lookupSource "volume_type"

sourceIsBlock, sourceIsFast :: SourceDict -> Bool
sourceIsBlock sd = case lookupVolumeType sd of
  Nothing                      -> True
  Just x  -> if
    | x == volumeTypeBlockId -> True
    | otherwise              -> False
sourceIsFast  sd = case lookupVolumeType sd of
  Nothing                      -> False
  Just x -> if
    | x == volumeTypeFastId  -> True
    | otherwise              -> False

$(declarePF    "Volume"
              ("Status", ''Word8)
            [ ("Error",     0)
            , ("Available", 1)
            , ("Creating",  2)
            , ("Extending", 3)
            , ("Deleting",  4)
            , ("Attaching", 5)
            , ("Detaching", 6)
            , ("InUse",     7)
            , ("Retyping",  8)
            , ("Uploading", 9) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

$(declarePF    "Volume"
              ("Verb", ''Word8)
            [ ("Create", 1)
            , ("Resize", 2)
            , ("Delete", 3)
            , ("Attach", 4)
            , ("Detach", 5)
            , ("Update", 6) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

data PDVolume = PDVolume
  { _volumeStatus   :: PFVolumeStatus
  , _volumeVerb     :: PFVolumeVerb
  , _volumeEndpoint :: PFEndpoint
  , _volumeVal      :: PFValue32 }
  deriving (Eq, Show, Read, Typeable)

data PDSSD = PDSSD
  { _ssdStatus   :: PFVolumeStatus
  , _ssdVerb     :: PFVolumeVerb
  , _ssdEndpoint :: PFEndpoint
  , _ssdVal      :: PFValue32 }
  deriving (Eq, Show, Read, Typeable)

$(makeLenses ''PDVolume)
$(makeLenses ''PDSSD)

pdVolume :: Prism' PRCompoundEvent PDVolume
pdVolume = prism' pretty parse
  where parse raw
          =   PDVolume
          <$> (raw ^? eventStatus   . pfVolumeStatus)
          <*> (raw ^? eventVerb     . pfVolumeVerb)
          <*> (raw ^? eventEndpoint . pfEndpoint)
          <*> (raw ^? eventVal )
        pretty (PDVolume status verb ep val)
          = PRCompoundEvent
            val
            (ep     ^. re pfEndpoint)
            (verb   ^. re pfVolumeVerb)
            (status ^. re pfVolumeStatus)

pdSSD :: Prism' PRCompoundEvent PDSSD
pdSSD = prism' pretty parse
  where parse raw
          =   PDSSD
          <$> (raw ^? eventStatus   . pfVolumeStatus)
          <*> (raw ^? eventVerb     . pfVolumeVerb)
          <*> (raw ^? eventEndpoint . pfEndpoint)
          <*> (raw ^? eventVal )
        pretty (PDSSD status verb ep val)
          = PRCompoundEvent
            val
            (ep     ^. re pfEndpoint)
            (verb   ^. re pfVolumeVerb)
            (status ^. re pfVolumeStatus)
