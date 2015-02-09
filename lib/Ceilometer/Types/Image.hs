--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines the Ceilometer Image type.
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ceilometer.Types.Image
  ( -- * Fields
    PFImageStatus(..), pfImageStatus
  , PFImageVerb(..), pfImageVerb
  , PDImage(..), pdImage
  , PDImageP(..), pdImageP
  , imageStatus, imageVerb, imageVal
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Binary           (Word8)
import           Data.Text             (Text)
import           Data.Typeable

import           Ceilometer.Types.Base
import           Ceilometer.Types.TH
import           Vaultaire.Types

$(declarePF    "Image"
              ("Status", ''Word8)
            [ ("Active"       , 1)
            , ("Saving"       , 2)
            , ("Deleted"      , 3)
            , ("Queued"       , 4)
            , ("PendingDelete", 5)
            , ("Killed"       , 6) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

$(declarePF    "Image"
              ("Verb", ''Word8)
            [ ("Serve"   , 1)
            , ("Update"  , 2)
            , ("Upload"  , 3)
            , ("Download", 4)
            , ("Delete"  , 5) ]
            [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])

data PDImage = PDImage
  { _imageStatus   :: PFImageStatus
  , _imageVerb     :: PFImageVerb
  , _imageVal      :: PFValue32 }
  deriving (Eq, Show, Read, Typeable)

$(makeLenses ''PDImage)

newtype PDImageP = PDImageP { _pdImagePVal :: PFValue64 }
     deriving (Show, Read, Eq, Typeable)

pdImageP :: Iso' PRSimple PDImageP
pdImageP = iso (PDImageP . _prSimpleVal) (PRSimple . _pdImagePVal)

pdImage :: Prism' PRCompoundEvent PDImage
pdImage = prism' pretty parse
  where parse raw = do
          s <- raw ^? eventStatus   . pfImageStatus
          v <- raw ^? eventVerb     . pfImageVerb
          _ <- raw ^? eventEndpoint . pfEndpoint ^? only (Just Instant)
          x <- raw ^? eventVal
          Just $ PDImage s v x
        pretty (PDImage status verb val)
          = PRCompoundEvent
            val
            (Instant ^. re pfEndpoint)
            (verb    ^. re pfImageVerb)
            (status  ^. re pfImageStatus)
