--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- Common Ceilometer field types.
--
{-# LANGUAGE TemplateHaskell #-}

module Ceilometer.Types.Base
  ( -- * Payload Raws
    PRSimple(..), prSimple
  , PRCompoundEvent(..), prCompoundEvent
  , eventVal, eventEndpoint, eventVerb, eventStatus
  , PRCompoundPollster(..), prCompoundPollster
  , pollsterVal, pollsterStatus

    -- * Decoded Payload Fields (common)
  , PFEndpoint(..), pfEndpoint
  , PFValue64, PFValue32, PFValueString, PFValueText

    -- * Instance Flavour Config
  , Flavor, FlavorMap
  ) where

import           Control.Lens        hiding (Fold, Simple)
import           Data.Bimap          (Bimap)
import           Data.Binary         (Word32, Word64, Word8)
import qualified Data.Binary         as B
import qualified Data.Binary.Get     as B
import qualified Data.Binary.Put     as B
import           Data.Text           (Text)

import           Ceilometer.Types.TH

type Flavor    = Text
type FlavorMap = Bimap Flavor Word32

-- Raw Payload Types -----------------------------------------------------------

-- | Payload Raw Simple
newtype PRSimple = PRSimple { _prSimpleVal :: Word64 }
  deriving (Show, Read, Eq)

prSimple :: Iso' Word64 PRSimple
prSimple = iso PRSimple _prSimpleVal

-- | Payload Raw Compound Event
--
data PRCompoundEvent = PRCompoundEvent
  { _eventVal      :: {-# UNPACK #-} !Word32 -- ^ Payload
  , _eventEndpoint :: {-# UNPACK #-} !Word8  -- ^ PFEndpoint
  , _eventVerb     :: {-# UNPACK #-} !Word8  -- ^ Verb
  , _eventStatus   :: {-# UNPACK #-} !Word8  -- ^ Status
  } deriving (Show, Eq)

makeLenses ''PRCompoundEvent

-- | An "improper" iso to convert between a Word64 and the raw compound.
--
--   It is improper because the reserved 8 bytes are ignored on parsing/printing.
--   So it doesn't satisfy a "roundtrip" between the serialised Word64 and the
--   decoded Compound raw. This is fine since we don't care about the reserved bytes.
--
prCompoundEvent :: Iso' Word64 PRCompoundEvent
prCompoundEvent = iso getIt putIt
  where getIt bytes = flip B.runGet (B.encode bytes) $ do
          a <- B.getWord32be
          _ <- B.getWord8 -- reserved bytes
          c <- B.getWord8
          d <- B.getWord8
          e <- B.getWord8
          return $ PRCompoundEvent a c d e
        putIt x = B.decode $ B.runPut $ do
          B.putWord32be $ x ^. eventVal
          B.putWord8      0
          B.putWord8    $ x ^. eventEndpoint
          B.putWord8    $ x ^. eventVerb
          B.putWord8    $ x ^. eventStatus

data PRCompoundPollster = PRCompoundPollster
  { _pollsterVal    :: {-# UNPACK #-} !Word32 -- ^ Payload
  , _pollsterStatus :: {-# UNPACK #-} !Word8
  } deriving (Eq, Show)

makeLenses ''PRCompoundPollster

-- | An "improper" iso to convert between a Word64 and the raw compound.
--
--   It is improper because we only preserve the value and status in a "roundtrip".
--   This is fine for pollster since that's all we care about.
--
prCompoundPollster :: Iso' Word64 PRCompoundPollster
prCompoundPollster = iso getIt putIt
  where getIt bytes = flip B.runGet (B.encode bytes) $ do
          a <- B.getWord32be
          _ <- B.getWord8 -- reserved bytes
          _ <- B.getWord8 -- endpoint
          _ <- B.getWord8 -- verb
          e <- B.getWord8
          return $ PRCompoundPollster a e
        putIt x = B.decode $ B.runPut $ do
          B.putWord32be $ x ^. pollsterVal
          B.putWord8      0
          B.putWord8      0
          B.putWord8      0
          B.putWord8    $ x ^. pollsterStatus


-- Decoded Payload Fields ------------------------------------------------------

type PFValue64 = Word64
type PFValue32 = Word32
type PFValueString = String
type PFValueText   = Text

$(declarePF "" ("Endpoint", ''Word8)
             [ ("Instant", 0)
             , ("Start",   1)
             , ("End",     2) ]
             [ ''Show, ''Read, ''Eq, ''Bounded, ''Enum ])
