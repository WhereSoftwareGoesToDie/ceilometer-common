{-# LANGUAGE RankNTypes #-}

--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
--
-- This module defines a convienient interface for clients
-- of Ceilometer.
--
-- For flexibility use the Collector and Fold modules.
--
module Ceilometer.Client
  ( -- * Decoding
    decodeChecked, decode, decodeWith
    -- * Folding
  , foldDecoded, foldDecodedWith
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Traversable    as T
import           Data.Typeable
import           Data.Word
import           Marquise.Client
import           Pipes

import           Ceilometer.Fold
import           Ceilometer.Infer
import           Ceilometer.Types


-- Decode ----------------------------------------------------------------------

-- | Figures out what prism to use, then decode.
--   if the inferred prism doesn't match the supplied type, there will be a
--   run-time error. (that's what you get for untyped data!)
--
decodeChecked
  :: (Typeable a, Monad m)
  => proxy a
  -> Env
  -> Maybe (Pipe SimplePoint (Maybe (Timed a)) m ())
decodeChecked _ = decode

-- | Figures out what prism to use, then decode.
--
decode
  :: (Typeable a, Monad m)
  => Env
  -> Maybe (Pipe SimplePoint (Maybe (Timed a)) m ())
decode env = case inferPrism env of
  Just p  -> Just $ decodeWith $ clonePrism p
  Nothing -> Nothing -- note: don't try to fmap. ImpredicativeTypes nastiness.

-- | Decodes the raw stream of SimplePoint using the given prism for the payload.
decodeWith
  :: Monad m
  => Prism' Word64 a
  -> Pipe SimplePoint (Maybe (Timed a)) m ()
decodeWith p = do
  SimplePoint _ (TimeStamp t) v <- await
  yield $ T.sequence $ Timed t $ v ^? p

-- Fold ------------------------------------------------------------------------

-- | Folds a stream of decoded points,
--   using the sourcedict to determine the correct fold.
--
foldDecoded
  :: (Typeable a, Applicative m, Monad m)
  => Env
  -> Producer (Timed a) m ()
  -> m (Maybe FoldResult)
foldDecoded env points = T.sequenceA $ flip pFoldStream points <$> inferFold env

-- | Folds a stream of decoded points,
--   using the explicitly provided fold.
--
foldDecodedWith
  :: (Monad m)
  => PFold (Timed a) FoldResult
  -> Producer (Timed a) m ()
  -> m FoldResult
foldDecodedWith = pFoldStream
