{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
  ( -- * Combo
    decodeAndFold
    -- * Decoding
  , decode, decodeWith
    -- * Folding
  , foldDecoded, foldDecodedWith
    -- * Re-exports
  , module C
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import qualified Data.Traversable    as T
import           Data.Typeable
import           Data.Word
import           Pipes

import           Ceilometer.Fold     as C
import           Ceilometer.Infer    as C
import           Ceilometer.Types    as C
import           Vaultaire.Types


-- | Decode and fold a stream of @SimplePoint@.
--
--   If the assertion that these points should be type `a` is wrong: run-time error.
--
--   If the assertion is true but the @SourceDict@ doesn't match what we expect
--   for the fold: `Nothing`.
--
--   Otherwise we get a `FoldResult` matching that type.
--
decodeAndFold
  :: forall a m proxy. (Typeable a, Monad m, Applicative m)
  => proxy a                    -- ^ We expect these @SimplePoint@ to be of type `a`
  -> Env                        -- ^ @SourceDict@ to verify the above claim.
  -> Producer SimplePoint m ()  -- ^ The raw data points to parse and aggregate.
  -> m (Maybe (FoldResult a))   -- ^ Result
decodeAndFold _ env points
  = let decoded :: Maybe (Producer (Timed a) m ())
        decoded =  useDecoding <$> decode env
    in  fromMaybe (return Nothing) $ foldDecoded env <$> decoded
    where useDecoding d = points >-> d >-> handleParseFails


-- Decode ----------------------------------------------------------------------

-- | Figures out what prism to use, then decode.
--
decode
  :: (Typeable a, Monad m)
  => Env                                             -- ^ @SourceDict@ to use and guess the correct decoding
  -> Maybe (Pipe SimplePoint (Maybe (Timed a)) m ()) -- ^ Decoding pipe
decode env = case inferPrism env of
  Just p  -> Just $ decodeWith $ clonePrism p
  Nothing -> Nothing -- note: don't try to fmap. ImpredicativeTypes nastiness.

-- | Decodes the raw stream of SimplePoint using the given prism for the payload.
decodeWith
  :: Monad m
  => Prism' Word64 a                         -- ^ Use this particular prism for decoding
  -> Pipe SimplePoint (Maybe (Timed a)) m () -- ^ Decoding pipe
decodeWith p = do
  SimplePoint _ (TimeStamp t) v <- await
  yield $ T.sequence $ Timed t $ v ^? p

ignore :: Monad m => Pipe (Maybe x) x m r
ignore = forever $ do
  x <- await
  maybe (return ()) yield x

-- TODO "blow up" more gracefully, log?
blowup :: Monad m => Pipe (Maybe x) x m r
blowup = forever $ do
  x <- await
  maybe (error "fatal: unparseable point") yield x

handleParseFails :: forall a m r. (Monad m, Typeable a) => Pipe (Maybe (Timed a)) (Timed a) m r
handleParseFails = fromMaybe blowup $
      (const ignore <$> (eqT :: Maybe (a :~: PDCPU)))
  <|> (const blowup <$> (eqT :: Maybe (a :~: PDVolume)))


-- Fold ------------------------------------------------------------------------

-- | Folds a stream of decoded points,
--   using the sourcedict to determine the correct fold.
--
foldDecoded
  :: (Typeable a, Applicative m, Monad m)
  => Env                     -- ^ @SourceDict@ to use and guess the correct decoding
  -> Producer (Timed a) m () -- ^ Data points
  -> m (Maybe (FoldResult a))
foldDecoded env points = T.sequenceA $ flip pFoldStream points <$> inferFold env

-- | Folds a stream of decoded points,
--   using the explicitly provided fold.
--
foldDecodedWith
  :: (Monad m)
  => PFold (Timed a) (FoldResult a) -- ^ Use this particular fold method
  -> Producer (Timed a) m ()        -- ^ Data points
  -> m (FoldResult a)
foldDecodedWith = pFoldStream
