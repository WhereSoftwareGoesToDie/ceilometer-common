--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines partial folds with early give-up
-- based on the code in Control.Foldl and Pipes
--

{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

module Control.PFold
  ( PFold(..)
  , pFold, pFoldStream
  , generalizeFold
  , Acc(..), unwrapAcc
  ) where

import qualified Control.Foldl  as L
import qualified Data.Foldable  as F
import           Pipes
import           Pipes.Internal


data Acc x = Term x | More x deriving Functor

unwrapAcc :: Acc x -> x
unwrapAcc (Term x) = x
unwrapAcc (More x) = x
{-# INLINE unwrapAcc #-}

-- | A "partial" Fold that allows early termination
data PFold a b = forall x. PFold (Acc x -> a -> Acc x) (Acc x) (Acc x -> b)

instance Functor (PFold a) where
  fmap f (PFold step begin done) = PFold step begin (f . done)
  {-# INLINABLE fmap #-}


pFold :: Foldable f => PFold a b -> f a -> b
pFold (PFold step begin done) as = F.foldr cons done as begin
  where cons a k x = k $! step x a
{-# INLINE pFold #-}

pFoldStream :: Monad m => PFold a b -> Producer a m () -> m b
pFoldStream (PFold step begin done) p0 = loop p0 begin
  where
    loop _ (Term x) = return (done $ Term x)
    loop p (More x) = case p of
        Request v  _  -> closed v
        Respond a  fu -> loop (fu ()) $! step (More x) a
        M          m  -> m >>= \p' -> loop p' (More x)
        Pure    _     -> return (done $ Term x)
{-# INLINABLE pFoldStream #-}

generalizeFold :: L.Fold a b -> PFold a b
generalizeFold (L.Fold step begin end)
  = PFold (flip (fmap . flip step))
          (More begin)
          (\case Term x -> end x
                 More x -> end x)
{-# INLINABLE generalizeFold #-}
