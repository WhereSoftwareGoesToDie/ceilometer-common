--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines folding methods for Ceilometer types,
-- to be used by clients such as Borel.
--

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS -fno-warn-missing-signatures #-}

module Ceilometer.Fold
  ( -- * Resource-specific Folds
    foldCPU
  , foldVolume
  , foldSSD
  , foldInstanceFlavor
  , foldInstanceVCPU
  , foldInstanceRAM
  , foldInstanceDisk
    -- * Utilities
  , FoldResult
  , Acc(..), PFold(..), pFold, pFoldStream
  , generalizeFold
  , timewrapFold
  ) where


import qualified Control.Foldl    as L
import           Control.Lens     hiding (Fold, Simple)
import           Data.Map         (Map)
import qualified Data.Map.Strict  as M
import           Data.Word

import           Ceilometer.Types
import           Control.PFold


type family FoldResult x where
  FoldResult PDCPU            = Word64
  FoldResult PDVolume         = Word64
  FoldResult PDSSD            = Word64
  FoldResult PDInstanceVCPU   = Map PFValue32 Word64
  FoldResult PDInstanceRAM    = Map PFValue32 Word64
  FoldResult PDInstanceDisk   = Map PFValue32 Word64
  FoldResult PDInstanceFlavor = Map PFValueText Word64

-- Fold ------------------------------------------------------------------------

foldCPU :: L.Fold PDCPU (FoldResult PDCPU)
foldCPU = L.Fold sCumulative bCumulative eCumulative

foldVolume :: Window -> PFold (Timed PDVolume) (FoldResult PDVolume)
foldVolume window = PFold step bEvent (eEvent window)
  where -- Stop folding as soon as the volume is deleted
        step (More (prev,acc)) (Timed end (PDVolume _ VolumeDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until volume delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldSSD :: Window -> PFold (Timed PDSSD) (FoldResult PDSSD)
foldSSD window = PFold step bEvent (eEvent window)
  where -- Stop folding as soon as the volume is deleted
        step (More (prev,acc)) (Timed end (PDSSD _ VolumeDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until volume delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldInstanceFlavor :: L.Fold (Timed PDInstanceFlavor) (FoldResult PDInstanceFlavor)
foldInstanceFlavor =  L.Fold sPollster bPollster snd

foldInstanceVCPU   :: L.Fold (Timed PDInstanceVCPU) (FoldResult PDInstanceVCPU)
foldInstanceVCPU   =  L.Fold sPollster bPollster snd

foldInstanceRAM    :: L.Fold (Timed PDInstanceRAM) (FoldResult PDInstanceRAM)
foldInstanceRAM    =  L.Fold sPollster bPollster snd

foldInstanceDisk   :: L.Fold (Timed PDInstanceDisk) (FoldResult PDInstanceDisk)
foldInstanceDisk   =  L.Fold sPollster bPollster snd


-- Utilities -------------------------------------------------------------------

-- | Wrap a fold that doens't depend on time with dummy times.
--   note: useful to give a unified interface to clients (borel) while keeping
--         concerns separate for testing.
--
timewrapFold :: L.Fold x y -> L.Fold (Timed x) y
timewrapFold (L.Fold s b e)
  = L.Fold (\(Timed _ a) (Timed _ x) -> Timed 0 $ s a x)
           (Timed 0 b)
           (\(Timed _ v) -> e v)


-- Common Steps ----------------------------------------------------------------

-- The above folds are built from these common fold steps

type Window   = (Word64, Word64)

-- | Openstack cumulative data is from last startup.
--   So when we process cumulative data we need to account for this.
--   Since (excluding restarts) each point is strictly non-decreasing,
--   we simply use a modified fold to deal with the case where the latest point
--   is less than the second latest point (indicating a restart)
--
--   note: copied from vaultaire-query
--
type ACumulative x = ( Maybe ( PFValue x  -- first
                             , PFValue x) -- latest
                     , Word64 )           -- sum

sCumulative :: (Valued x, Ord (PFValue x), Integral (PFValue x))
            => ACumulative x -> x -> ACumulative x
sCumulative (Nothing,          acc) x | v <- x ^. value = (Just (v, v), acc)
sCumulative (Just (f, latest), acc) x | v <- x ^. value =
  if    v < latest
  then (Just (f, v), acc + fromIntegral latest)
  else (Just (f, v), acc)

bCumulative = (Nothing, 0)

eCumulative (Just (first, latest), acc) = acc + latest - first
eCumulative (_, acc)                    = acc


type APollster x = ( Maybe (Timed x)          -- latest
                   , Map (PFValue x) Word64 ) -- accumulated map

-- | Finds the length of time allocated to each "state" of the resource.
--   e.g. time a @Volume@ spent at 10GB, then at 20GB (if resized), etc.
sPollster :: (Valued x, Ord (PFValue x))
          => APollster x -> Timed x -> APollster x
sPollster (Nothing,            acc) x =  (Just x, acc)
sPollster (Just (Timed t1 v1), acc) x@(Timed t2 _)
  = let delta = t2 - t1
    in  (Just x, M.insertWith (+) (v1 ^. value) (fromIntegral delta) acc)

bPollster = (Nothing, M.empty)


type AEvent x = Acc ( Maybe (Timed x)          -- latest
                    , Map (PFValue x) Word64 ) -- accumulated map

-- | Time-lapse for each value from a stream of events.
--   note: mostly COPIED from borel-core's Consolidated Query code
--         needs checking.
--
sEvent :: (Valued x, Ord (PFValue x), Show (PFValue x))
        => Window -> AEvent x -> Timed x -> AEvent x
sEvent _ a@(Term _) _ = a
sEvent _ (More (Nothing, acc)) x
  = More (Just x, acc)
sEvent (start, end) (More (Just prev, acc)) x
  = if | prev ^. time > end   -> Term (Nothing, acc) -- last step went past the end of the window
       | prev ^. time < start -> More (Just x, acc)  -- last step hasn't gone in the window
       | d <= 0               -> Term (Nothing, acc) -- this step went past the window
       | otherwise            -> More (Just x, M.insertWith (+) (prev ^. value) d acc)
  where s = max start (prev ^. time)
        e = min end   (x    ^. time)
        d = e - s

bEvent = More (Nothing, M.empty)

eEvent (start, end) = M.foldlWithKey (\a k v -> a + (fromIntegral k * v)) 0 . go . unwrapAcc
  where -- deal with the dangling last event
        go (Just x, acc)
          | d <- end - s x, d > 0 = insertEvent x d acc
        go a@_                          = snd a
        s x = max start (x ^. time)

insertEvent x = M.insertWith (+) (x ^. value)
