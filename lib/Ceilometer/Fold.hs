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
  ( -- * Generic folds (and Decode)
    Known(..)
  , FoldResult(..)

    -- * Low-level folds
  , foldCPU
  , foldDiskRead
  , foldDiskWrite
  , foldNeutronRx
  , foldNeutronTx
  , foldVolume
  , foldSSD
  , foldImage
  , foldImagePollster
  , foldInstanceFlavor
  , foldInstanceVCPU
  , foldInstanceRAM
  , foldInstanceDisk
  , foldIP
  , foldSnapshot
    -- * Utilities
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
import           Vaultaire.Types


--------------------------------------------------------------------------------

-- | A universial wrapper of fold results to expose to the user.
--
--   For simplicity, it's not associated with the @Known@ class. If the need to do
--   that arises, use Typeable and Constraint tricks to pattern match.
--
data FoldResult
  = RSingle   Word64
  | RMapNum32 (Map PFValue32 Word64)
  | RMapNum64 (Map PFValue64 Word64)
  | RMapText  (Map PFValueText Word64)

-- | An OpenStack measured known to Ceilometer. We can determine how to decode
--   and aggregate Vaultaire data for it.
--
class Known a where
  mkPrism :: Env -> APrism' Word64 a
  mkFold  :: Env -> PFold (Timed a) FoldResult

instance Known PDCPU where
  mkPrism _ = prSimple . pdCPU
  mkFold  _ = after RSingle (generalizeFold (timewrapFold foldCPU))

instance Known PDDiskRead where
  mkPrism _ = prSimple . pdDiskRead
  mkFold  _ = after RSingle (generalizeFold (timewrapFold foldDiskRead))

instance Known PDDiskWrite where
  mkPrism _ = prSimple . pdDiskWrite
  mkFold  _ = after RSingle (generalizeFold (timewrapFold foldDiskWrite))

instance Known PDNeutronTx where
  mkPrism _ = prSimple . pdNeutronTx
  mkFold  _ = after RSingle (generalizeFold (timewrapFold foldNeutronTx))

instance Known PDNeutronRx where
  mkPrism _ = prSimple . pdNeutronRx
  mkFold  _ = after RSingle (generalizeFold (timewrapFold foldNeutronRx))

instance Known PDImagePollster where
  mkPrism _ = prSimple . pdImagePollster
  mkFold  _ = after RMapNum64 (generalizeFold foldImagePollster)

instance Known PDVolume where
  mkPrism _ = prCompoundEvent . pdVolume
  mkFold (Env _ _ _ (TimeStamp s) (TimeStamp e))
    = after RSingle (foldVolume (s,e))

instance Known PDSSD where
  mkPrism _  = prCompoundEvent . pdSSD
  mkFold (Env _ _ _ (TimeStamp s) (TimeStamp e))
    = after RSingle (foldSSD (s,e))

instance Known PDImage where
  mkPrism _  = prCompoundEvent . pdImage
  mkFold (Env _ _ _ (TimeStamp s) (TimeStamp e))
    = after RSingle (foldImage (s,e))

instance Known PDSnapshot where
  mkPrism _  = prCompoundEvent . pdSnapshot
  mkFold (Env _ _ _ (TimeStamp s) (TimeStamp e))
    = after RSingle (foldSnapshot (s,e))

instance Known PDIP where
  mkPrism _  = prCompoundEvent . pdIP
  mkFold (Env _ _ _ (TimeStamp s) (TimeStamp e))
    = after RSingle (foldIP (s,e))

instance Known PDInstanceVCPU where
  mkPrism _                = prCompoundPollster . pdInstanceVCPU
  mkFold  (Env _ _  f _ _) = after RMapNum32
                                   ( generalizeFold   $
                                     foldInstanceVCPU $
                                     filterByInstanceStatus f (\(PDInstanceVCPU s _) -> s))

instance Known PDInstanceRAM where
  mkPrism _                = prCompoundPollster . pdInstanceRAM
  mkFold  (Env _ _  f _ _) = after RMapNum32
                                   ( generalizeFold  $
                                     foldInstanceRAM $
                                     filterByInstanceStatus f (\(PDInstanceRAM s _) -> s))

instance Known PDInstanceDisk where
  mkPrism _ = prCompoundPollster . pdInstanceDisk
  mkFold  (Env _ _  f _ _) = after RMapNum32
                                   ( generalizeFold   $
                                     foldInstanceDisk $
                                     filterByInstanceStatus f (\(PDInstanceDisk s _) -> s))

instance Known PDInstanceFlavor where
  mkPrism (Env fm _ _ _ _) = prCompoundPollster . pdInstanceFlavor fm
  mkFold  (Env _ _  f _ _) = after RMapText
                                   ( generalizeFold   $
                                     foldInstanceFlavor $
                                     filterByInstanceStatus f (\(PDInstanceFlavor s _) -> s))


-- Fold ------------------------------------------------------------------------

foldCPU :: L.Fold PDCPU Word64
foldCPU = L.Fold sCumulative bCumulative eCumulative

foldDiskRead :: L.Fold PDDiskRead Word64
foldDiskRead = L.Fold sCumulative bCumulative eCumulative

foldDiskWrite :: L.Fold PDDiskWrite Word64
foldDiskWrite = L.Fold sCumulative bCumulative eCumulative

foldNeutronTx :: L.Fold PDNeutronTx Word64
foldNeutronTx = L.Fold sCumulative bCumulative eCumulative

foldNeutronRx :: L.Fold PDNeutronRx Word64
foldNeutronRx = L.Fold sCumulative bCumulative eCumulative

foldVolume :: Window -> PFold (Timed PDVolume) Word64
foldVolume window = PFold step bEvent (eEvent window standardEventFolder)
  where -- Stop folding as soon as the volume is deleted
        step (More (prev,acc)) (Timed end (PDVolume _ VolumeDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until volume delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldSSD :: Window -> PFold (Timed PDSSD) Word64
foldSSD window = PFold step bEvent (eEvent window standardEventFolder)
  where -- Stop folding as soon as the volume is deleted
        step (More (prev,acc)) (Timed end (PDSSD _ VolumeDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until volume delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldImage :: Window -> PFold (Timed PDImage) Word64
foldImage window = PFold step bEvent (eEvent window standardEventFolder)
  where -- Stop folding as soon as the image is deleted
        step (More (prev,acc)) (Timed end (PDImage _ ImageDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until image delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldSnapshot :: Window -> PFold (Timed PDSnapshot) Word64
foldSnapshot window = PFold step bEvent (eEvent window standardEventFolder)
  where -- Stop folding as soon as the snapshot is deleted
        step (More (prev,acc)) (Timed end (PDSnapshot _ SnapshotDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until snapshot delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldIP :: Window ->  PFold (Timed PDIP) Word64
foldIP window = PFold step bEvent (eEvent window ipEventFolder)
  where -- Stop folding as soon as the IP is deleted
        step (More (prev,acc)) (Timed end (PDIP _ IPDelete _ _))
          = Term (Nothing, go end acc prev)
        step a x = sEvent window a x

        -- Adds the duration up until IP delete
        go end acc (Just x) = M.insertWith (+) (x ^. value) (end - x ^. time) acc
        go _   acc  Nothing = acc

foldInstanceFlavor   :: (PDInstanceFlavor -> Bool)
                     -> L.Fold (Timed PDInstanceFlavor) (Map PFValueText Word64)
foldInstanceFlavor f =  L.Fold (sGaugePollster f) bGaugePollster snd

foldInstanceVCPU     :: (PDInstanceVCPU -> Bool)
                     -> L.Fold (Timed PDInstanceVCPU) (Map PFValue32 Word64)
foldInstanceVCPU   f =  L.Fold (sGaugePollster f) bGaugePollster snd

foldInstanceRAM      :: (PDInstanceRAM -> Bool)
                     -> L.Fold (Timed PDInstanceRAM)    (Map PFValue32 Word64)
foldInstanceRAM    f =  L.Fold (sGaugePollster f) bGaugePollster snd

foldInstanceDisk     :: (PDInstanceDisk -> Bool)
                     -> L.Fold (Timed PDInstanceDisk)   (Map PFValue32 Word64)
foldInstanceDisk   f =  L.Fold (sGaugePollster f) bGaugePollster snd

foldImagePollster  :: L.Fold (Timed PDImagePollster)  (Map PFValue64 Word64)
foldImagePollster  =  L.Fold (sGaugePollster $ const True) bGaugePollster snd

-- Utilities -------------------------------------------------------------------

after :: (y -> z) -> PFold x y -> PFold x z
after f (PFold s b e) = PFold s b (f . e)

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

type AGaugePollster x = ( Maybe (Timed x)          -- latest
                        , Map (PFValue x) Word64 ) -- accumulated map

-- | Finds the length of time allocated to each "state" of the resource.
--   e.g. time a @Volume@ spent at 10GB, then at 20GB (if resized), etc.
sGaugePollster :: (Valued x, Ord (PFValue x))
          => (x -> Bool) -> AGaugePollster x -> Timed x -> AGaugePollster x
sGaugePollster _          (Nothing,            acc) x =  (Just x, acc)
sGaugePollster isBillable (Just (Timed t1 v1), acc) x@(Timed t2 _)
  = let delta = t2 - t1
        acc'  = if isBillable v1 then
                    M.insertWith (+) (v1 ^. value) (fromIntegral delta) acc
                else
                    acc
    in (Just x, acc')

bGaugePollster = (Nothing, M.empty)


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

eEvent (start, end) f = M.foldlWithKey f 0 . go . unwrapAcc
  where -- deal with the dangling last event
        go (Just x, acc)
          | d <- end - s x, d > 0 = insertEvent x d acc
        go a@_                          = snd a
        s x = max start (x ^. time)

standardEventFolder a k v = a + (fromIntegral k * v)
ipEventFolder       a _ v = a + v

insertEvent x = M.insertWith (+) (x ^. value)
