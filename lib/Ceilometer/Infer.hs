{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS -fno-warn-missing-signatures #-}

--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
--
-- This module defines "type inference" for Ceilometer points
-- based on the (key,val) SourceDict schema.
--
--
module Ceilometer.Infer
  ( -- * Inferences
    inferPrism
  , inferFold
  , FoldResult
    -- * Utilities
  , lookupEvent, lookupMetricName, isEvent
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Text           (Text)
import           Data.Typeable
import           Data.Word

import           Ceilometer.Fold
import           Ceilometer.Types
import           Vaultaire.Types

lookupEvent, lookupMetricName :: SourceDict -> Maybe Text
lookupMetricName = lookupSource "metric_name"
lookupEvent      = lookupSource "_event"

isEvent :: SourceDict -> Bool
isEvent sd = case lookupEvent sd of
    Just "1" -> True
    Just _   -> False
    Nothing  -> False

inferPrism :: forall a. Typeable a
           => Env -> Maybe (APrism' Word64 a)
inferPrism e = fst <$> inferPrismFold e

inferFold  :: forall a. Typeable a
           => Env -> Maybe (PFold (Timed a) (FoldResult a))
inferFold  e = snd <$> inferPrismFold e

-- | Infers the schema of the raw stream from the @SourceDict@,
--   and gives a decoding and folding method for that schema type.
--
inferPrismFold :: forall a. Typeable a
               => Env -> Maybe (APrism' Word64 a, PFold (Timed a) (FoldResult a))
inferPrismFold (Env fm sd (TimeStamp s) (TimeStamp e)) = do
  name <- lookupMetricName sd
  case name of
    "cpu"                -> do Refl <- eqT :: Maybe (a :~: PDCPU)
                               Just (pCPU, fCPU)

    "volume.size" -> if
      | sourceIsBlock sd -> do Refl <- eqT :: Maybe (a :~: PDVolume)
                               Just (pVolume, fVolume s e)
      | sourceIsFast  sd -> do Refl <- eqT :: Maybe (a :~: PDSSD)
                               Just (pSSD, fSSD s e)
      | otherwise        ->    Nothing

    "instance_flavor"    -> do Refl <- eqT :: Maybe (a :~: PDInstanceFlavor)
                               Just (pInstanceFlavor fm, fInstanceFlavor)

    "instance_vcpu"      -> do Refl <- eqT :: Maybe (a :~: PDInstanceVCPU)
                               Just (pInstanceVCPU, fInstanceVCPU)

    "instance_ram"       -> do Refl <- eqT :: Maybe (a :~: PDInstanceRAM)
                               Just (pInstanceRAM, fInstanceRAM)

    "instance_disk"      -> do Refl <- eqT :: Maybe (a :~: PDInstanceDisk)
                               Just (pInstanceDisk, fInstanceDisk)

    "image.size" -> if
      | isEvent sd       -> do Refl <- eqT :: Maybe (a :~: PDImage)
                               Just (pImage, fImage s e)
      | otherwise        -> do Refl <- eqT :: Maybe (a :~: PDImageP)
                               Just (pImageP, fImageP)

    "snapshot.size"      -> do Refl <- eqT :: Maybe (a :~: PDSnapshot)
                               Just (pSnapshot, fSnapshot s e)

    _ -> Nothing

-- "Universalised" versions of prisms and folds

pCPU :: APrism' Word64 PDCPU
pCPU = prSimple . pdCPU

pVolume :: APrism' Word64 PDVolume
pVolume = prCompoundEvent . pdVolume

pSSD :: APrism' Word64 PDSSD
pSSD = prCompoundEvent . pdSSD

pInstanceFlavor :: FlavorMap -> APrism' Word64 PDInstanceFlavor
pInstanceFlavor fm = prCompoundPollster . pdInstanceFlavor fm

pInstanceVCPU :: APrism' Word64 PDInstanceVCPU
pInstanceVCPU = prCompoundPollster . pdInstanceVCPU

pInstanceRAM :: APrism' Word64 PDInstanceRAM
pInstanceRAM = prCompoundPollster . pdInstanceRAM

pInstanceDisk :: APrism' Word64 PDInstanceDisk
pInstanceDisk = prCompoundPollster . pdInstanceDisk

pImage :: APrism' Word64 PDImage
pImage = prCompoundEvent . pdImage

pImageP :: APrism' Word64 PDImageP
pImageP = prSimple . pdImageP

pSnapshot :: APrism' Word64 PDSnapshot
pSnapshot = prCompoundEvent . pdSnapshot

fCPU            = generalizeFold (timewrapFold foldCPU)
fVolume s e     = foldVolume (s,e)
fSSD s e        = foldSSD (s,e)
fInstanceFlavor = generalizeFold foldInstanceFlavor
fInstanceVCPU   = generalizeFold foldInstanceVCPU
fInstanceRAM    = generalizeFold foldInstanceRAM
fInstanceDisk   = generalizeFold foldInstanceDisk
fImage s e      = foldImage (s,e)
fImageP         = generalizeFold foldImageP
fSnapshot s e   = foldSnapshot (s,e)
