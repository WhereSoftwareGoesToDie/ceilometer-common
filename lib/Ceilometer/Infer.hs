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
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Text           (Text)
import           Data.Typeable
import           Data.Word

import           Ceilometer.Tags
import           Ceilometer.Fold
import           Ceilometer.Types
import           Ceilometer.Tags
import           Vaultaire.Types

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
  if | name == valCPU -> do
         Refl <- eqT :: Maybe (a :~: PDCPU)
         Just (pCPU, fCPU)

     | name == valVolume && sourceIsBlock sd -> do
         Refl <- eqT :: Maybe (a :~: PDVolume)
         Just (pVolume, fVolume s e)

     | name == valVolume && sourceIsFast  sd -> do
         Refl <- eqT :: Maybe (a :~: PDSSD)
         Just (pSSD, fSSD s e)

     | name == valInstanceFlavor -> do
         Refl <- eqT :: Maybe (a :~: PDInstanceFlavor)
         Just (pInstanceFlavor fm, fInstanceFlavor)

     | name == valInstanceVCPU -> do
         Refl <- eqT :: Maybe (a :~: PDInstanceVCPU)
         Just (pInstanceVCPU, fInstanceVCPU)

     | name == valInstanceRAM -> do
         Refl <- eqT :: Maybe (a :~: PDInstanceRAM)
         Just (pInstanceRAM, fInstanceRAM)

     | otherwise -> Nothing

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

fCPU            = generalizeFold (timewrapFold foldCPU)
fVolume s e     = foldVolume (s,e)
fSSD s e        = foldSSD (s,e)
fInstanceFlavor = generalizeFold foldInstanceFlavor
fInstanceVCPU   = generalizeFold foldInstanceVCPU
fInstanceRAM    = generalizeFold foldInstanceRAM
