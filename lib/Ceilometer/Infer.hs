{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , FoldResult(..)
    -- * Wrappers around prisms and folds
  , pCPU, pVolume, pInstanceVCPU, pInstanceRAM, pInstanceFlavor
  , fCPU, fVolume, fInstanceVCPU, fInstanceRAM, fInstanceFlavor
    -- * Utilities
  , lookupEvent, lookupMetricName
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Map            (Map)
import           Data.Text           (Text)
import           Data.Typeable
import           Data.Word

import           Ceilometer.Fold
import           Ceilometer.Types
import           Vaultaire.Types

lookupEvent, lookupMetricName :: SourceDict -> Maybe Text
lookupMetricName = lookupSource "metric_name"
lookupEvent      = lookupSource "_event"


inferPrism :: forall a. Typeable a => Env -> Maybe (APrism' Word64 a)
inferPrism e = fst <$> inferPrismFold e

inferFold  :: forall a. Typeable a => Env -> Maybe (PFold (Timed a) FoldResult)
inferFold  e = snd <$> inferPrismFold e

-- | Infers the schema of the raw stream from the @SourceDict@,
--   and gives a decoding and folding method for that schema type.
--
inferPrismFold :: forall a. Typeable a => Env -> Maybe (APrism' Word64 a, PFold (Timed a) FoldResult)
inferPrismFold (Env fm sd (TimeStamp s) (TimeStamp e)) = do
  name <- lookupMetricName sd
  case name of
    "cpu"             -> do Refl <- eqT :: Maybe (a :~: PDCPU)
                            Just (pCPU, fCPU)

    "volume.size"     -> do Refl <- eqT :: Maybe (a :~: PDVolume)
                            Just (pVolume, fVolume s e)

    "instance_flavor" -> do Refl <- eqT :: Maybe (a :~: PDInstanceFlavor)
                            Just (pInstanceFlavor fm, fInstanceFlavor)

    "instance_vcpu"   -> do Refl <- eqT :: Maybe (a :~: PDInstanceVCPU)
                            Just (pInstanceVCPU, fInstanceVCPU)

    "instance_ram"    -> do Refl <- eqT :: Maybe (a :~: PDInstanceRAM)
                            Just (pInstanceRAM, fInstanceRAM)

    -- TODO what about instance_disk??? does it exist? is it disk.read/write??

    _ -> Nothing

-- | Universal wrapper so that clients can deal with fold results transparently
data FoldResult = W  Word64
                | M1 (Map PFValue32 Word64)
                | M2 (Map PFValueText Word64)

-- "Universalised" versions of prisms and folds

pCPU :: APrism' Word64 PDCPU
pCPU  = prSimple . pdCPU

fCPU = W <$> generalizeFold (timewrapFold foldCPU)

pVolume :: APrism' Word64 PDVolume
pVolume     = prCompoundEvent . pdVolume
fVolume s e = W <$> foldVolume (s,e)

pInstanceFlavor :: FlavorMap -> APrism' Word64 PDInstanceFlavor
pInstanceFlavor fm = prCompoundPollster . pdInstanceFlavor fm
fInstanceFlavor    = M2 <$> generalizeFold foldInstanceFlavor

pInstanceVCPU :: APrism' Word64 PDInstanceVCPU
pInstanceVCPU   = prCompoundPollster . pdInstanceVCPU
fInstanceVCPU   = M1 <$> generalizeFold foldInstanceVCPU

pInstanceRAM  :: APrism' Word64 PDInstanceRAM
pInstanceRAM    = prCompoundPollster . pdInstanceRAM
fInstanceRAM    = M1 <$> generalizeFold foldInstanceRAM
