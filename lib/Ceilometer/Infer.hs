{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
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
-- This module defines "type inference" for Ceilometer points
-- based on the (key,val) SourceDict schema.
--
--
module Ceilometer.Infer
  ( -- * Inferences
    inferPrism
  , inferFold
  , FoldResult(..)
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
inferPrism (Env fm sd _ _) = do
  name <- lookupMetricName sd
  case name of
    "cpu"             -> do Refl <- eqT :: Maybe (a :~: PDCPU)
                            Just (prSimple . pdCPU)
    "volume.size"     -> do Refl <- eqT :: Maybe (a :~: PDVolume)
                            Just (prCompoundEvent . pdVolume)
    "instance_flavor" -> do Refl <- eqT :: Maybe (a :~: PDInstanceFlavor)
                            Just (prCompoundPollster . pdInstanceFlavor fm)
    "instance_vcpu"   -> do Refl <- eqT :: Maybe (a :~: PDInstanceVCPU)
                            Just (prCompoundPollster . pdInstanceVCPU)
    "instance_ram"    -> do Refl <- eqT :: Maybe (a :~: PDInstanceRAM)
                            Just (prCompoundPollster . pdInstanceRAM)

    -- TODO what about instance_disk??? does it exist? is it disk.read/write??

    _ -> Nothing


-- | Infers the schema of the raw stream from the @SourceDict@,
--   and gives an aggregation method for that schema type.
--
inferFold :: forall a. Typeable a => Env -> Maybe (PFold (Timed a) FoldResult)
inferFold (Env _ sd (TimeStamp s) (TimeStamp e))= case (lookupMetricName sd, lookupEvent sd) of
  (Just "disk.write.bytes", Nothing)  -> undefined
  (Just "disk.write.bytes", Just "0") -> undefined
  (Just "disk.write.bytes", Just "1") -> Nothing

  (Just "instance_flavor", Just "0")  -> do
    Refl <- eqT :: Maybe (a :~: PDInstanceFlavor)
    return $ M2 <$> generalizeFold foldInstanceFlavor

  (Just "instance_vcpu", Just "0")  -> do
    Refl <- eqT :: Maybe (a :~: PDInstanceVCPU)
    return $ M1 <$> generalizeFold foldInstanceVCPU

  (Just "instance_ram", Just "0")  -> do
    Refl <- eqT :: Maybe (a :~: PDInstanceRAM)
    return $ M1 <$> generalizeFold foldInstanceRAM

  (Just "volume.size",      Just "1") -> do
    Refl <- eqT :: Maybe (a :~: PDVolume)
    return $ W <$> foldVolume (s, e)

  (Just "cpu",              _)        -> do
    Refl <- eqT :: Maybe (a :~: PDCPU)
    return $ W <$> generalizeFold (timewrapFold foldCPU)

  _                                   -> Nothing

-- | Universal wrapper so that clients can deal with fold results transparently
data FoldResult = W  Word64
                | M1 (Map PFValue32 Word64)
                | M2 (Map PFValueText Word64)
