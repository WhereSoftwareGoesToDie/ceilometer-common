{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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
  ( -- * Interface
    decodeFold
  , decodeFold_

    -- * Re-exports
  , module Ceilometer.Fold 
  , module Ceilometer.Tags
  , module Ceilometer.Types
  , module Vaultaire.Types
  ) where

import           Control.Applicative
import           Control.Foldl
import           Control.Lens
import           Control.Monad
import qualified Data.Traversable    as T
import           Pipes
import qualified Pipes.Prelude       as P

import           Ceilometer.Fold    
import           Ceilometer.Tags
import           Ceilometer.Types  
import           Vaultaire.Types


decodeFold
  :: (Monad m, Applicative m)
  => Env                        -- ^ @SourceDict@ to infer the resource type.
  -> Producer SimplePoint m ()  -- ^ The raw data points to parse and aggregate.
  -> m (Maybe FoldResult)       -- ^ Result

decodeFold env@(Env _ sd _ _ _) raw = do
  let x = do
        name <- lookupMetricName sd

        if | name == valCPU
             -> return (decodeFold_ (undefined :: proxy PDCPU) env raw)

           | name == valDiskReads
             -> return (decodeFold_ (undefined :: proxy PDDiskRead) env raw)

           | name == valDiskWrites
             -> return (decodeFold_ (undefined :: proxy PDDiskWrite) env raw)

           | name == valNeutronIn
             -> return (decodeFold_ (undefined :: proxy PDNeutronRx) env raw)

           | name == valNeutronOut
             -> return (decodeFold_ (undefined :: proxy PDNeutronTx) env raw)

           | name == valVolume -> do
             voltype <- lookupVolumeType sd

             if | voltype == valVolumeBlock
                  -> return (decodeFold_ (undefined :: proxy PDVolume) env raw)

                | voltype == valVolumeFast
                  -> return (decodeFold_ (undefined :: proxy PDSSD) env raw)

                | otherwise -> mzero

           | name == valInstanceFlavor -> do
             compound <- lookupCompound sd
             event    <- lookupEvent    sd

             if | compound == valTrue && event == valFalse
                  -> return (decodeFold_ (undefined :: proxy PDInstanceFlavor) env raw)

                | otherwise -> mzero

           | name == valInstanceVCPU
             -> return (decodeFold_ (undefined :: proxy PDInstanceVCPU) env raw)

           | name == valInstanceRAM
             -> return (decodeFold_ (undefined :: proxy PDInstanceRAM) env raw)

           | name == valImage ->
             if | isEvent sd
                  -> return (decodeFold_ (undefined :: proxy PDImage) env raw)
                | otherwise
                  -> return (decodeFold_ (undefined :: proxy PDImagePollster) env raw)

           | name == valSnapshot
              -> return (decodeFold_ (undefined :: proxy PDSnapshot) env raw)

           | name == valIP
              -> return (decodeFold_ (undefined :: proxy PDIP) env raw)

           | otherwise -> mzero
  T.sequence x

decodeFold_
  :: forall proxy a m . (Known a, Applicative m, Monad m)
  => proxy a
  -> Env
  -> Producer SimplePoint m ()
  -> m FoldResult
decodeFold_ _ env raw
  = foldDecoded env (raw >-> (decode env :: Pipe SimplePoint (Maybe (Timed a)) m ()) >-> blowup)

decode
  :: (Known a, Monad m)
  => Env
  -> Pipe SimplePoint (Maybe (Timed a)) m ()
decode env
  = let p = clonePrism (mkPrism env)
    in  forever $ {-# SCC actual_decode #-} do
          SimplePoint _ (TimeStamp t) v <- await
          yield $ T.sequence $ Timed t $ v ^? p
{-# INLINE decode #-}

foldDecoded
  :: (Known a, Monad m)
  => Env
  -> Producer (Timed a) m ()
  -> m FoldResult
foldDecoded env = impurely P.foldM (generalize $ mkFold env)
{-# INLINE foldDecoded #-}

-- | Abort the entire pipeline when encoutering malformed data in the Vault.
--
blowup :: Monad m => Pipe (Maybe x) x m r
blowup = forever $ {-# SCC blowup #-} do
  x <- await
  --maybe (error "fatal: unparseable point") yield x
  maybe (return ()) yield x
{-# INLINE blowup #-}
