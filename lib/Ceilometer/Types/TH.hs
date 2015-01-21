--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--
-- /Description/
-- This module defines TH to define Ceilometer types internally.
-- DO NOT EXPORT.
--

module Ceilometer.Types.TH
  ( declarePF
  ) where

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           Language.Haskell.TH

-- | Declare a Payload Field data type and create a prism for it
--   with the mapping provided.
--
--   The type must be non-parametrised.
--
--   /e.g./
--
--   @
--   declarePF    "Volume"
--               ("Verb", ''Word8)
--             [ ("Create", 1)
--             , ("Resize", 2) ]
--             [ ''Show, ''Eq, ''Bounded, ''Enum ]
--   @
--
--   will create:
--
--   @
--   data PFVolumeVerb = VolumeCreate | VolumeResize
--        deriving (Show, Eq, Enum, Bounded)
--
--   pfVolumeVerb :: Prism' Word8 PFVolumeVerb
--   pfVolumeVerb = prism' pretty parse
--     where pretty VolumeCreate = 1
--           pretty VolumeResize = 2
--           parse  1 = Just VolumeCreate
--           parse  2 = Just VolumeResize
--           parse  _ = Nothing
--   @
--
--   This is useful if you want parsing/print prisms for a non-parametric type.
--
declarePF :: String              -- ^ Field prefix, e.g. Volume
          -> (String,  Name)     -- ^ Type constructors to be declared and parsed from,
                                 --   the type from which we parse must have literal values.
          -> [(String, Integer)] -- ^ Data constructors and their mapping
          -> [Name]              -- ^ Derived classes
          -> Q [Dec]
declarePF = declareWith "pf"

declareWith :: String -> String -> (String,  Name) -> [(String, Integer)] -> [Name] -> Q [Dec]
declareWith prefix field (tyconStr, mappedType) ds derives = do
  pfunc      <- lookupPrismFunc
  pty        <- lookupPrismTyCon
  just       <- lookupJust
  nothing    <- lookupNothing
  let tycon    = mkName $ map toUpper prefix ++ field ++ tyconStr
      dacons   = map (mkName . (field ++) . fst) ds
      dec      = DataD [] tycon [] (map (flip NormalC []) dacons) derives

      p        = mkName (prefix ++ field ++ tyconStr)
      vals     = map (IntegerL . snd) ds
      -- make pattern: pretty VolumeCreate = 1
      pretties = zipWith mkClause
                         (map ((:[]) . flip ConP []) dacons)
                         (map (NormalB . LitE) vals)
      pretty   = mkName "pretty"
      -- make pattern: parse 1 = Just VolumeCreate
      parses   = zipWith mkClause
                         (map ((:[]) . LitP) vals)
                         (map (NormalB . AppE (ConE just) . ConE) dacons)
               ++ [Clause [WildP] (NormalB (ConE nothing)) []]
      parse    = mkName "parse"
      -- make clause: prism' pretty parse where ...
      cases    = Clause []
                        (NormalB $ AppE (AppE (VarE pfunc) (VarE pretty)) (VarE parse))
                        [FunD pretty pretties, FunD parse parses]
      -- declare the prism
      sig      = SigD p (AppT (AppT (ConT pty) (ConT mappedType)) (ConT tycon))
      def      = FunD p [cases]
  return [dec, sig, def]

mkClause :: [Pat] -> Body -> Clause
mkClause x y = Clause x y []

-- | These Will throw a first-stage compile-time error if not in scope.
--
lookupPrismFunc, lookupPrismTyCon :: Q Name
lookupJust, lookupNothing         :: Q Name
lookupPrismFunc  = lookupV "prism'"
lookupPrismTyCon = lookupT "Prism'"
lookupJust       = lookupV "Just"
lookupNothing    = lookupV "Nothing"

lookupT :: String -> Q Name
lookupT x = fromMaybe (error $ "TH: not in scope " ++ x) <$> lookupTypeName x

lookupV :: String -> Q Name
lookupV x = fromMaybe (error $ "TH: not in scope " ++ x) <$> lookupValueName x
