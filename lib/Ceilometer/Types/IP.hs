module Ceilometer.Types.IP where

import           Data.Bimap (Bimap)
import qualified Data.Bimap as B
import           Data.Word

import Ceilometer.Types.Base

data IPStatus = IPNone
              | IPActive
              | IPDown
  deriving (Show, Eq, Ord)

ipStatusMap :: Bimap IPStatus Word64
ipStatusMap = foldl (\bm (s, v) -> B.insert s v bm)
                    B.empty
                    [ (IPNone,   0)
                    , (IPActive, 1)
                    , (IPDown ,  2)
                    ]

data IPVerb = IPCreate
            | IPUpdate
            | IPDelete
  deriving (Show, Eq, Ord)

ipVerbMap :: Bimap IPVerb Word64
ipVerbMap = foldl (\bm (s, v) -> B.insert s v bm)
                    B.empty
                    [ (IPCreate, 0)
                    , (IPUpdate, 1)
                    , (IPDelete, 2)
                    ]

ipPayloadMap :: Bimap () Word64
ipPayloadMap = B.singleton () 1
