module Ceilometer.Types.Base where

import           Control.Isomorphism.Partial.Iso
import           Data.Bimap                      (Bimap)
import qualified Data.Bimap                      as B
import           Data.Word

-- |Iso generator
generateIsoFromBimap :: (Ord a, Ord b) => Bimap a b -> Iso a b
generateIsoFromBimap bm = unsafeMakeIso (`B.lookup` bm) (`B.lookupR` bm)

data Endpoint = Start
              | End
  deriving (Show, Eq, Ord)

endpointMap :: Bimap Endpoint Word64
endpointMap = foldl (\bm (s, v) -> B.insert s v bm)
                    B.empty
                    [ (Start, 1)
                    , (End,   2)
                    ]

endpointIso :: Iso Endpoint Word64
endpointIso = generateIsoFromBimap endpointMap
