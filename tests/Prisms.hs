{-# LANGUAGE RankNTypes #-}

import           Control.Lens          hiding (elements)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Ceilometer.Types
import           Properties
import           SampleData

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
  -- Optics for raw payloads aren't required to be proper, since we are
  -- ignoring some bits of the serialised format (Word64), depending on
  -- the raw payload type.
  --
  -- They should still satisfy prism identity since that is concerned with
  -- the RHS of the prism only, but they don't need to satisfy the "suffcient"
  -- law.

  -- does what it says it does
  describe "CHECK: prism for RAW payload: " $ do
    prop "obey the identity prism law" $ prismIdentity prCompoundEvent
    prop "obey the identity prism law" $ prismIdentity prCompoundPollster

  describe "CHECK: prism for DECODED PAYLOAD FIELD: " $ do
    prop "ENDPOINT      - is a proper prism" $ isPrism pfEndpoint
    prop "VOLUME STATUS - is a proper prism" $ isPrism pfVolumeStatus
    prop "VOLUME VERB   - is a proepr prism" $ isPrism pfVolumeVerb

  describe "CHECK: prism for DECODED PAYLOAD: " $ do
    prop "VOLUME   - is a proper prism" $ isPrism pdVolume
    prop "CPU      - is a proper prism" $ isPrism pdCPU
    prop "VCPU     - is a proper prism" $ isPrism pdInstanceVCPU
    prop "RAM      - is a proper prism" $ isPrism pdInstanceRAM
    prop "DISK     - is a proper prism" $ isPrism pdInstanceDisk
    prop "FLAVOR   - is a proper prism" $ isPrism $ pdInstanceFlavor testFlavors

  -- what it does is what we expect
  describe "REFINE: prism:" $ do
    it "parses/prints values correct to spec for: VOLUME" $
      shouldAllBe (preview pdVolume . view prCompoundEvent) volumePRs volumePDs

    it "parses/prints values correct to spec for: INSTANCE FLAVOR" $
      shouldAllBe (preview (pdInstanceFlavor testFlavors) . view prCompoundPollster) flavorPRs flavorPDs

  where shouldAllBe f xs ys = map f xs `shouldBe` map Just ys


prismIdentity :: Eq b => Prism' a b -> b -> Property
prismIdentity l b = property $ preview l (review l b) == Just b
