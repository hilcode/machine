module Hilcode.Machine.AtomSourceSpec
( spec
) where

import           Data.Text (Text)
import           Test.Hspec

import qualified HaskellWorks.Hspec.Hedgehog

import           Hilcode.Checks
import           Hilcode.Generators
import           Hilcode.Machine.AtomSource

spec :: Spec
spec =
    describe "Hilcode.Machine.AtomSource" $ do
        it "makeAtomSource String []" $
            makeAtomSource ("" :: String) `shouldBe` AtomSource ([] :: [Char])
        it "makeAtomSource Text []" $
            makeAtomSource ("" :: Text) `shouldBe` AtomSource ([] :: [Char])
        it "makeAtomSource String [a]" $
            makeAtomSource ("a" :: String) `shouldBe` AtomSource (['a'] :: [Char])
        it "makeAtomSource Text [a]" $
            makeAtomSource ("a" :: Text) `shouldBe` AtomSource (['a'] :: [Char])
        it "makeAtomSource String [ab]" $
            makeAtomSource ("ab" :: String) `shouldBe` AtomSource (['a', 'b'] :: [Char])
        it "makeAtomSource Text [ab]" $
            makeAtomSource ("ab" :: Text) `shouldBe` AtomSource (['a', 'b'] :: [Char])
        it "Eq AtomSource" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateAtomSource)
        it "Ord AtomSource" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateAtomSource)
