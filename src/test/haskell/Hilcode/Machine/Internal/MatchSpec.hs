module Hilcode.Machine.Internal.MatchSpec
( spec
) where

import           Data.Text (Text)
import           Test.Hspec

import qualified Data.Text
import qualified HaskellWorks.Hspec.Hedgehog

import           Hilcode.Checks
import           Hilcode.Generators
import           Hilcode.Machine.AtomSource
import           Hilcode.Machine.Internal.Match

newtype MyText
    = MyText Text
    deriving (Show, Eq)

instance Ord MyText where
    MyText lft `compare` MyText rgt
        = if lftLength == rgtLength
            then lft `compare` rgt
            else rgtLength `compare` lftLength
          where
            lftLength = Data.Text.length lft
            rgtLength = Data.Text.length rgt

data MyToken
    = One
    | Two MyText
    deriving (Show, Eq, Ord)

myToken :: Text -> MyToken
myToken text = Two $ MyText text

lftAtomSource :: AtomSource Char
lftAtomSource = AtomSource "abc"

rgtAtomSource :: AtomSource Char
rgtAtomSource = AtomSource "abcdef"

spec :: Spec
spec =
    describe "Hilcode.Machine.Internal.Match" $ do
        it "Eq Match" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateMatch)
        it "Ord Match" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateMatch)
        it "Semigroup Match" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckSemigroup generateMatch)
        it "Semigroup Match 1" $
            Match One lftAtomSource <> Match One rgtAtomSource `shouldBe` Match One lftAtomSource
        it "Semigroup Match 2" $
            Match One lftAtomSource <> Match (myToken "") rgtAtomSource `shouldBe` Match One lftAtomSource
        it "Semigroup Match 3" $
            Match (myToken "") rgtAtomSource <> Match (myToken "a") rgtAtomSource `shouldBe` Match (myToken "a") rgtAtomSource
        it "Semigroup Match 4" $
            Match (myToken "abc") rgtAtomSource <> Match (myToken "xyz123") rgtAtomSource `shouldBe` Match (myToken "xyz123") rgtAtomSource
