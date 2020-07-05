module Hilcode.Machine.TokenBuilderSpec
( spec
) where

import           Test.Hspec

import qualified HaskellWorks.Hspec.Hedgehog

import           Hilcode.Checks
import           Hilcode.Generators
import           Hilcode.Machine.TokenBuilder

spec :: Spec
spec =
    describe "Hilcode.Machine.TokenBuilder" $ do
        it "Show" $
            show (makeTokenBuilder "TokenBuilder" (const undefined)) `shouldBe` "(TokenBuilder)"
        it "Eq TokenBuilder" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateTokenBuilder)
