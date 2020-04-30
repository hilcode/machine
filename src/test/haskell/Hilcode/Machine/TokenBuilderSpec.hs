module Hilcode.Machine.TokenBuilderSpec
( spec
) where

import           Test.Hspec

import           Hilcode.Machine.TokenBuilder

spec :: Spec
spec =
    describe "Hilcode.Machine.TokenBuilder" $
        it "Show" $
            show (makeTokenBuilder "TokenBuilder" (const undefined)) `shouldBe` "(TokenBuilder)"
