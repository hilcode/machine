module Hilcode.MiscSpec
( spec
) where

import Test.Hspec

import Hilcode.Misc.Internal

spec :: Spec
spec = do
    describe "Misc::hello" $
        it "Return \"Hello\"" $
            hello `shouldBe` "Hello"
