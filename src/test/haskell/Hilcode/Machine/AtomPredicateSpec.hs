module Hilcode.Machine.AtomPredicateSpec
( spec
) where

import Test.Hspec

import Hilcode.Machine.AtomPredicate

spec :: Spec
spec =
    describe "Hilcode.Machine.AtomPredicate" $
        it "Show" $
            show (AtomPredicate "AtomPredicate" (const True)) `shouldBe` "(AtomPredicate)"
