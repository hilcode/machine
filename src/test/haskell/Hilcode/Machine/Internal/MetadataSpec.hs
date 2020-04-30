module Hilcode.Machine.Internal.MetadataSpec
( spec
) where

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Vector (Vector)
import           Hedgehog
import           Test.Hspec

import qualified Data.Map
import qualified Data.Set
import qualified Data.Vector
import qualified HaskellWorks.Hspec.Hedgehog

import           Hilcode.Checks
import           Hilcode.Generators
import           Hilcode.Machine.Internal.Metadata

propertyNullableAnd :: Property
propertyNullableAnd = property $ do
    (lft@(Nullable lft'), rgt@(Nullable rgt')) <- generateTuple generateNullable
    assert $ (lft &&& rgt) == Nullable (lft' && rgt')

propertyNullableOr :: Property
propertyNullableOr = property $ do
    (lft@(Nullable lft'), rgt@(Nullable rgt')) <- generateTuple generateNullable
    assert $ (lft ||| rgt) == Nullable (lft' || rgt')

five :: Index
five = Index 5

ten :: Index
ten = Index 10

set :: Set Index
set = Data.Set.fromList [zero, five, ten]

vector :: Vector Index
vector = Data.Vector.fromList [zero, five, ten]

instructionIndexMap :: Map Index (Set Index)
instructionIndexMap = Data.Map.fromList [(five, set)]

spec :: Spec
spec =
    describe "Hilcode.Machine.Internal.Metadata" $ do
        it "Eq Index" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateIndex)
        it "Ord Index" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateIndex)
        it "Show zero" $
            show zero `shouldBe` "0"
        it "Show increment zero" $
            show (increment zero) `shouldBe` "1"
        it "Eq Nullable" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateNullable)
        it "nullable" $
            nullable `shouldBe` Nullable True
        it "notNullable" $
            notNullable `shouldBe` Nullable False
        it "Nullable &&&" $
            HaskellWorks.Hspec.Hedgehog.require propertyNullableAnd
        it "Nullable |||" $
            HaskellWorks.Hspec.Hedgehog.require propertyNullableOr
        it "Eq FirstPos" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateFirstPos)
        it "Semigroup FirstPos" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckSemigroup generateFirstPos)
        it "firstPos" $
            firstPos five `shouldBe` FirstPos (Data.Set.singleton five)
        it "noFirstPos" $
            noFirstPos `shouldBe` FirstPos Data.Set.empty
        it "Eq LastPos" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateLastPos)
        it "Semigroup LastPos" $
            HaskellWorks.Hspec.Hedgehog.require (propertyCheckSemigroup generateLastPos)
        it "lastPos" $
            lastPos five `shouldBe` LastPos (Data.Set.singleton five)
        it "noLastPos" $
            noLastPos `shouldBe` LastPos Data.Set.empty
        it "convertFirstPosToVector []" $
            convertFirstPosToVector noFirstPos `shouldBe` Data.Vector.empty
        it "convertFirstPosToVector [5]" $
            convertFirstPosToVector (firstPos five) `shouldBe` Data.Vector.singleton five
        it "convertFirstPosToVector [10, 5, 0]" $
            convertFirstPosToVector (FirstPos $ Data.Set.fromList [ten, five, zero]) `shouldBe` vector
        it "getFromMap 5" $
            getFromMap instructionIndexMap five `shouldBe` set
        it "getFromMap x" $
            getFromMap instructionIndexMap ten `shouldBe` Data.Set.empty
        it "at" $ do
            vector `at` zero `shouldBe` zero
            vector `at` Index 1 `shouldBe` five
            vector `at` Index 2 `shouldBe` ten
        it "setFollowPos empty" $ do
            setFollowPos noLastPos      Data.Map.empty                                        (FirstPos set) `shouldBe` Data.Map.empty
            setFollowPos (lastPos five) Data.Map.empty                                        (FirstPos set) `shouldBe` Data.Map.singleton five set
            setFollowPos (lastPos five) (Data.Map.singleton five (ten `Data.Set.delete` set)) (firstPos ten) `shouldBe` Data.Map.singleton five set
