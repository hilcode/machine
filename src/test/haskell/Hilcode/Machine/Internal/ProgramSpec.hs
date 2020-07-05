module Hilcode.Machine.Internal.ProgramSpec
( spec
) where

import           Data.Text (Text)
import           Test.Hspec

import qualified Data.Vector
-- import qualified HaskellWorks.Hspec.Hedgehog

-- import           Hilcode.Checks
-- import           Hilcode.Generators
import           Hilcode.Machine.AtomPredicate
import           Hilcode.Machine.Internal.Metadata
import           Hilcode.Machine.Internal.Program

always :: AtomPredicate Char
always = AtomPredicate "" (const True)

instruction :: Instruction () Char Text
instruction = Instruction () always

spec :: Spec
spec =
    describe "Hilcode.Machine.Internal.Program" $
        it "transform" $
            transform (Program (Data.Vector.singleton instruction)) `shouldBe` Program (Data.Vector.singleton (Instruction (Index 0) always))
