import Test.Hspec

import qualified Hilcode.Machine.AtomPredicateSpec
import qualified Hilcode.Machine.AtomSourceSpec
import qualified Hilcode.Machine.Internal.MetadataSpec
import qualified Hilcode.Machine.Internal.MatchSpec
import qualified Hilcode.Machine.Internal.ProgramSpec
import qualified Hilcode.Machine.TokenBuilderSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hilcode.Machine.AtomPredicate"     Hilcode.Machine.AtomPredicateSpec.spec
    describe "Hilcode.Machine.AtomSource"        Hilcode.Machine.AtomSourceSpec.spec
    describe "Hilcode.Machine.Internal.Metadata" Hilcode.Machine.Internal.MetadataSpec.spec
    describe "Hilcode.Machine.Internal.Match"    Hilcode.Machine.Internal.MatchSpec.spec
    describe "Hilcode.Machine.Internal.Program"  Hilcode.Machine.Internal.ProgramSpec.spec
    describe "Hilcode.Machine.TokenBuilder"      Hilcode.Machine.TokenBuilderSpec.spec
