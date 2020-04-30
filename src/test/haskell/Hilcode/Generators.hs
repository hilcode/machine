module Hilcode.Generators where

import           Data.Text (Text)
import           Hedgehog (Gen)

import qualified Data.Set
import qualified Data.Text
import qualified Hedgehog.Gen
import qualified Hedgehog.Range

import           Hilcode.Machine.AtomSource
import           Hilcode.Machine.Internal.Metadata
import           Hilcode.Machine.Internal.Match

generateAtomSource :: Gen (AtomSource Char)
generateAtomSource = Hedgehog.Gen.frequency
    [ (1, pure (AtomSource []))
    , (9, AtomSource <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) Hedgehog.Gen.alpha)
    ]

generateMatch :: Gen (Match Char Text)
generateMatch = Match <$> generateText <*> generateAtomSource

generateIndex :: Gen Index
generateIndex = Index <$> generateInt

generateNullable :: Gen Nullable
generateNullable = Nullable <$> Hedgehog.Gen.bool

generateFirstPos :: Gen FirstPos
generateFirstPos = Hedgehog.Gen.frequency
    [ (1, pure (FirstPos Data.Set.empty))
    , (9, FirstPos . Data.Set.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 100) generateIndex)
    ]

generateLastPos :: Gen LastPos
generateLastPos = Hedgehog.Gen.frequency
    [ (1, pure (LastPos Data.Set.empty))
    , (9, LastPos . Data.Set.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 100) generateIndex)
    ]

generateText :: Gen Text
generateText = Hedgehog.Gen.frequency
    [ (1, pure "")
    , (9, Data.Text.pack <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) Hedgehog.Gen.alpha)
    ]

generateInt :: Gen Int
generateInt = Hedgehog.Gen.integral (Hedgehog.Range.constant 0 100)
