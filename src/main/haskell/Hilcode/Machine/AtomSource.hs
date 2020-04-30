module Hilcode.Machine.AtomSource where

import           Data.Text (Text)

import qualified Data.Text

newtype AtomSource atom
    = AtomSource [atom]
    deriving (Show, Eq, Ord)

class MakeAtomSource input atom where
    makeAtomSource :: input -> AtomSource atom

instance MakeAtomSource String Char where
    makeAtomSource = AtomSource

instance MakeAtomSource Text Char where
    makeAtomSource text = AtomSource (Data.Text.unpack text)
