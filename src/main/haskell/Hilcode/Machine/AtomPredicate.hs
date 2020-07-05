module Hilcode.Machine.AtomPredicate where

import           Data.Text (Text)

import qualified Data.Text

data AtomPredicate atom
    = AtomPredicate Text (atom -> Bool)

instance Eq (AtomPredicate atom) where
    (AtomPredicate lft _) == (AtomPredicate rgt _) = lft == rgt

instance Show (AtomPredicate atom) where
    show (AtomPredicate text _) = "(" <> Data.Text.unpack text <> ")"
