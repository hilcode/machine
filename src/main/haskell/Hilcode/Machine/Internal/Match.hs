module Hilcode.Machine.Internal.Match where

import           Hilcode.Machine.AtomSource

data Match atom token
    = Match token (AtomSource atom)

instance Show token => Show (Match atom token) where
    show (Match token _) = show token

instance Eq token => Eq (Match atom token) where
    Match lftToken _ == Match rgtToken _ = lftToken == rgtToken

instance Ord token => Ord (Match atom token) where
    Match lftToken _ `compare` Match rgtToken _ = lftToken `compare` rgtToken

instance Ord token => Semigroup (Match atom token) where
    lft@(Match lftToken _) <> rgt@(Match rgtToken _)
        = if lftToken <= rgtToken
            then lft
            else rgt
