module Hilcode.Machine.Internal.Metadata where

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Vector (Vector)

import qualified Data.Foldable
import qualified Data.Map
import qualified Data.Set
import qualified Data.Vector

import           Hilcode.Misc.Collections

newtype Index
    = Index Int
    deriving (Eq, Ord)

instance Show Index where
    show (Index index) = show index

zero :: Index
zero = Index 0

increment :: Index -> Index
increment (Index i) = Index (i + 1)

newtype Nullable = Nullable Bool
    deriving (Show, Eq)

nullable :: Nullable
nullable = Nullable True

notNullable :: Nullable
notNullable = Nullable False

(&&&) :: Nullable -> Nullable -> Nullable
Nullable True &&& Nullable True = Nullable True
_             &&& _             = Nullable False

(|||) :: Nullable -> Nullable -> Nullable
Nullable False ||| Nullable False = Nullable False
_              ||| _              = Nullable True

newtype FirstPos
    = FirstPos (Set Index)
    deriving (Show, Eq)

instance Semigroup FirstPos where
    (FirstPos lft) <> (FirstPos rgt) = FirstPos (lft <> rgt)

firstPos :: Index -> FirstPos
firstPos instructionIndex = FirstPos (Data.Set.singleton instructionIndex)

noFirstPos :: FirstPos
noFirstPos = FirstPos Data.Set.empty

newtype LastPos
    = LastPos (Set Index)
    deriving (Show, Eq)

instance Semigroup LastPos where
    (LastPos lft) <> (LastPos rgt) = LastPos (lft <> rgt)

lastPos :: Index -> LastPos
lastPos instructionIndex = LastPos (Data.Set.singleton instructionIndex)

noLastPos :: LastPos
noLastPos = LastPos Data.Set.empty

convertFirstPosToVector :: FirstPos -> Vector Index
convertFirstPosToVector (FirstPos positions) = convertSetToVector positions

convertSetToVector :: Set a -> Vector a
convertSetToVector = Data.Vector.fromList . Data.Set.toList

getFromMap :: Map Index (Set Index) -> Index -> Set Index
getFromMap map position
        | Data.Map.member position map = map ! position
        | otherwise                    = Data.Set.empty

at :: Vector a -> Index -> a
at as (Index index) = as ! index

setFollowPos :: LastPos -> Map Index (Set Index) -> FirstPos -> Map Index (Set Index)
setFollowPos (LastPos positions) map (FirstPos nextPositions) = createFollowPos nextPositions (Data.Set.toList positions) map

createFollowPos :: Set Index -> [Index] -> Map Index (Set Index) -> Map Index (Set Index)
createFollowPos nextPositions positions map = Data.Foldable.foldl' (putFollowPos nextPositions) map positions

putFollowPos :: Set Index -> Map Index (Set Index) -> Index -> Map Index (Set Index)
putFollowPos nextPositions map position = Data.Map.insert position (Data.Set.union nextPositions (getFromMap map position)) map
