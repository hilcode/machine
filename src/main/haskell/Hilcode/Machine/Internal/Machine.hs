module Hilcode.Machine.Internal.Machine
( module Hilcode.Machine.AtomPredicate
, module Hilcode.Machine.AtomSource
, module Hilcode.Machine.Internal.Machine
, module Hilcode.Machine.Internal.Match
, module Hilcode.Machine.TokenBuilder
) where

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Vector (Vector)

import qualified Data.Foldable
import qualified Data.Map
import qualified Data.Set
import qualified Data.Vector

import           Hilcode.Machine.AtomPredicate
import           Hilcode.Machine.AtomSource
import           Hilcode.Machine.Internal.Program
import           Hilcode.Machine.Internal.Match
import           Hilcode.Machine.Metadata
import           Hilcode.Machine.TokenBuilder
import           Hilcode.Tuples

data Thread atom
    = Thread [atom] (AtomSource atom) Index
    deriving Show

instance Eq atom => Eq (Thread atom) where
    Thread lftAtoms _ lftIndex == Thread rgtAtoms _ rgtIndex = lftAtoms == rgtAtoms && lftIndex == rgtIndex

instance Ord atom => Ord (Thread atom) where
    Thread lftAtoms _ lftIndex `compare` Thread rgtAtoms _ rgtIndex
        = if atomsCompare == EQ
            then lftIndex `compare` rgtIndex
            else atomsCompare
      where
        atomsCompare :: Ordering
        atomsCompare = lftAtoms `compare` rgtAtoms

data Step atom token
    = Step (Vector Index) (AtomPredicate atom)
    | TokenFound (TokenBuilder atom token)
    deriving Show

data Machine atom token
    = Machine (Vector Index) (Vector (Step atom token))

data Pattern atom token
    = Pattern (Instruction () atom token) (TokenBuilder atom token)

runThread :: (Ord token, Show token, Ord atom, Show atom) => Vector (Step atom token) -> AtomSource atom -> Thread atom -> (Set (Thread atom), Maybe (Match atom token))
runThread instructions atomSource (Thread atomsSoFar _ instructionIndex)
    = case instructions `at` instructionIndex of
        Step nextPositions (AtomPredicate _ predicate) -> case atomSource of
            AtomSource []       -> (Data.Set.empty, Nothing)
            AtomSource (atom:_) ->
                let
                    activeThreads = if predicate atom
                        then (Data.Set.fromList . Data.Vector.toList) (Thread (atom : atomsSoFar) atomSource <$> nextPositions)
                        else Data.Set.empty
                    tokenFound = Nothing
                in
                    (activeThreads, tokenFound)
        TokenFound (TokenBuilder _ mkToken)                      ->
            let
                activeThreads = Data.Set.empty
                tokenFound = Just (Match (mkToken (reverse atomsSoFar)) atomSource)
            in
                (activeThreads, tokenFound)

run :: forall atom token . (Ord token, Show token, Ord atom, Show atom) => Machine atom token -> AtomSource atom -> Maybe (Match atom token)
run (Machine startPositions instructions) atomSource
    = go (startThreads, Nothing) atomSource
  where
    startThreads :: Set (Thread atom)
    startThreads = Data.Set.fromList (Thread [] atomSource <$> Data.Vector.toList startPositions)
    go :: Show token => (Set (Thread atom), Maybe (Match atom token)) -> AtomSource atom -> Maybe (Match atom token)
    go (activeThreads, tokenFound) atomSource
        | Data.Set.null activeThreads = tokenFound
        | otherwise                   = go (readAtom tokenFound activeThreads atomSource) (dropAtom atomSource)
      where
        dropAtom :: AtomSource atom -> AtomSource atom
        dropAtom atomSource = case atomSource of
            AtomSource []        -> atomSource
            AtomSource (_:atoms) -> AtomSource atoms
        runThreads :: Set (Thread atom) -> AtomSource atom -> [(Set (Thread atom), Maybe (Match atom token))]
        runThreads activeThreads atomSource = runThread instructions atomSource <$> Data.Set.toList activeThreads
        joinThreads :: [(Set (Thread atom), Maybe (Match atom token))] -> (Set (Thread atom), Maybe (Match atom token))
        joinThreads = Data.Foldable.foldl' (<>) (Data.Set.empty, Nothing)
        readAtom :: Maybe (Match atom token) -> Set (Thread atom) -> AtomSource atom -> (Set (Thread atom), Maybe (Match atom token))
        readAtom tokenFound activeThreads atomSource = (Data.Set.empty, tokenFound) <> joinThreads (runThreads activeThreads atomSource)


instance Show (Machine atom token) where
    show (Machine startIndices instructions) = "Machine (" <> show startIndices <> ")" <> fst (Data.Foldable.foldl' showInstruction ("\n", 0) instructions)
      where
        showInstruction :: (String, Int) -> Step atom token -> (String, Int)
        showInstruction (accumulator, index) instruction = (accumulator <> "    " <> showIndex index <> ") " <> show instruction <> "\n", index + 1)
        showIndex :: Int -> String
        showIndex index
            | index < 10  = "  " <> show index
            | index < 100 = " " <> show index
            | otherwise   = show index

compile :: forall atom token . Program () atom token -> Machine atom token
compile program = Machine startPositions (toInstructionVector (makeInstructionMap decoratedTopNode Data.Map.empty))
  where
    topNode :: Node () atom token
    topNode = case transform program of
        Program instructions -> instructionsToNode (NodeAnd ()) instructions
    decoratedTopNode :: Node (Nullable, FirstPos, LastPos) atom token
    decoratedTopNode = decorate topNode
    followPosMap :: Map Index (Set Index)
    followPosMap = followPos Data.Map.empty decoratedTopNode
    makeInstructionMap :: Node (Nullable, FirstPos, LastPos) atom token -> Map Index (Step atom token) -> Map Index (Step atom token)
    makeInstructionMap (NodeTerminal _ instructionIndex predicate) map = Data.Map.insert instructionIndex (Step (convertSetToVector (getFromMap followPosMap instructionIndex)) predicate) map
    makeInstructionMap (NodeSuccess _ instructionIndex mkToken) map    = Data.Map.insert instructionIndex (TokenFound mkToken) map
    makeInstructionMap (NodeEmpty _) map                               = map
    makeInstructionMap (NodeAnd _ lft rgt) map                         = makeInstructionMap rgt (makeInstructionMap lft map)
    makeInstructionMap (NodeOr _ lft rgt) map                          = makeInstructionMap rgt (makeInstructionMap lft map)
    makeInstructionMap (NodeMany _ node) map                           = makeInstructionMap node map
    toInstructionVector :: Map Index (Step atom token) -> Vector (Step atom token)
    toInstructionVector = Data.Vector.fromList . Data.Map.elems
    startPositions :: Vector Index
    startPositions = convertFirstPosToVector (_2 (getMetadata decoratedTopNode))
