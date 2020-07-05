module Hilcode.Machine.Internal.Program where

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Vector (Vector)

import qualified Data.Vector

import           Hilcode.Machine.AtomPredicate
import           Hilcode.Machine.Metadata
import           Hilcode.Machine.TokenBuilder
import           Hilcode.State
import           Hilcode.Tuples

data Instruction metadata atom token
    = Instruction  metadata (AtomPredicate atom)
    | Success      metadata (TokenBuilder atom token)
    | Instructions (Instruction metadata atom token) (Instruction metadata atom token) (Vector (Instruction metadata atom token))
    | Loop         (Instruction metadata atom token)
    | Optional     (Instruction metadata atom token)
    | Select       (Instruction metadata atom token) (Instruction metadata atom token) (Vector (Instruction metadata atom token))
    deriving (Show, Eq)

newtype Program metadata atom token
    = Program (Vector (Instruction metadata atom token))
    deriving (Show, Eq)

transform :: forall atom token . Program () atom token -> Program Index atom token
transform (Program instructions) = (Program . decorateInstructions) instructions
  where
    decorateInstructions :: Vector (Instruction () atom token) -> Vector (Instruction Index atom token)
    decorateInstructions instructions = execState (traverse setIndex instructions) zero

setIndex :: Instruction () atom token -> State Index (Instruction Index atom token)
setIndex instruction
    = case instruction of
        Instruction _ predicate -> do
            instructionIndex <- get
            put (increment instructionIndex)
            pure (Instruction instructionIndex predicate)
        Success _ mkToken -> do
            instructionIndex <- get
            put (increment instructionIndex)
            pure (Success instructionIndex mkToken)
        Instructions instructionOne instructionTwo instructions -> do
            (instructionOneWithIndex, instructionTwoWithIndex) <- setIndices instructionOne instructionTwo
            instructionsWithIndex <- traverse setIndex instructions
            pure (Instructions instructionOneWithIndex instructionTwoWithIndex instructionsWithIndex)
        Loop instruction -> do
            instructionWithIndex <- setIndex instruction
            pure (Loop instructionWithIndex)
        Optional instruction -> do
            instructionWithIndex <- setIndex instruction
            pure (Optional instructionWithIndex)
        Select instructionOne instructionTwo instructions -> do
            (instructionOneWithIndex, instructionTwoWithIndex) <- setIndices instructionOne instructionTwo
            instructionsWithIndex <- traverse setIndex instructions
            pure (Select instructionOneWithIndex instructionTwoWithIndex instructionsWithIndex)
  where
    setIndices :: Instruction () atom token -> Instruction () atom token -> State Index (Instruction Index atom token, Instruction Index atom token)
    setIndices instructionOne instructionTwo = do
        instructionOneWithIndex <- setIndex instructionOne
        instructionTwoWithIndex <- setIndex instructionTwo
        pure (instructionOneWithIndex, instructionTwoWithIndex)

data Node metadata atom token
    = NodeTerminal metadata Index (AtomPredicate atom)
    | NodeSuccess metadata Index (TokenBuilder atom token)
    | NodeEmpty metadata
    | NodeAnd metadata (Node metadata atom token) (Node metadata atom token)
    | NodeOr metadata (Node metadata atom token) (Node metadata atom token)
    | NodeMany metadata (Node metadata atom token)
    deriving Show

getMetadata :: Node metadata atom token -> metadata
getMetadata (NodeTerminal metadata _ _) = metadata
getMetadata (NodeSuccess metadata _ _)  = metadata
getMetadata (NodeEmpty metadata)        = metadata
getMetadata (NodeAnd metadata _ _)      = metadata
getMetadata (NodeOr metadata _ _)       = metadata
getMetadata (NodeMany metadata _)       = metadata

decorate :: Node () atom token -> Node (Nullable, FirstPos, LastPos) atom token
decorate (NodeTerminal _ instructionIndex predicate) = NodeTerminal (notNullable, firstPos instructionIndex, lastPos instructionIndex) instructionIndex predicate
decorate (NodeSuccess _ instructionIndex mkToken)    = NodeSuccess (notNullable, firstPos instructionIndex, lastPos instructionIndex) instructionIndex mkToken
decorate (NodeEmpty _)                               = NodeEmpty (nullable, noFirstPos, noLastPos)
decorate (NodeAnd _ lft rgt)                         = NodeAnd (nullability, firstPos, lastPos) lft' rgt'
  where
    lft' = decorate lft
    metadataLft' = getMetadata lft'
    rgt' = decorate rgt
    metadataRgt' = getMetadata rgt'
    nullability = _1 metadataLft' &&& _1 metadataRgt'
    firstPos = if _1 metadataLft' == notNullable then _2 metadataLft' else _2 metadataLft' <> _2 metadataRgt'
    lastPos = if _1 metadataRgt' == notNullable then _3 metadataRgt' else _3 metadataLft' <> _3 metadataRgt'
decorate (NodeOr _ lft rgt)                            = NodeOr (nullability, firstPos, lastPos) lft' rgt'
  where
    lft' = decorate lft
    metadataLft' = getMetadata lft'
    rgt' = decorate rgt
    metadataRgt' = getMetadata rgt'
    nullability = _1 metadataLft' ||| _1 metadataRgt'
    firstPos = _2 metadataLft' <> _2 metadataRgt'
    lastPos = _3 metadataLft' <> _3 metadataRgt'
decorate (NodeMany _ node)                             = NodeMany (nullable, firstPos, lastPos) node'
  where
    node' = decorate node
    metadataNode' = getMetadata node'
    firstPos = _2 metadataNode'
    lastPos = _3 metadataNode'

followPos :: Map Index (Set Index) -> Node (Nullable, FirstPos, LastPos) atom token -> Map Index (Set Index)
followPos map (NodeAnd _ lft rgt)  = setFollowPos lastPosLft (followPos (followPos map lft) rgt) firstPosRgt
  where
    lastPosLft :: LastPos
    lastPosLft = _3 (getMetadata lft)
    firstPosRgt :: FirstPos
    firstPosRgt = _2 (getMetadata rgt)
followPos map (NodeMany _ node)    = setFollowPos lastPosNode (followPos map node) firstPosNode
  where
    metadata :: (Nullable, FirstPos, LastPos)
    metadata = getMetadata node
    lastPosNode :: LastPos
    lastPosNode = _3 metadata
    firstPosNode :: FirstPos
    firstPosNode = _2 metadata
followPos map (NodeOr _ lft rgt)   = followPos (followPos map lft) rgt
followPos map (NodeTerminal _ _ _) = map
followPos map (NodeSuccess _ _ _)  = map
followPos map (NodeEmpty _)        = map

instructionsToNode :: (Node () atom token -> Node () atom token -> Node () atom token) -> Vector (Instruction Index atom token) -> Node () atom token
instructionsToNode builder instructions
    | Data.Vector.null instructions        = NodeEmpty ()
    | Data.Vector.length instructions == 1 = instructionToNode (Data.Vector.unsafeHead instructions)
    | otherwise                            = builder (instructionToNode (Data.Vector.unsafeHead instructions)) (instructionsToNode builder (Data.Vector.unsafeTail instructions))

instructionToNode :: Instruction Index atom token -> Node () atom token
instructionToNode (Instruction index instruction) = NodeTerminal () index instruction
instructionToNode (Instructions instructionOne instructionTwo instructions)
    | Data.Vector.null instructions = NodeAnd () (instructionToNode instructionOne) (instructionToNode instructionTwo)
    | otherwise                     = NodeAnd () (instructionToNode instructionOne) (NodeAnd () (instructionToNode instructionTwo) (instructionsToNode (NodeAnd ()) instructions))
instructionToNode (Loop instruction)     = NodeMany () (instructionToNode instruction)
instructionToNode (Optional instruction) = NodeOr () (NodeEmpty ()) (instructionToNode instruction)
instructionToNode (Select instructionOne instructionTwo instructions)
    | Data.Vector.null instructions = NodeOr () (instructionToNode instructionOne) (instructionToNode instructionTwo)
    | otherwise                     = NodeOr () (instructionToNode instructionOne) (NodeOr () (instructionToNode instructionTwo) (instructionsToNode (NodeOr ()) instructions))
instructionToNode (Success index mkToken) = NodeSuccess () index mkToken
