module Hilcode.Machine
( AtomSource
, Instruction
, Machine
, Pattern
, TokenBuilder
, tokenize
, makeAtomSource
, makeMachine
, makeTokenBuilder
, makePattern
, atom
, atoms
, success
, atLeastOnce
, loop
, optional
, select
) where

import           Data.Vector (Vector)
import           Data.Text (Text)

import qualified Data.Vector

import           Hilcode.Machine.Internal.Machine
import           Hilcode.Machine.Internal.Program
import           Hilcode.Misc.Collections

makeMachine :: Vector (Pattern atom token) -> Machine atom token
makeMachine patterns = compile (tokenizer patterns)

tokenizer :: forall atom token . Vector (Pattern atom token) -> Program () atom token
tokenizer patterns
    | Data.Vector.null patterns = error "No token patterns."
    | tokenCount == 1           = Program compiledPatterns
    | otherwise                 = Program (Data.Vector.singleton (Select (compiledPatterns ! 0) (compiledPatterns ! 1) (Data.Vector.unsafeTail (Data.Vector.unsafeTail compiledPatterns))))
  where
    tokenCount :: Int
    tokenCount = Data.Vector.length patterns
    compiledPatterns :: Vector (Instruction () atom token)
    compiledPatterns = compilePattern <$> patterns

tokenize :: (Ord atom, Show atom, Ord token, Show token) => Machine atom token -> AtomSource atom -> [token]
tokenize _       (AtomSource []) = []
tokenize machine atomSource      = case run machine atomSource of
    Nothing                             -> error "No token found."
    Just (Match token atomSource') -> token : tokenize machine atomSource'

makePattern :: Instruction () atom token -> TokenBuilder atom token -> Pattern atom token
makePattern = Pattern

compilePattern :: Pattern atom token -> Instruction () atom token
compilePattern (Pattern instruction tokenBuilder) = Instructions instruction (Success () tokenBuilder) Data.Vector.empty

atom :: Text -> (atom -> Bool) -> Instruction () atom token
atom text predicate = Instruction () (AtomPredicate text predicate)

atoms :: Instruction () atom token -> Instruction () atom token -> [Instruction () atom token] -> Instruction () atom token
atoms one two rest = Instructions one two (Data.Vector.fromList rest)

success :: Text -> ([atom] -> token) -> Instruction () atom token
success text tokenBuilder = Success () (makeTokenBuilder text tokenBuilder)

atLeastOnce :: Instruction () atom token -> Instruction () atom token
atLeastOnce instruction = Instructions instruction (loop instruction) Data.Vector.empty

loop :: Instruction () atom token -> Instruction () atom token
loop = Loop

optional :: Instruction () atom token -> Instruction () atom token
optional = Optional

select :: Instruction () atom token -> Instruction () atom token -> [Instruction () atom token] -> Instruction () atom token
select one two rest = Select one two (Data.Vector.fromList rest)
