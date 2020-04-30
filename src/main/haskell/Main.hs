module Main where

import Prelude hiding (iterate, (&&), (||), String)

import           Data.Text (Text)

import qualified Data.Text
import qualified Data.Vector

import           Hilcode.Machine

main :: IO ()
main = print $ tokenize jsonTokenizer atomSource

jsonTokenizer :: Machine Char Token
jsonTokenizer = makeMachine (Data.Vector.fromList [whitespace, leftBrace, rightBrace, leftBracket, rightBracket])

atomSource :: AtomSource Char
atomSource = makeAtomSource text
  where
    text :: Text
    text = " {  { [ ]}}\r\n\r   "

data Token
    = LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Whitespace Text
    deriving (Show, Eq)

instance Ord Token where
    Whitespace lft `compare` Whitespace rgt = rgt `compare` lft
    lft            `compare` rgt            = toInt lft `compare` toInt rgt
      where
        toInt :: Token -> Int
        toInt LeftBrace      = 0
        toInt RightBrace     = 1
        toInt LeftBracket    = 2
        toInt RightBracket   = 3
        toInt (Whitespace _) = 4

whitespace :: Pattern Char Token
whitespace = makePattern (atLeastOnce (atom "isWhitespace" (\token -> (== token) `Data.Text.any` whitespaceChars))) (makeTokenBuilder "Whitespace . Data.Text.pack" (Whitespace . Data.Text.pack))
  where
    whitespaceChars :: Text
    whitespaceChars = " \t\n\r"

leftBrace :: Pattern Char Token
leftBrace = makePattern (atom "== '{'" (== '{')) (makeTokenBuilder "const LeftBrace" (const LeftBrace))

rightBrace :: Pattern Char Token
rightBrace = makePattern (atom "== '}'" (== '}')) (makeTokenBuilder "const RightBrace" (const RightBrace))

leftBracket :: Pattern Char Token
leftBracket = makePattern (atom "== '['" (== '[')) (makeTokenBuilder "const LeftBracket" (const LeftBracket))

rightBracket :: Pattern Char Token
rightBracket = makePattern (atom "== ']'" (== ']')) (makeTokenBuilder "const RightBracket" (const RightBracket))
