module Hilcode.Machine.TokenBuilder where

import           Data.Text (Text)

import qualified Data.Text

data TokenBuilder atom token
    = TokenBuilder Text ([atom] -> token)

makeTokenBuilder :: Text -> ([atom] -> token) -> TokenBuilder atom token
makeTokenBuilder = TokenBuilder

instance Eq (TokenBuilder atom token) where
    TokenBuilder lft _ == TokenBuilder rgt _ = lft == rgt

instance Show (TokenBuilder atom token) where
    show (TokenBuilder text _) = "(" <> Data.Text.unpack text <> ")"
