module Hilcode.Misc.Collections where

import           Data.Map (Map)
import           Data.Vector (Vector)

import qualified Data.Foldable
import qualified Data.Map
import qualified Data.Vector

class Get collection k v | collection -> k, collection -> v where
    (!) :: collection -> k -> v

instance Get (Vector a) Int a where
    (!) = (Data.Vector.!)

instance Ord k => Get (Map k v) k v where
    (!) = (Data.Map.!)

forEachElementIn :: Foldable collection => collection element -> (result -> element -> result) -> result -> result
forEachElementIn collection function zero = Data.Foldable.foldl' function zero collection

apply :: result -> result
apply loop = loop

startingWith :: (result -> result) -> result -> result
startingWith loop = loop
