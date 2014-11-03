module Multipac where

import Control.Monad.Eff
import Debug.Trace (trace)
import Data.Maybe
import Data.Array (map)
import Data.Foldable
import Data.Traversable

import Types
import LevelMap
import Utils

testMap :: Maybe [[Block]]
testMap = concatTiles basicMap

testMapString :: Maybe [[String]]
testMapString = fmap (fmap (fmap show)) testMap

testMapRows :: Maybe [String]
testMapRows = fmap (fmap mconcat) testMapString

main = case testMapRows of
    Just rs -> sequence_ (map trace rs)
    Nothing -> trace "oh dear"
