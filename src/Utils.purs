module Utils where

import Data.Traversable (sequence)
import Data.Array

replicate :: forall a. Number -> a -> [a]
replicate n x =
    if x <= 0
        then []
        else x : replicate (n - 1) x

transpose :: forall a. [[a]] -> [[a]]
transpose = sequence
