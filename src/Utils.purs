module Utils where

import Data.Maybe
import Data.Array

replicate :: forall a. Number -> a -> [a]
replicate n x =
    if n <= 0
        then []
        else x : replicate (n - 1) x

collect :: forall a b. (a -> Maybe b) -> [a] -> [b]
collect f = map f >>> catMaybes

transpose :: forall a. [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs) : xss) =
    (x : collect head xss) : transpose (xs : collect tail xss)
