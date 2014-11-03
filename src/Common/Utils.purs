module Utils where

import Data.Maybe
import Data.Array
import Data.Traversable (sequence)

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

collectMaybes :: forall a. [[Maybe a]] -> Maybe [[a]]
collectMaybes = map sequence >>> sequence

fmap :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
fmap = (<$>)
