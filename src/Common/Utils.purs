module Utils where

import Data.Maybe
import Data.Array
import Data.Traversable (sequence)
import Data.Foldable (for_)
import Prelude.Unsafe (unsafeIndex)

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

eachWithIndex_ :: forall a b m. (Applicative m) =>
  [a] -> (a -> Number -> m b) -> m Unit
eachWithIndex_ as f =
  for_ (0 .. length as - 1) $ \n ->
    f (unsafeIndex as n) n

whenJust :: forall a f. (Applicative f) => Maybe a -> (a -> f Unit) -> f Unit
whenJust mx f = maybe (pure unit) f mx

foreign import unshift
  "function unshift(x) { \
  \  return function(xs) { \
  \    var ys = xs.slice(); \
  \    ys.unshift(x); \
  \    return ys; \
  \  } \
  \}" :: forall a. a -> [a] -> [a]
