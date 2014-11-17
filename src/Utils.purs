module Utils where

import Debug.Trace
import Data.Function
import Data.Maybe
import Data.Array
import Data.Either
import Data.Traversable (sequence)
import Data.Foldable (for_)
import Prelude.Unsafe (unsafeIndex)
import Control.Monad.Eff

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

-- this could be generalised to Applicative, but is restricted to Eff for
-- performance reasons.
foreign import eachWithIndex_
  "function eachWithIndex_(xs) {\
  \  return function(f) {\
  \    return function() {\
  \      var i = 0; \
  \      var l = xs.length; \
  \      for (var i = 0; i < l; i++) {\
  \        f(xs[i])(i)(); \
  \      } \
  \    } \
  \  }\
  \}":: forall a b e. [a] -> (a -> Number -> Eff e b) -> Eff e Unit

-- eachWithIndex_ as f =
--   for_ (0 .. length as - 1) $ \n ->
--     f (unsafeIndex as n) n

whenJust :: forall a f. (Applicative f) => Maybe a -> (a -> f Unit) -> f Unit
whenJust mx f = maybe (pure unit) f mx

(>>) :: forall a b m. (Monad m) => m a -> m b -> m b
(>>) a b = a >>= (\_ -> b)

foreign import unshift
  "function unshift(x) { \
  \  return function(xs) { \
  \    var ys = xs.slice(); \
  \    ys.unshift(x); \
  \    return ys; \
  \  } \
  \}" :: forall a. a -> [a] -> [a]

foreign import traceTimeImpl
  """
  function traceTimeImpl(trace, msg) {
    return trace((new Date()) + ' ' + msg)
  }
  """ :: forall e.
  Fn2
    (String -> Eff (trace :: Trace | e) Unit)
    String
    (Eff (trace :: Trace | e) Unit)

traceTime :: forall e.  String -> Eff (trace :: Trace | e) Unit
traceTime msg = runFn2 traceTimeImpl trace msg

fromEither :: forall a b. Either a b -> Maybe b
fromEither (Left _) = Nothing
fromEither (Right x) = Just x
