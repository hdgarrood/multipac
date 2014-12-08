module Utils where

import Debug.Trace
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.Either
import Data.JSON (decode)
import qualified Data.Map as M
import Data.Traversable (sequence)
import Data.Monoid.All
import Data.Foldable (Foldable, for_, foldMap, foldr, foldl)
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

applyN :: forall a. Number -> (a -> a) -> a -> a
applyN = go id
  where
  go f n _ | n <= 0 = f
  go f n g = go (f >>> g) (n - 1) g

foreign import error
  """
  function error(msg) {
    throw new Error(msg)
  }
  """ :: forall a. String -> a

zipNumbers :: forall a. [a] -> [Tuple Number a]
zipNumbers xs = zip (range 0 (length xs - 1)) xs

deleteWhere :: forall k v. (Ord k) =>
  (k -> v -> Boolean) -> M.Map k v -> M.Map k v
deleteWhere pred map =
  foldr go map (M.toList map)
  where
  go (Tuple k v) m =
    if pred k v
       then M.delete k m
       else m

unionWith :: forall k v. (Ord k) => (v -> v -> v) -> M.Map k v -> M.Map k v -> M.Map k v
unionWith f m1 m2 = foldl go m2 (M.toList m1)
 where
 go m (Tuple k v) = M.alter (Just <<< maybe v (f v)) k m



foreign import data Process :: !

foreign import chdir
  """
  function chdir(path) {
    return function() {
      process.chdir(path)
    }
  }
  """ :: forall e. String -> Eff (process :: Process | e) Unit

foreign import getEnvImpl
  """
  function getEnvImpl(just, nothing, key) {
    return function() {
      var v = process.env[key]
      return v ? just(v) : nothing
    }
  }
  """ :: forall e a.
  Fn3
    (a -> Maybe a)
    (Maybe a)
    String
    (Eff (process :: Process | e) (Maybe String))

getEnv :: forall e.
  String -> Eff (process :: Process | e) (Maybe String)
getEnv key =
  runFn3 getEnvImpl Just Nothing key

parseNumber :: String -> Maybe Number
parseNumber = decode

portOrDefault :: forall e.
  Number -> Eff (process :: Process | e) Number
portOrDefault default = do
  port <- getEnv "PORT"
  return $ fromMaybe default (port >>= parseNumber)

foreign import traceP
  """
  function traceP(message) {
    return function(value) {
      console.log(message);
      return value;
    }
  }""" :: forall a. String -> a -> a

tracePM :: forall m. (Monad m) => String -> m Unit
tracePM msg = traceP msg $ return unit
