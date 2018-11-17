module Utils where

import Prelude
import Control.Bind ((>=>))
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Data.Function
import Data.Int as Int
import Data.Maybe
import Data.Tuple
import Data.Array hiding ((..))
import Data.Either
import Data.List as List
import Data.List.ZipList (ZipList(..))
import Data.List.Lazy as LazyList
import Data.Map as M
import Data.Traversable (sequence)
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable, for_, foldMap, foldr, foldl)
import Foreign
import Foreign.Generic
import Foreign.Generic.Class (class GenericEncode, class GenericDecode)
import Foreign.Object as StrMap
import Effect
import Node.Process as Process

iterateN :: forall a. Int -> a -> (a -> a) -> Array a
iterateN n x f =
  if n <= 0
     then []
     else [x] <> iterateN (n - 1) (f x) f


collect :: forall a b. (a -> Maybe b) -> Array a -> Array b
collect f = map f >>> catMaybes

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose =
  toZipList
   >>> map toZipList
   >>> sequence
   >>> map fromZipList
   >>> fromZipList

  where
  toZipList :: forall f a'. (Foldable f) => f a' -> ZipList a'
  toZipList = ZipList <<< LazyList.fromFoldable

  fromZipList :: forall f a'. (Unfoldable f) => ZipList a' -> f a'
  fromZipList = LazyList.toUnfoldable <<< unwrap

collectMaybes :: forall a. Array (Array (Maybe a)) -> Maybe (Array (Array a))
collectMaybes = map sequence >>> sequence

-- this could be generalised to Applicative, but is restricted to Effect for
-- performance reasons.
foreign import eachWithIndex_ :: forall a b e. Array a -> (a -> Number -> Effect b) -> Effect Unit

whenJust :: forall a f. (Applicative f) => Maybe a -> (a -> f Unit) -> f Unit
whenJust mx f = maybe (pure unit) f mx

fromEither :: forall a b. Either a b -> Maybe b
fromEither (Left _) = Nothing
fromEither (Right x) = Just x

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f x
  | n <= 0    = x
  | otherwise = applyN (n - 1) f (f x)

zipIndices :: forall a. Array a -> Array (Tuple Int a)
zipIndices xs = zip (range 0 (length xs - 1)) xs

deleteWhere :: forall k v. (Ord k) =>
  (k -> v -> Boolean) -> M.Map k v -> M.Map k v
deleteWhere pred map =
  foldr go map (M.toUnfoldable map :: Array (Tuple k v))
  where
  go (Tuple k v) m =
    if pred k v
       then M.delete k m
       else m

portOrDefault :: forall e.
  Int -> Effect Int
portOrDefault default = do
  port <- Process.lookupEnv "PORT"
  pure $ fromMaybe default (port >>= Int.fromString)

encode :: forall a rep.
  Generic a rep =>
  GenericEncode rep =>
  a -> String
encode = genericEncodeJSON defaultOptions

decode :: forall a rep.
  Generic a rep =>
  GenericDecode rep =>
  String -> F a
decode = genericDecodeJSON defaultOptions
