module Utils where

import Debug.Trace
import Data.Function
import Data.Int as Int
import Data.Maybe
import Data.Tuple
import Data.Array hiding ((..), map)
import Data.Either
import Data.JSON (decode)
import Data.List.ZipList (ZipList(..), runZipList)
import Data.List.Lazy as List
import Data.Map as M
import Data.Traversable (sequence)
import Data.Monoid.All
import Data.Foldable (Foldable, for_, foldMap, foldr, foldl)
import Prelude.Unsafe (unsafeIndex)
import Control.Monad.Eff
import Node.Process as Process

(~) :: forall a b. a -> b -> Tuple a b
(~) = Tuple

(..) :: forall s a b c. (Semigroupoid s) => s b c -> s a b -> s a c
(..) = (<<<)

type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t
type TraversalP s a = Traversal s s a a

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
   >>> map runZipList
   >>> runZipList
  where
  toZipList = ZipList <<< List.fromFoldable
  fromZipList = List.toUnfoldable <<< runZipList

collectMaybes :: forall a. Array (Array (Maybe a)) -> Maybe (Array (Array a))
collectMaybes = map sequence >>> sequence

-- this could be generalised to Applicative, but is restricted to Eff for
-- performance reasons.
foreign import eachWithIndex_ :: forall a b e. Array a -> (a -> Number -> Eff e b) -> Eff e Unit

whenJust :: forall a f. (Applicative f) => Maybe a -> (a -> f Unit) -> f Unit
whenJust mx f = maybe (pure unit) f mx

(>>) :: forall a b m. (Monad m) => m a -> m b -> m b
(>>) a b = a >>= (\_ -> b)

fromEither :: forall a b. Either a b -> Maybe b
fromEither (Left _) = Nothing
fromEither (Right x) = Just x

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN = go id
  where
  go f n _ | n <= 0 = f
  go f n g = go (f >>> g) (n - 1) g

zipNumbers :: forall a. Array a -> Array (Tuple Number a)
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

portOrDefault :: forall e.
  Int -> Eff (process :: Process.PROCESS | e) Int
portOrDefault default = do
  port <- Process.lookupEnv "PORT"
  return $ fromMaybe default (port >>= Int.fromString)
