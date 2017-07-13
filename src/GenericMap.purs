module GenericMap
  ( GenericMap(..)
  , mkGenericMap
  , runGenericMap
  ) where

import Prelude
import Data.Generic
import Data.Tuple
import Data.Map (Map())
import Data.Map as Map
import Data.List (List())
import Data.List as List
import Type.Proxy (Proxy(..))

newtype GenericMap k v = GenericMap (Array (Tuple k v))

mkGenericMap :: forall k v. (Ord k) => Map k v -> GenericMap k v
mkGenericMap = GenericMap <<< Map.toUnfoldable

runGenericMap :: forall k v. (Ord k) => GenericMap k v -> Map k v
runGenericMap (GenericMap m) = Map.fromFoldable m

derive instance genericGenericMap :: (Generic k, Generic v) => Generic (GenericMap k v)
