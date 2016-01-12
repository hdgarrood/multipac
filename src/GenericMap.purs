module GenericMap
  ( GenericMap(..)
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

newtype GenericMap k v = GenericMap (Map k v)

runGenericMap :: forall k v. GenericMap k v -> Map k v
runGenericMap (GenericMap m) = m

newtype GenericMapRepr k v = GenericMapRepr (Array (Tuple k v))

runGenericMapRepr :: forall k v. GenericMapRepr k v -> Array (Tuple k v)
runGenericMapRepr (GenericMapRepr r) = r

derive instance genericGenericMapRepr :: (Generic k, Generic v) => Generic (GenericMapRepr k v)

toRepr :: forall k v. (Ord k) => GenericMap k v -> GenericMapRepr k v
toRepr = GenericMapRepr <<< List.toUnfoldable <<< Map.toList <<< runGenericMap

fromRepr :: forall k v. (Ord k) => GenericMapRepr k v -> GenericMap k v
fromRepr = GenericMap <<< Map.fromFoldable <<< runGenericMapRepr

instance genericMap :: (Ord k, Generic k, Generic v) => Generic (GenericMap k v) where
  toSpine = toSpine <<< toRepr
  fromSpine = map fromRepr <<< fromSpine
  toSignature _ = toSignature (Proxy :: Proxy (GenericMapRepr k v))
