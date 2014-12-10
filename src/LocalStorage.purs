module LocalStorage
  ( Storage()
  , getStorage
  , setStorage
  ) where

import Data.Function
import Data.Maybe
import Control.Monad.Eff

foreign import data Storage :: !

foreign import storageGetImpl """
  function storageGetImpl(key) {
    return function() {
      return localStorage[key]
    }
  }
""" :: forall e. String -> Eff (storage :: Storage | e) String

foreign import setStorage """
  function setStorage(key) {
    return function(value) {
      return function() {
        localStorage[key] = value
      }
    }
  }
""" :: forall e. String -> String -> Eff (storage :: Storage | e) Unit

foreign import ensureImpl """
  function ensureImpl(just, nothing, value) {
    return value ? just(value) : nothing
  }
""" :: forall a. Fn3 (a -> Maybe a) (Maybe a) a (Maybe a)

ensure :: forall a. a -> Maybe a
ensure value = runFn3 ensureImpl Just Nothing value

getStorage :: forall e. String -> Eff (storage :: Storage | e) (Maybe String)
getStorage key = ensure <$> storageGetImpl key
