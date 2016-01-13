module NodeUrl where

import Prelude

type Url =
  { href :: String
  , search :: String
  , query :: String
  , pathname :: String
  }

foreign import parseUrl :: String -> Url
