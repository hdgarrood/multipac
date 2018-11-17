module BaseCommon where

import Prelude
import Data.Maybe
import Data.Map (Map)

import Types

-- message types internal to the BaseClient/BaseServer
data InternalMessage
  = NewPlayer (Map PlayerId String)
  | YourPlayerIdIs PlayerId

derive instance genericInternalMessage :: Generic InternalMessage
