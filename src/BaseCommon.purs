module BaseCommon where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe
import Data.Map (Map)

import Types (PlayerId)

-- message types internal to the BaseClient/BaseServer
data InternalMessage
  = NewPlayer (Map PlayerId String)
  | YourPlayerIdIs PlayerId

derive instance genericInternalMessage :: Generic InternalMessage _
