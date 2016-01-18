module BaseCommon where

import Prelude
import Data.Generic
import Data.Maybe
import Data.Map (Map())

import GenericMap
import Types

-- message types internal to the BaseClient/BaseServer
data InternalMessage
  = NewPlayer (GenericMap PlayerId String)
  | YourPlayerIdIs PlayerId

derive instance genericInternalMessage :: Generic InternalMessage
