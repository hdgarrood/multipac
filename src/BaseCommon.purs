module BaseCommon where

import Prelude
import Data.Generic
import Data.Maybe
import Data.Map (Map())
import Data.Argonaut.Core as A
import Data.Argonaut.Encode
import Data.Argonaut.Decode

import GenericMap
import Types

-- message types internal to the BaseClient/BaseServer
data InternalMessage
  = NewPlayer (GenericMap PlayerId String)
  | YourPlayerIdIs PlayerId

derive instance genericInternalMessage :: Generic InternalMessage

instance encodeInternalMessage :: EncodeJson InternalMessage where
  encodeJson = gEncodeJson

instance decodeInternalMessage :: DecodeJson InternalMessage where
  decodeJson = gDecodeJson
