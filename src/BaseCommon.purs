module BaseCommon where

import Prelude
import Control.Monad
import Data.Maybe
import Data.Map (Map())
import Data.Argonaut.Core as A
import Data.Argonaut.Encode
import Data.Argonaut.Decode

import Types

-- message types internal to the BaseClient/BaseServer
data InternalMessage
  = NewPlayer (Map PlayerId String)
  | YourPlayerIdIs PlayerId

instance encodeInternalMessage :: EncodeJson InternalMessage where
  encodeJson (NewPlayer m) =
    A.fromArray [A.fromString "__internal", A.fromString "NewPlayer", encodeJson m]
  toJSON (YourPlayerIdIs pId) =
    A.fromArray [A.fromString "__internal", A.fromString "YourPlayerIdIs", encodeJson pId]

instance decodeInternalMessage :: DecodeJson InternalMessage where
  decodeJson json =
    case maybe (Left "Wrong type: expected array") A.toArray json of
      [internal, type_, x] -> do
        unless (A.fromString internal == "__internal")
          (Left "Not internal")
        case A.fromString type_ of
          Just "NewPlayer" -> NewPlayer <$> decodeJson x
          Just "YourPlayerIdIs" -> YourPlayerIdIs <$> decodeJson x
          Nothing -> Left "Wrong type: expected String"
