module BaseCommon where

import Data.JSON
import qualified Data.Map as M
import Types

-- message types internal to the BaseClient/BaseServer
data InternalMessage
  = NewPlayer (M.Map PlayerId String)
  | YourPlayerIdIs PlayerId

instance toJSONInternalMessage :: ToJSON InternalMessage where
  toJSON (NewPlayer m) =
    JArray [JString "__internal", JString "NewPlayer", toJSON m]
  toJSON (YourPlayerIdIs pId) =
    JArray [JString "__internal", JString "YourPlayerIdIs", toJSON pId]

instance fromJSONInternalMessage :: FromJSON InternalMessage where
  parseJSON (JArray [JString "__internal", JString type_, x]) =
    case type_ of
      "NewPlayer" -> NewPlayer <$> parseJSON x
      "YourPlayerIdIs" -> YourPlayerIdIs <$> parseJSON x
