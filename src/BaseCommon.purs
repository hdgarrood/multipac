module BaseCommon where

import Data.JSON
import Types

-- message types internal to the BaseClient/BaseServer
data InternalMessage =
  NewPlayer PlayerId String

instance toJSONInternalMessage :: ToJSON InternalMessage where
  toJSON (NewPlayer pId name) =
    JArray [JString "__internal", toJSON pId, toJSON name]

instance fromJSONInternalMessage :: FromJSON InternalMessage where
  parseJSON (JArray [JString "__internal", pId, name]) =
    NewPlayer <$> parseJSON pId <*> parseJSON name

