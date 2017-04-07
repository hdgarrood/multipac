module BrowserWebSocket where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data WEBSOCKET :: Effect

newtype WebSocket e =
  WebSocket
    (EffObject
      (websocket :: WEBSOCKET | e)
      -- Readable properties
      ( binaryType :: String
      , bufferedAmount :: Int
      , extensions :: String
      , onclose :: Unit
      , onerror :: Unit
      , onmessage :: Unit
      , onopen :: Unit
      , protocol :: String
      , readyState :: Int
      , url :: String
      )
      -- Writable properties
      ( binaryType :: String
      , extensions :: String
      , onclose :: Unit
      , onerror :: Unit
      , onmessage :: Unit
      , onopen :: Unit
      , protocol :: String
      )
    )
