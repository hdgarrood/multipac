module BrowserWebSocket where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import EffObject (EffObject, ReadWrite, ReadOnly)

foreign import data WEBSOCKET :: Effect

newtype WebSocket e =
  WebSocket
    (EffObject
      (websocket :: WEBSOCKET | e)
      ( binaryType :: ReadWrite String
      , bufferedAmount :: ReadOnly Int
      , extensions :: ReadWrite String
      , onclose :: ReadWrite Unit
      , onerror :: ReadWrite Unit
      , onmessage :: ReadWrite Unit
      , onopen :: ReadWrite Unit
      , protocol :: ReadWrite String
      , readyState :: ReadOnly Int
      , url :: ReadOnly String
      )
    )
