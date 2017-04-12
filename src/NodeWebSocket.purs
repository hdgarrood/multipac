module NodeWebSocket where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2, Fn5, runFn5)
import Control.Monad.Eff
import Node.HTTP as Http

import NodeUrl

foreign import data WebSocket :: Effect
foreign import data Server :: Type
foreign import data Connection :: Type
foreign import data Request :: Type
foreign import data Close :: Type

-- for now, only UTF8 messages are supported
-- TODO: node buffers
type Message = String

type CloseReasonCode = Number
type CloseReasonDescription = String

-- TODO: options
foreign import mkServer :: forall e. Eff (ws :: WebSocket | e) Server

foreign import registerEventHandlerUnsafe :: forall receiver param x y eff.
  Fn5
    receiver
    String
    String
    (param -> Eff (ws :: WebSocket | eff) x)
    (y -> param)
    (Eff (ws :: WebSocket | eff) Unit)

type RegisterHandler receiver param = forall e a.
  receiver
  -> (param -> Eff (ws :: WebSocket | e) a)
  -> Eff (ws :: WebSocket | e) Unit

onRequest :: RegisterHandler Server Request
onRequest server callback =
  runFn5 registerEventHandlerUnsafe
    server "on" "request" callback id

onMessage :: RegisterHandler Connection String
onMessage conn callback =
  runFn5 registerEventHandlerUnsafe
    conn "on" "message" callback getMessageData

onceOnMessage :: RegisterHandler Connection String
onceOnMessage conn callback =
  runFn5 registerEventHandlerUnsafe
    conn "once" "message" callback getMessageData

foreign import getMessageData :: Message -> String

onClose :: RegisterHandler Connection Close
onClose conn callback =
  runFn5 registerEventHandlerUnsafe
    conn "on" "close" callback id

foreign import reject :: forall e. Request -> Eff (ws :: WebSocket | e) Unit

foreign import accept :: forall e. Request -> Eff (ws :: WebSocket | e) Connection

foreign import resourceUrl :: Request -> Url

foreign import sendImpl :: forall e.
  Fn2 Connection String (Eff (ws :: WebSocket | e) Unit)

send :: forall e. Connection -> String -> Eff (ws :: WebSocket | e) Unit
send conn msg = runFn2 sendImpl conn msg

foreign import mountImpl :: forall e.
  Fn2 Server Http.Server (Eff (ws :: WebSocket, http :: Http.HTTP | e) Unit)

mount :: forall e.
  Server -> Http.Server -> Eff (ws :: WebSocket, http :: Http.HTTP | e) Unit
mount wsServer httpServer = runFn2 mountImpl wsServer httpServer

foreign import close :: forall e. Connection -> Eff (ws :: WebSocket | e) Unit
