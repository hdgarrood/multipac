module NodeWebSocket where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2, Fn5, runFn5)
import Effect
import Node.HTTP as Http

import NodeUrl

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
foreign import mkServer :: forall e. Effect Server

foreign import registerEventHandlerUnsafe :: forall receiver param x y eff.
  Fn5
    receiver
    String
    String
    (param -> Effect x)
    (y -> param)
    (Eff (ws :: WebSocket | eff) Unit)

type RegisterHandler receiver param = forall e a.
  receiver
  -> (param -> Effect a)
  -> Effect Unit

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

foreign import reject :: forall e. Request -> Effect Unit

foreign import accept :: forall e. Request -> Effect Connection

foreign import resourceUrl :: Request -> Url

foreign import sendImpl :: forall e.
  Fn2 Connection String (Effect Unit)

send :: forall e. Connection -> String -> Effect Unit
send conn msg = runFn2 sendImpl conn msg

foreign import mountImpl :: forall e.
  Fn2 Server Http.Server (Effect Unit)

mount :: forall e.
  Server -> Http.Server -> Effect Unit
mount wsServer httpServer = runFn2 mountImpl wsServer httpServer

foreign import close :: forall e. Connection -> Effect Unit
