module NodeWebSocket where

import Data.Tuple
import Data.Function
import Data.Maybe
import Control.Monad.Eff

foreign import data WebSocket :: !
foreign import data Server :: *
foreign import data Connection :: *
foreign import data Request :: *
foreign import data Close :: *

import qualified NodeHttp as Http

-- for now, only UTF8 messages are supported
-- TODO: node buffers
type Message = String

type CloseReasonCode = Number
type CloseReasonDescription = String

foreign import ws "var ws = require('websocket')" :: Unit

-- TODO: options
foreign import mkServer
  """
  function mkServer() {
    return new ws.server()
  }
  """ :: forall e. Eff (ws :: WebSocket | e) Server

foreign import registerEventHandlerUnsafe
  """
  function registerEventHandlerUnsafe(receiver, msgType, callback, transform) {
    return function() {
      receiver.on(msgType, function(param) {
        var param2 = transform ? transform(param) : param
        callback(param2)()
      })
    }
  }
  """ :: forall e a b c d.
    Fn4
      a
      String
      (b -> Eff (ws :: WebSocket | e) c)
      (d -> b)
      (Eff (ws :: WebSocket | e) Unit)

type RegisterHandler receiver param = forall e a.
  receiver
  -> (param -> Eff (ws :: WebSocket | e) a)
  -> Eff (ws :: WebSocket | e) Unit

onRequest :: RegisterHandler Server Request
onRequest server callback =
  runFn4 registerEventHandlerUnsafe server "request" callback id

onMessage :: RegisterHandler Connection String
onMessage conn callback =
  runFn4 registerEventHandlerUnsafe conn "message" callback getMessageData

foreign import getMessageData
  """
  function getMessageData(msg) {
    if (msg.type == 'utf8') {
      return msg.utf8Data
    } else if (msg.type == 'binary') {
      throw new Error('unhandled websocket message type: binary')
    }
  }
  """ :: Message -> String

onClose :: RegisterHandler Connection Close
onClose conn callback =
  runFn4 registerEventHandlerUnsafe conn "close" callback id

foreign import reject
  """
  function reject(request) {
    return function() {
      request.reject()
    }
  }
  """ :: forall e. Request -> Eff (ws :: WebSocket | e) Unit

foreign import accept
  """
  function accept(request) {
    return function() {
      return request.accept(null, request.origin)
    }
  }
  """ :: forall e. Request -> Eff (ws :: WebSocket | e) Connection

foreign import sendImpl
  """
  function sendImpl(conn, msg) {
    return function() {
      conn.sendUTF(msg)
    }
  }
  """ :: forall e.
  Fn2 Connection String (Eff (ws :: WebSocket | e) Unit)

send :: forall e. Connection -> String -> Eff (ws :: WebSocket | e) Unit
send conn msg = runFn2 sendImpl conn msg

foreign import mountImpl
  """
  function mountImpl(wsServer, httpServer) {
    return function() {
      wsServer.mount({httpServer: httpServer})
    }
  }
  """ :: forall e.
    Fn2 Server Http.Server (Eff (ws :: WebSocket, http :: Http.Http | e) Unit)

mount :: forall e.
  Server -> Http.Server -> Eff (ws :: WebSocket, http :: Http.Http | e) Unit
mount wsServer httpServer = runFn2 mountImpl wsServer httpServer
