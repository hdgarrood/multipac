module BrowserWebSocket where

import Data.Tuple
import Data.Function
import Data.Maybe
import Control.Monad.Eff

foreign import data WebSocket :: !
foreign import data Socket :: *
foreign import data CloseEvent :: *

-- for now, only UTF8 messages are supported
-- TODO: node buffers
type Message = String

foreign import mkWebSocket
  """
  function mkWebSocket(url) {
    return function() {
      return new WebSocket(url)
    }
  }
  """ :: forall e. String -> Eff (ws :: WebSocket | e) Socket

foreign import onMessageImpl
  """
  function onMessageImpl(socket, callback) {
    return function() {
      socket.onmessage = function(msg) {
        callback(msg.data)()
      }
    }
  }
  """ :: forall e a.
  Fn2
    Socket
    (Message -> Eff (ws :: WebSocket | e) a)
    (Eff (ws :: WebSocket | e) Unit)

onMessage :: forall e a.
  Socket
  -> (Message -> Eff (ws :: WebSocket | e) a)
  -> Eff (ws :: WebSocket | e) Unit
onMessage socket callback =
  runFn2 onMessageImpl socket callback


foreign import onErrorImpl
  """
  function onErrorImpl(socket, callback) {
    return function() {
      socket.onerror = callback
    }
  }
  """ :: forall e a.
  Fn2
    Socket
    (Eff (ws :: WebSocket | e) a)
    (Eff (ws :: WebSocket | e) Unit)

onError :: forall e a.
  Socket
  -> (Eff (ws :: WebSocket | e) a)
  -> (Eff (ws :: WebSocket | e) Unit)
onError socket callback =
  runFn2 onErrorImpl socket callback

foreign import onCloseImpl
  """
  function onCloseImpl(socket, callback) {
    return function() {
      socket.onclose = callback
    }
  }
  """ :: forall e a.
  Fn2
    Socket
    (Eff (ws :: WebSocket | e) a)
    (Eff (ws :: WebSocket | e) Unit)

onClose :: forall e a.
  Socket
  -> (Eff (ws :: WebSocket | e) a)
  -> (Eff (ws :: WebSocket | e) Unit)
onClose socket callback =
  runFn2 onCloseImpl socket callback

foreign import sendImpl
  """
  function sendImpl(socket, message) {
    return function() {
      socket.send(message)
    }
  } """ :: forall e.
  Fn2 Socket Message (Eff (ws :: WebSocket | e) Unit)

send :: forall e.
  Socket -> Message -> Eff (ws :: WebSocket | e) Unit
send sock msg = runFn2 sendImpl sock msg
