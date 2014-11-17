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
  function mkWebSocket(path) {
    return function() {
      return new WebSocket(path)
    }
  }
  """ :: forall e. String -> Eff (ws :: WebSocket | e) Socket

foreign import onMessageImpl
  """
  function onMessageImpl(socket, callback) {
    return function() {
      socket.onmessage = function(msg) {
        callback(msg)()
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
