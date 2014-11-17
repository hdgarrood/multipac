module Client where

import Debug.Trace
import Data.Maybe
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types (DOM(), DOMEvent())
import Data.DOM.Simple.Window (globalWindow)
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer

import qualified BrowserWebSocket as WS
import Rendering
import Game
import Types
import Utils

main = do
  ctx <- setupRendering
  game <- newRef initialGame

  socket <- WS.mkWebSocket "ws://localhost:8080/"
  WS.onMessage socket $ \msg -> do
    case eitherDecode msg of
      E.Left err -> trace $ "failed to parse message from server: " <> err
      E.Right update -> modifyRef game (applyGameUpdate update)

  addKeyboardEventListener
    KeydownEvent
    (handleKeydown socket)
    globalWindow

  startAnimationLoop (readRef game >>= renderGame ctx)

handleKeydown :: forall e.
  WS.Socket -> DOMEvent -> Eff (ws :: WS.WebSocket, dom :: DOM | e) Unit
handleKeydown socket event = do
  code <- keyCode event
  whenJust (directionFromKeyCode code) $ \direction ->
    WS.send socket (encode direction)

directionFromKeyCode :: Number -> Maybe Direction
directionFromKeyCode code =
  case code of
    38 -> Just Up
    40 -> Just Down
    37 -> Just Left
    39 -> Just Right
    _  -> Nothing

foreign import startAnimationLoop
  """
  function startAnimationLoop(action) {
    return function go() {
      window.requestAnimationFrame(go);
      action();
    }
  }
  """ :: forall a e. Eff e a -> Eff e Unit
