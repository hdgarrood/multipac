module Client where

import Debug.Trace
import Data.Maybe
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Window (globalWindow, location)
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer

import qualified BrowserWebSocket as WS
import Rendering
import Game
import Types
import Utils

foreign import host
  """
  function host(location) {
    return location.host;
  }
  """ :: DOMLocation -> String

initialState :: ClientState
initialState =
  { game: initialGame
  , prevGame: initialGame
  , redrawMap: true
  }

main = do
  ctx <- setupRendering
  state <- newRef initialState

  h <- host <$> location globalWindow
  socket <- WS.mkWebSocket $ "ws://" <> h <> "/"
  WS.onMessage socket $ \msg -> do
    case eitherDecode msg of
      E.Left err ->
        trace $ "failed to parse message from server: " <> err
      E.Right update ->
        modifyRef state $ \s -> s { game = applyGameUpdate update s.game
                                  , prevGame = s.game
                                  }

  addKeyboardEventListener
    KeydownEvent
    (handleKeydown socket)
    globalWindow

  startAnimationLoop $ do
    s <- readRef state
    render ctx s
    when (s.redrawMap) $
      writeRef state (s { redrawMap = false })

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
