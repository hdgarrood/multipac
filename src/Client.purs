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

data Owner = RenderLoop | WebSocketLoop

instance eqOwner :: Eq Owner where
  (==) RenderLoop RenderLoop = true
  (==) WebSocketLoop WebSocketLoop = true
  (==) _ _ = false
  (/=) x y = not (x == y)

instance showOwner :: Show Owner where
  show RenderLoop = "RenderLoop"
  show WebSocketLoop = "WebSocketLoop"


waitForOwnership :: forall e.
  Owner
  -> RefVal (Maybe Owner)
  -> Eff (ref :: Ref, trace :: Trace, timer :: Timer | e) Unit
  -> Eff (ref :: Ref, trace :: Trace, timer :: Timer | e) Unit
waitForOwnership owner ownerRef action = go
  where
  go = do
    currentOwner <- readRef ownerRef
    case currentOwner of
      Just o ->
        if o == owner
           then trace $ "wtf: ownerRef is already set to " <> show owner
           else do
             trace $ "waiting for " <> show o <> " to relinquish ownership"
             void $ timeout 1 go
      Nothing -> do
        writeRef ownerRef (Just owner)
        action
        writeRef ownerRef (Nothing)


main = do
  ctx <- setupRendering
  state <- newRef initialState

  -- who currently 'owns' the state ref.
  owner <- newRef (Nothing :: Maybe Owner)

  h <- host <$> location globalWindow
  socket <- WS.mkWebSocket $ "ws://" <> h <> "/"
  WS.onMessage socket $ \msg -> do
    case eitherDecode msg of
      E.Left err ->
        trace $ "failed to parse message from server: " <> err
      E.Right update ->
        waitForOwnership WebSocketLoop owner $
          modifyRef state $ \s -> s { game = applyGameUpdate update s.game
                                    , prevGame = s.game
                                    }

  addKeyboardEventListener
    KeydownEvent
    (handleKeydown socket)
    globalWindow

  startAnimationLoop $
    waitForOwnership RenderLoop owner $ do
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
