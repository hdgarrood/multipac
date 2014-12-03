module Client where

import Debug.Trace
import Data.Maybe
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Data.Array (length)
import qualified Data.Map as M
import Data.DOM.Simple.Events hiding (view)
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Window (globalWindow, location)
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer
import Control.Lens hiding ((.=))

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

mkInitialState :: WS.Socket -> ClientState
mkInitialState socket =
  { socket: socket
  , gameState: CWaitingForPlayers false
  , callbacks: waitingCallbacks
  }

runCallback refState callback args = do
  state <- readRef refState
  let cc = unwrapClientCallbacks state.callbacks
  state' <- callback cc args state
  writeRef refState state'


gameInProgress :: LensP ClientState ClientStateInProgress
gameInProgress = lens
  (\s -> case s.gameState of
    CInProgress x -> x
    _ -> error "gameInProgress: expected game to be in progress")
  (\s x -> s { gameState = CInProgress x })

gameWaiting :: LensP ClientState Boolean
gameWaiting = lens
  (\s -> case s.gameState of
    CWaitingForPlayers x -> x
    _ -> error "gameInProgress: expected game to be in progress")
  (\s x -> s { gameState = CWaitingForPlayers x })



main = do
  ctx <- setupRendering
  h <- host <$> location globalWindow
  socket <- WS.mkWebSocket $ "ws://" <> h <> "/"

  refState <- newRef (mkInitialState socket)

  WS.onMessage socket $ \msg -> do
    runCallback refState (\c -> c.onMessage) {msg:msg}

  addKeyboardEventListener
    KeydownEvent
    (\event -> runCallback refState (\c -> c.onKeyDown) {event:event})
    globalWindow

  startAnimationLoop $ do
    runCallback refState (\c -> c.render) {ctx:ctx}


inProgressCallbacks = ClientCallbacks
  { render: \args state -> do
      let g = state ^. gameInProgress
      render args.ctx g.game g.redrawMap
      return $ state { gameState = CInProgress (g { redrawMap = false }) }

  , onKeyDown: \args state -> do
      code <- keyCode args.event
      whenJust (directionFromKeyCode code) $ \direction ->
        WS.send state.socket (encode direction)
      return state

  , onMessage: \args state -> do
      case eitherDecode args.msg of
        E.Left err -> do
          trace $ "failed to parse message from server: " <> err
          return state
        E.Right update -> do
          let g = state ^. gameInProgress
          let game' = applyGameUpdate update g.game
          let newGameState = CInProgress (g { game = game'
                                            , prevGame = g.game })
          return $ state { gameState = newGameState }
  }


directionFromKeyCode :: Number -> Maybe Direction
directionFromKeyCode code =
  case code of
    38 -> Just Up
    40 -> Just Down
    37 -> Just Left
    39 -> Just Right
    _  -> Nothing


waitingCallbacks = ClientCallbacks
  { render: \args state -> do
      let g = state ^. gameWaiting
      renderWaiting args.ctx g
      return state

  , onMessage: \args state -> do
      case eitherDecode args.msg of
        E.Left err -> do
          trace $ "failed to parse message from server: " <> err
          return state
        E.Right update ->
          case update of
            (GameStarting game) -> do
              let cip = { game: game, prevGame: game, redrawMap: true }
              return $ state
                { gameState = CInProgress cip
                , callbacks = inProgressCallbacks
                }

  , onKeyDown: \args state -> do
      code <- keyCode args.event
      let ready = state ^. gameWaiting
      case code of
        32 -> do
          let ready' = not ready
          WS.send state.socket (encode ready')
          return $ state { gameState = CWaitingForPlayers ready' }
        _ -> return state
  }


foreign import data AnimationLoop :: *

foreign import startAnimationLoop
  """
  function startAnimationLoop(action) {
    var loop = {}

    var go = (function() {
      window.requestAnimationFrame(this.go);
      action();
    }).bind(loop)
    loop.go = go

    var stop = (function() {
      this.go = function () { }
    }).bind(loop)
    loop.stop = stop

    return function() {
      loop.go()
      return loop
    }
  }
  """ :: forall a e. Eff e a -> Eff e AnimationLoop

foreign import stopAnimationLoop
  """
  function stopAnimationLoop(loop) {
    loop.stop()
  }
  """ :: forall e. AnimationLoop -> Eff e Unit
