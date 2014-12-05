module Client where

import Debug.Trace
import Data.Maybe
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Data.Foldable (for_, foldr)
import Data.Array (length)
import qualified Data.Map as M
import qualified Data.String as S
import Data.DOM.Simple.Events hiding (view)
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Window (globalWindow, location)
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer
import Control.Lens hiding ((.=))

import qualified BrowserWebSocket as WS
import qualified Rendering as R
import Game
import Types
import Utils

foreign import host
  """
  function host(location) {
    return location.host;
  }
  """ :: DOMLocation -> String

mkInitialState :: WS.Socket -> PlayerId -> ClientState
mkInitialState socket pId =
  { socket: socket
  , gameState: mkWaitingState Nothing
  , playerId: pId
  }

runCallback refState callback args = do
  state <- readRef refState
  state' <- callback args state
  writeRef refState state'


main = do
  -- TODO: nicer ui
  name <- prompt "Enter a screen name:"
  if S.null name
     then main
     else start name


start name = do
  ctx <- R.setupRendering
  h <- host <$> location globalWindow
  socket <- WS.mkWebSocket $ "ws://" <> h <> "/?" <> name

  getPlayerId socket $ \pId -> do
    refState <- newRef (mkInitialState socket pId)

    WS.onMessage socket $ \msg -> do
      runCallback refState onMessage {msg:msg}

    addKeyboardEventListener
      KeydownEvent
      (\event -> runCallback refState onKeyDown {event:event})
      globalWindow

    void $ startAnimationLoop $ do
      runCallback refState render {ctx:ctx}


getPlayerId socket cont =
  WS.onMessage socket callback
  where
  callback msg =
    case eitherDecode msg of
      E.Right (YourPlayerIdIs pId) -> trace $ show pId >> cont pId
      E.Left err -> trace err


-- callbacks
render args state = do
  case state ^. gameState of
    CInProgress g -> do
      R.render args.ctx g.game state.playerId g.redrawMap
      return $ state { gameState = CInProgress (g { redrawMap = false }) }

    CWaitingForPlayers g -> do
      R.renderWaiting args.ctx g.ready state.playerId
      return state

onKeyDown args state = do
  case state ^. gameState of
    CInProgress _ -> do
      code <- keyCode (args.event::DOMEvent)
      whenJust (directionFromKeyCode code) $ \direction ->
        WS.send state.socket (encode direction)
      return state

    CWaitingForPlayers g -> do
      code <- keyCode args.event
      if code == keyCodeSpace
        then do
          let ready' = not g.ready
          WS.send state.socket (encode ready')
          return $ state { gameState = CWaitingForPlayers $ g { ready = ready' } }
        else
          return state

onMessage args state = do
  case state ^. gameState of
    CInProgress g -> 
      case eitherDecode args.msg of
        E.Left err -> do
          trace $ "failed to parse message from server: " <> err
          return state
        E.Right updates -> do
          let game' = foldr applyGameUpdate g.game (updates :: [GameUpdate])
          if isEnded game'
            then do
              let newGameState = mkWaitingState (Just game')
              return $ state { gameState = newGameState }
            else do
              let newGameState = CInProgress (g { game = game'
                                                , prevGame = g.game })
              return $ state { gameState = newGameState }

    CWaitingForPlayers g -> do
      case eitherDecode args.msg of
        E.Left err -> do
          trace $ "failed to parse message from server: " <> err
          return state
        E.Right update ->
          case update of
            GameStarting game -> do
              let gip = { game: game, prevGame: game, redrawMap: true }
              return $ state { gameState = CInProgress gip }


mkWaitingState prevGame =
  CWaitingForPlayers
    { prevGame: prevGame
    , backgroundCleared: false
    , ready: false
    }

directionFromKeyCode :: Number -> Maybe Direction
directionFromKeyCode code =
  case code of
    38 -> Just Up
    40 -> Just Down
    37 -> Just Left
    39 -> Just Right
    _  -> Nothing

keyCodeSpace = 32

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

foreign import prompt
  """
  function prompt(msg) {
    return function() {
      return window.prompt(msg);
    }
  }""" :: forall e. String -> Eff e String
