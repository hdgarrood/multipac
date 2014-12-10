module Client where

import Debug.Trace (trace)
import Data.Maybe
import qualified Data.Either as E
import Data.JSON (eitherDecode)
import qualified Data.String as S
import Data.DOM.Simple.Events hiding (view)
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Window (globalWindow, location)
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Eff
import Control.Monad.State.Class
import Control.Monad.Eff.Ref (newRef)
import Control.Lens (lens, (.~), (..), (^.))

import qualified BrowserWebSocket as WS
import qualified Rendering as R
import BaseClient
import Game
import Types
import Utils

foreign import host
  """
  function host(location) {
    return location.host;
  }
  """ :: DOMLocation -> String

foreign import prompt
  """
  function prompt(msg) {
    return function() {
      return window.prompt(msg);
    }
  }""" :: forall e. String -> Eff e String


mkInitialState :: WS.Socket -> PlayerId -> Client ClientGameState
mkInitialState socket pId =
  { socket: socket
  , state: mkWaitingState Nothing
  , playerId: pId
  }

main = do
  -- TODO: nicer ui
  name <- prompt "Enter a screen name:"
  if S.null name
     then main
     else start name


start name = do
  callbacks <- mkCallbacks
  h <- host <$> location globalWindow
  socket <- WS.mkWebSocket $ "ws://" <> h <> "/?" <> name

  getPlayerId socket $ \pId -> do
    refCln <- newRef $ mkInitialState socket pId
    startClient callbacks refCln

getPlayerId socket cont =
  WS.onMessage socket callback
  where
  callback msg =
    case eitherDecode msg of
      E.Right (SOConnecting (YourPlayerIdIs pId)) ->
        trace $ show pId >> cont pId
      E.Left err -> trace err

-- LENSES

redrawMap = lens
  (\s -> s.redrawMap)
  (\s x -> s { redrawMap = x })

backgroundCleared = lens
  (\s -> s.backgroundCleared)
  (\s x -> s { backgroundCleared = x })

ready = lens
  (\s -> s.ready)
  (\s x -> s { ready = x })

-- CALLBACKS
mkCallbacks =
  R.setupRendering <#> \ctx ->
    { render: render ctx
    , onMessage: onMessage
    , onKeyDown: onKeyDown
    }

render :: forall e. RenderingContext -> PlayerId -> CM e Unit
render ctx pId = do
  state <- get
  case state of
    CInProgress g -> do
      lift..lift $ R.render ctx g.game pId g.redrawMap
      put $ CInProgress (g # redrawMap .~ false)

    CWaitingForPlayers sw -> do
      lift..lift $ renderWaiting sw pId
      when (not (sw ^. backgroundCleared)) $ do
        lift..lift $ R.clearBackgroundWaiting ctx
        put $ CWaitingForPlayers (sw # backgroundCleared .~ true)


onKeyDown :: forall e. DOMEvent -> CM e Unit
onKeyDown event = do
  state <- get
  code <- lift .. lift $ keyCode event

  case state of
    CInProgress _ -> do
      whenJust (directionFromKeyCode code) $ \direction ->
        sendUpdate (SIInProgress direction)

    CWaitingForPlayers g -> do
      when (code == keyCodeSpace) do
        let ready' = not g.ready
        sendUpdate (SIWaiting ready')
        let g' = g # ready .~ ready'
        put $ CWaitingForPlayers g'


type CM e a = ClientM ClientGameState ServerIncomingMessage e a

matchInProgress :: forall e.
  ServerOutgoingMessage -> ([GameUpdate] -> CM e Unit) -> CM e Unit
matchInProgress = matchMessage asInProgressMessageO

matchWaiting :: forall e.
  ServerOutgoingMessage -> (WaitingUpdate -> CM e Unit) -> CM e Unit
matchWaiting = matchMessage asWaitingMessageO

onMessage msg = do
  state <- get
  case state of
    CInProgress g ->
      matchInProgress msg $ \updates ->
        let game' = applyGameUpdates updates g.game
        in put $ if isEnded game'
                  then mkWaitingState $ Just game'
                  else CInProgress (g { game = game'
                                      , prevGame = g.game })

    CWaitingForPlayers g -> do
      matchWaiting msg $ \update -> do
        case update of
          GameStarting game -> do
            let gip = { game: game, prevGame: game, redrawMap: true }
            put $ CInProgress gip


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

renderWaiting :: forall e. ClientStateWaiting -> PlayerId -> Eff e Unit
renderWaiting sw pId = do
  return unit
