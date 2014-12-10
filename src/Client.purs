module Client where

import Debug.Trace (trace)
import Data.Maybe
import qualified Data.Either as E
import Data.JSON (eitherDecode)
import qualified Data.String as S
import Data.DOM.Simple.Events hiding (view)
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Window (globalWindow, location, document)
import Data.DOM.Simple.Element (setInnerHTML, querySelector, setAttribute)
import Data.DOM.Simple.Document ()
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Eff
import Control.Monad.State.Class
import Control.Monad.Eff.Ref (newRef)
import Control.Lens (lens, (.~), (..), (^.))

import qualified BrowserWebSocket as WS
import qualified Rendering as R
import qualified HtmlViews as V
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
    let initialState = mkInitialState socket pId

    -- perform the initial render
    case initialState.state of
      CWaitingForPlayers sw -> renderWaiting sw pId
      _ -> return unit

    refCln <- newRef initialState
    startClient callbacks refCln

getPlayerId socket cont =
  WS.onMessage socket callback
  where
  callback msg =
    case eitherDecode msg of
      E.Right (SOConnecting (YourPlayerIdIs pId)) -> cont pId
      E.Left err -> trace err

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
      when (not (sw ^. backgroundCleared)) $ do
        lift..lift $ R.clearBoth ctx
        put $ CWaitingForPlayers (sw # backgroundCleared .~ true)


onKeyDown :: forall e. PlayerId -> DOMEvent -> CM e Unit
onKeyDown pId event = do
  state <- get
  code <- lift .. lift $ keyCode event

  case state of
    CInProgress _ -> do
      whenJust (directionFromKeyCode code) $ \direction ->
        sendUpdate (SIInProgress direction)

    CWaitingForPlayers sw -> do
      when (code == keyCodeSpace) do
        let ready' = not sw.ready
        sendUpdate (SIWaiting ready')
        let sw' = sw # ready .~ ready'
        put $ CWaitingForPlayers sw'
        lift..lift $ renderWaiting sw' pId

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
        in if isEnded game'
            then do
              lift..lift $ showWaitingMessageDiv
              put $ mkWaitingState $ Just game'
            else do
              put $ CInProgress (g { game = game'
                                   , prevGame = g.game })

    CWaitingForPlayers g -> do
      matchWaiting msg $ \update -> do
        case update of
          GameStarting game -> do
            let gip = { game: game, prevGame: game, redrawMap: true }
            lift..lift $ hideWaitingMessageDiv
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

setWaitingDivStyle s = do
  el <- q "#waiting-message"
  whenJust el $ setAttribute "style" s

showWaitingMessageDiv = setWaitingDivStyle "display: block;"
hideWaitingMessageDiv = setWaitingDivStyle "display: none;"

q sel = document globalWindow >>= querySelector sel

renderWaiting :: forall e.
  ClientStateWaiting -> PlayerId -> Eff (dom :: DOM | e) Unit
renderWaiting sw pId = do
  let html = V.waitingMessage sw pId
  el <- q "#waiting-message"
  whenJust el $ setInnerHTML html
