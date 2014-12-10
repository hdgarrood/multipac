module Client where

import Debug.Trace (trace)
import Data.Maybe
import qualified Data.Either as E
import Data.JSON (eitherDecode, encode)
import qualified Data.String as S
import qualified Data.Map as M
import Data.DOM.Simple.Events hiding (view)
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Window (globalWindow, location, document)
import Data.DOM.Simple.Element (setInnerHTML, querySelector, setAttribute)
import Data.DOM.Simple.Document ()
import Control.Monad
import Control.Monad.Eff
import Control.Monad.RWS.Class
import Control.Monad.State.Class
import Control.Monad.Eff.Ref (newRef)
import Control.Lens (lens, (.~), (..), (^.))
import Control.Reactive.Timer

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

initialState =
  ClientState
    { playerNames: M.empty
    , gameState: CWaitingForPlayers
                    { prevGame: Nothing
                    , backgroundCleared: false
                    , readyStates: M.empty
                    , cachedHtml: ""
                    }
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

  let socketUrl = "ws://" <> h <> "/"
  startClient initialState callbacks socketUrl name


-- CALLBACKS
mkCallbacks =
  R.setupRendering <#> \ctx ->
    { render: render ctx
    , onMessage: onMessage
    , onKeyDown: onKeyDown
    }

putGameState :: forall e. ClientGameState -> CM e Unit
putGameState gs = modify $ \(ClientState s) ->
                              ClientState $ s { gameState = gs }

render :: forall e. RenderingContext -> CM e Unit
render ctx = do
  state <- get
  pId <- askPlayerId
  case state ^. gameState of
    CInProgress g -> do
      liftEff $ R.render ctx g.game pId g.redrawMap
      putGameState $ CInProgress (g # redrawMap .~ false)

    CWaitingForPlayers sw -> do
      when (not (sw ^. backgroundCleared)) $ do
        liftEff $ R.clearBoth ctx
        putGameState $ CWaitingForPlayers (sw # backgroundCleared .~ true)

      let html = V.waitingMessage sw pId
      when (sw.cachedHtml /= html) $ do
        liftEff $ do
          el <- q "#waiting-message"
          whenJust el $ setInnerHTML html
        putGameState $ CWaitingForPlayers (sw # cachedHtml .~ html)


onKeyDown :: forall e. DOMEvent -> CM e Unit
onKeyDown event = do
  state <- get
  code <- liftEff $ keyCode event

  case state ^. gameState of
    CInProgress _ -> do
      whenJust (directionFromKeyCode code) $ \direction ->
        sendUpdate (SIInProgress direction)

    CWaitingForPlayers sw -> do
      when (code == keyCodeSpace) do
        sendUpdate SIToggleReadyState

type CM e a = ClientM ClientState ServerIncomingMessage e a

matchInProgress :: forall e.
  ServerOutgoingMessage -> ([GameUpdate] -> CM e Unit) -> CM e Unit
matchInProgress = matchMessage asInProgressMessageO

matchWaiting :: forall e.
  ServerOutgoingMessage -> (WaitingUpdate -> CM e Unit) -> CM e Unit
matchWaiting = matchMessage asWaitingMessageO

onMessage msg = do
  state <- get
  case state ^. gameState of
    CInProgress g ->
      matchInProgress msg $ \updates ->
        let game' = applyGameUpdates updates g.game
        in if isEnded game'
            then do
              liftEff $ showWaitingMessageDiv
              putWaitingState $ Just game'
            else do
              putGameState $ CInProgress (g { game = game'
                                            , prevGame = g.game })

    CWaitingForPlayers g -> do
      matchWaiting msg $ \update -> do
        case update of
          GameStarting game -> do
            let gip = { game: game, prevGame: game, redrawMap: true }
            liftEff $ hideWaitingMessageDiv
            putGameState $ CInProgress gip
          NewReadyStates m -> do
            putGameState $ CWaitingForPlayers (g # readyStates .~ m)


putWaitingState prevGame =
  putGameState $
    CWaitingForPlayers
      { prevGame: prevGame
      , backgroundCleared: false
      , readyStates: M.empty
      , cachedHtml: ""
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
