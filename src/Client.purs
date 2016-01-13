module Client where

import Debug.Trace (trace)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Either as E
import Data.Tuple
import Data.String as S
import Data.Map as M
import Data.DOM.Simple.Events hiding (view)
import Data.DOM.Simple.Types (DOM(), DOMEvent(), DOMLocation(), HTMLElement())
import Data.DOM.Simple.Window (globalWindow, location, document)
import Data.DOM.Simple.Element
  (setInnerHTML, querySelector, setAttribute, value, setValue, focus)
import Data.DOM.Simple.Events
  (keyCode, addKeyboardEventListener, KeyboardEventType(..))
import Data.DOM.Simple.Document ()
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.RWS.Class
import Control.Monad.State.Class
import Data.Lens (lens, LensP())
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~))
import Data.Lens.At (at)
import Control.Timer

import BrowserWebSocket as WS
import Rendering as R
import HtmlViews as V
import BaseClient
import Game
import Types
import Utils
import LocalStorage

initialState =
  CWaitingForPlayers
    { prevGame: Nothing
    , backgroundCleared: false
    , readyStates: M.empty
    , cachedHtml: ""
    }

main = do
  hideSimpleScoresDiv
  startRef <- newRef false
  loop startRef
  where
  loop startRef =
    promptScreenName "Enter a screen name:" $ \name ->
      if S.null (S.trim name)
         then loop startRef
         else start startRef name

promptScreenName :: forall e.
  String -> (String -> Eff (storage :: Storage, dom :: DOM | e) Unit)
  -> Eff (storage :: Storage, dom :: DOM | e) Unit
promptScreenName msg cont = do
  let key = "screenName"
  mVal <- getStorage key
  let val = fromMaybe "" mVal
  popupPromptInput msg val $ \name -> do
    setStorage "screenName" name
    cont name

popupPromptInput msg val cont = do
  containerEl <- q' "#prompt"
  showElement (containerEl :: HTMLElement)

  messageEl <- q' "#prompt-message"
  setInnerHTML msg (messageEl :: HTMLElement)

  inputEl <- q' "#prompt-input"
  setValue val (inputEl :: HTMLElement)

  focus inputEl
  selectElement inputEl

  addKeyboardEventListener
    KeydownEvent
    (handleKeydown containerEl inputEl)
    inputEl
  where
  handleKeydown cEl iEl event = do
    code <- keyCode (event :: DOMEvent)
    when (code == keyCodeEnter) $ do
      hideElement cEl
      value iEl >>= cont

start startedRef name = do
  started <- readRef startedRef
  when (not started) $ do
    writeRef startedRef true

    callbacks <- mkCallbacks
    h <- host <$> location globalWindow
    p <- protocol <$> location globalWindow

    let wsProtocol = if p == "https:" then "wss:" else "ws:"
    let socketUrl = wsProtocol <> "//" <> h <> "/"
    startClient initialState callbacks socketUrl name


-- CALLBACKS
mkCallbacks =
  R.setupRendering <#> \ctx ->
    { render: render ctx
    , onMessage: onMessage
    , onKeyDown: onKeyDown
    , onError: onError
    , onClose: onClose
    }

render :: forall e. RenderingContext -> CM e Unit
render ctx = do
  state <- get
  pId <- askPlayerId
  case state of
    CInProgress g -> do
      liftEff $ R.render ctx g.game pId g.redrawMap
      put $ CInProgress (g # redrawMap .~ false)

      players <- askPlayers
      let html = V.simpleScores players g.game
      when (g.cachedHtml /= html) $ do
        liftEff $ q' "#scores-container" >>= setInnerHTML html
        put $ CInProgress (g # cachedHtml .~ html)

    CWaitingForPlayers sw -> do
      when (not (sw ^. backgroundCleared)) $ do
        liftEff $ R.clearBoth ctx
        put $ CWaitingForPlayers (sw # backgroundCleared .~ true)

      players <- askPlayers
      let html = V.waitingMessage sw pId players
      when (sw.cachedHtml /= html) $ do
        liftEff $ do
          el <- q "#waiting-message"
          whenJust el $ setInnerHTML html
        put $ CWaitingForPlayers (sw # cachedHtml .~ html)


onKeyDown :: forall e. DOMEvent -> CM e Unit
onKeyDown event = do
  state <- get
  code <- liftEff $ keyCode event

  case state of
    CInProgress _ -> do
      whenJust (directionFromKeyCode code) $ \direction ->
        sendUpdate (SIInProgress direction)

    CWaitingForPlayers sw -> do
      when (code == keyCodeSpace) do
        sendUpdate SIToggleReadyState

type CM e a = ClientM ClientState ServerIncomingMessage e a

matchInProgress :: forall e.
  ServerOutgoingMessage -> (Array GameUpdate -> CM e Unit) -> CM e Unit
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
              liftEff $ do
                showWaitingMessageDiv
                hideSimpleScoresDiv
              putWaitingState $ Just game'
            else do
              put $ CInProgress (g { game = game'
                                            , prevGame = g.game })

    CWaitingForPlayers g -> do
      matchWaiting msg $ \update -> do
        case update of
          GameStarting game -> do
            liftEff $ do
              hideWaitingMessageDiv
              showSimpleScoresDiv
            put $ CInProgress $ startNewGame game
          NewReadyStates m -> do
            put $ CWaitingForPlayers (g # readyStates .~ m)


startNewGame game =
  { game: game
  , prevGame: game
  , redrawMap: true
  , cachedHtml: ""
  }


onError = liftEff $ do
  withEl showElement "#error"
  withEl showElement "#error-overlay"

onClose = onError


putWaitingState :: forall e. Maybe Game -> CM e Unit
putWaitingState prevGame =
  put $ CWaitingForPlayers
          { prevGame: prevGame
          , backgroundCleared: false
          , readyStates: getStates prevGame
          , cachedHtml: ""
          }
  where
  getStates mg =
    case mg of
      Just g -> M.fromList $
        (\(Tuple pId _) -> pId ~ false) <$> (M.toList (g ^. players))
      Nothing -> M.empty


directionFromKeyCode :: Number -> Maybe Direction
directionFromKeyCode code =
  case code of
    38 -> Just Up
    40 -> Just Down
    37 -> Just Left
    39 -> Just Right
    _  -> Nothing

keyCodeSpace = 32
keyCodeEnter = 13

q sel = document globalWindow >>= querySelector sel

q' sel = fromJust <$> q sel

withEl f sel = do
  el <- q sel
  whenJust el $ f

showElement el = setAttribute "style" "display: block;" (el :: HTMLElement)
hideElement el = setAttribute "style" "display: none;" (el :: HTMLElement)

showWaitingMessageDiv = withEl showElement "#waiting-message"
hideWaitingMessageDiv = withEl hideElement "#waiting-message"

showSimpleScoresDiv = withEl showElement "#scores-container"
hideSimpleScoresDiv = withEl hideElement "#scores-container"
