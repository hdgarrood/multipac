module Client where

import BaseClient
import Control.Monad
import Control.Monad.State.Class
import Data.Maybe
import Data.Tuple
import Effect
import Effect.Timer
import Game
import Prelude
import Types
import Utils

import Data.Either as E
import Data.Foldable (for_)
import Data.Lens (lens, Lens')
import Data.Lens.At (at)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~))
import Data.List (List)
import Data.Map as M
import Data.String as S
import Data.Tuple.Nested ((/\))
import Effect.Exception (throw)
import Effect.Ref as Ref
import HtmlViews as V
import Rendering as R
import Text.Smolder.Renderer.DOM (patch)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)
import Web.DOM.Element (Element)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLElement (HTMLElement, focus)
import Web.HTML.HTMLInputElement (value, setValue, select)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Location as Location
import Web.HTML.Window (localStorage)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KBDEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

initialState =
  CWaitingForPlayers
    { prevGame: Nothing
    , backgroundCleared: false
    , readyStates: M.empty
    }

main = do
  hideSimpleScoresDiv
  startRef <- Ref.new false
  loop startRef
  where
  loop startRef =
    promptScreenName "Enter a screen name:" $ \name ->
      if S.null (S.trim name)
         then loop startRef
         else start startRef name

promptScreenName :: String -> (String -> Effect Unit) -> Effect Unit
promptScreenName msg cont = do
  let key = "screenName"
  localStorage <- window >>= Window.localStorage
  mVal <- Storage.getItem key localStorage
  let val = fromMaybe "" mVal
  popupPromptInput msg val $ \name -> do
    Storage.setItem key name localStorage
    cont name

-- TODO: Use an actual form here
popupPromptInput msg val cont = do
  containerEl <- q' "#prompt"
  showElement containerEl

  messageEl <- q' "#prompt-message"
  Node.setTextContent msg (Element.toNode messageEl)

  minputEl <- map (\el -> el >>= HTMLInputElement.fromElement) (q "#prompt-input")
  case minputEl of
    Nothing ->
      throw "Could not locate HTML input element #prompt-input"
    Just inputEl -> do
      setValue val inputEl
      focus (HTMLInputElement.toHTMLElement inputEl)
      select inputEl

      keydownListener <- eventListener \evt ->
        for_ (KBDEvent.fromEvent evt) (handleKeydown containerEl inputEl)
      addEventListener keydown keydownListener false (HTMLInputElement.toEventTarget inputEl)

  where
  handleKeydown cEl iEl event = do
    when (KBDEvent.key event == keyEnter) $ do
      hideElement cEl
      value iEl >>= cont

start startedRef name = do
  started <- Ref.read startedRef
  when (not started) $ do
    Ref.write true startedRef

    callbacks <- mkCallbacks
    loc <- window >>= Window.location
    h <- Location.host loc
    p <- Location.protocol loc

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

render :: forall e. RenderingContext -> CM Unit
render ctx = do
  state <- get
  pId <- askPlayerId
  case state of
    CInProgress g -> do
      liftEff $ R.render ctx g.game pId g.redrawMap
      put $ CInProgress (g # redrawMap .~ false)

      players <- askPlayers
      let html = V.simpleScores players g.game
      liftEff $ q' "#scores-container" >>= flip patch html

    CWaitingForPlayers sw -> do
      when (not (sw ^. backgroundCleared)) $ do
        liftEff $ R.clearBoth ctx
        put $ CWaitingForPlayers (sw # backgroundCleared .~ true)

      players <- askPlayers
      let html = V.waitingMessage sw pId players
      liftEff $ do
        el <- q "#waiting-message"
        whenJust el $ flip patch html


onKeyDown :: forall e. KeyboardEvent -> CM Unit
onKeyDown event = do
  let key = KBDEvent.key event

  state <- get
  case state of
    CInProgress _ -> do
      whenJust (directionFromKey key) $ \direction ->
        sendUpdate (SIInProgress direction)

    CWaitingForPlayers sw -> do
      when (key == keySpace) do
        sendUpdate SIToggleReadyState

type CM a = ClientM ClientState ServerIncomingMessage a

matchInProgress :: forall e.
  ServerOutgoingMessage -> (Array GameUpdate -> CM Unit) -> CM Unit
matchInProgress = matchMessage asInProgressMessageO

matchWaiting :: forall e.
  ServerOutgoingMessage -> (WaitingUpdate -> CM Unit) -> CM Unit
matchWaiting = matchMessage asWaitingMessageO

onMessage :: ServerOutgoingMessage -> _
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
            put $ CInProgress $ startNewGame $ unwrapGame game
          NewReadyStates m -> do
            put $ CWaitingForPlayers (g # readyStates .~ m)


startNewGame game =
  { game: game
  , prevGame: game
  , redrawMap: true
  }


onError = liftEff $ do
  withEl showElement "#error"
  withEl showElement "#error-overlay"

onClose = onError


putWaitingState :: forall e. Maybe Game -> CM Unit
putWaitingState prevGame =
  put $ CWaitingForPlayers
          { prevGame: prevGame
          , backgroundCleared: false
          , readyStates: getStates prevGame
          }
  where
  getStates mg =
    case mg of
      Just g -> M.fromFoldable $
        (\(Tuple pId _) -> pId /\ NotReady) <$> (asList (g ^. players))
      Nothing -> M.empty

  asList = M.toUnfoldable :: M.Map _ _ -> List _

directionFromKey :: String -> Maybe Direction
directionFromKey code =
  case code of
    "ArrowUp" -> Just Up
    "ArrowDown" -> Just Down
    "ArrowLeft" -> Just Left
    "ArrowRight" -> Just Right
    _  -> Nothing

keySpace = " "
keyEnter = "Enter"

q :: String -> Effect (Maybe Element)
q sel =
  window
  >>= Window.document
  >>= (querySelector (QuerySelector sel) <<< Document.toParentNode)

q' :: String -> Effect Element
q' sel = do
  el <- q sel
  case el of
    Just el -> pure el
    Nothing -> throw $ "Could not find an element matching " <> sel

withEl f sel = do
  el <- q sel
  whenJust el $ f

showElement el = setAttribute "style" "display: block;" el
hideElement el = setAttribute "style" "display: none;" el

showWaitingMessageDiv = withEl showElement "#waiting-message"
hideWaitingMessageDiv = withEl hideElement "#waiting-message"

showSimpleScoresDiv = withEl showElement "#scores-container"
hideSimpleScoresDiv = withEl hideElement "#scores-container"

removeAllChildren :: Node -> Effect Unit
removeAllChildren node = do
  mchild <- Node.firstChild node
  case mchild of
    Just child -> do
      void $ Node.removeChild child node
      removeAllChildren node
    Nothing ->
      pure unit
