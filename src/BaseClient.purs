module BaseClient where

import BaseCommon
import Control.Monad.RWS.Trans
import Data.Maybe
import Effect.Console
import Prelude
import Types
import Utils

import Control.Monad.Reader.Class as R
import Control.Monad.Writer.Class as W
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Array (insertAt)
import Data.Either as E
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign as F
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, eventListener)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSE
import Web.Socket.Event.MessageEvent as WSME
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KBD
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type Client st =
  { sock        :: WebSocket
  , state       :: st
  , playerId    :: PlayerId
  , players     :: M.Map PlayerId String
  }

data ClientMReader =
  ClientMReader
    { playerId :: PlayerId
    , players  :: M.Map PlayerId String
    }

getReader :: forall st. Client st -> ClientMReader
getReader c = ClientMReader { playerId: c.playerId, players: c.players }

type ClientCallbacks st inc outg =
  { onMessage     :: inc -> ClientM st outg Unit
  , onKeyDown     :: KeyboardEvent -> ClientM st outg Unit
  , render        :: ClientM st outg Unit
  , onError       :: ClientM st outg Unit
  , onClose       :: ClientM st outg Unit
  }

type ClientM st outg a =
  RWST ClientMReader (Array outg) st (Effect) a

type ClientMResult st outg a =
  { nextState :: st
  , messages  :: Array outg
  , result    :: a
  }

runClientM :: forall st outg e a.
  ClientMReader -> st -> ClientM st outg a
  -> Effect (ClientMResult st outg a)
runClientM rdr state action = do
  RWSResult nextState result messages <- runRWST action rdr state
  pure $
    { nextState
    , result
    , messages
    }

mkClient :: forall st. st -> WebSocket -> PlayerId -> Client st
mkClient initialState sock pId =
  { sock
  , state: initialState
  , playerId: pId
  , players: M.empty
  }

runCallback :: forall st outg rep.
  Generic outg rep =>
  EncodeRep rep =>
  Ref (Client st) -> ClientM st outg Unit -> Effect Unit
runCallback refCln callback = do
  cln <- Ref.read refCln
  res <- runClientM (getReader cln) cln.state callback
  sendAllMessages cln res.messages
  Ref.write (cln { state = res.nextState }) refCln

sendAllMessages :: forall st outg rep.
  Generic outg rep =>
  EncodeRep rep =>
  Client st -> Array outg -> Effect Unit
sendAllMessages cln msgs =
  for_ msgs $ \msg ->
    WS.sendString cln.sock (encode msg)

sendUpdate :: forall m outg.
  Monad m =>
  W.MonadWriter (Array outg) m =>
  outg -> m Unit
sendUpdate m = sendUpdates [m]

sendUpdates :: forall m outg.
  Monad m =>
  W.MonadWriter (Array outg) m =>
  Array outg -> m Unit
sendUpdates = W.tell

askPlayerId :: forall m.
  Monad m =>
  R.MonadReader ClientMReader m =>
  m PlayerId
askPlayerId = (\(ClientMReader c) -> c.playerId) <$> R.ask

askPlayers :: forall m.
  Monad m =>
  R.MonadReader ClientMReader m =>
  m (M.Map PlayerId String)
askPlayers = (\(ClientMReader c) -> c.players) <$> R.ask

askPlayerName :: forall m.
  Monad m =>
  R.MonadReader ClientMReader m =>
  m String
askPlayerName = do
  pId <- askPlayerId
  f pId <$> R.ask
  where
  f pId (ClientMReader c) =
    fromMaybe err $ M.lookup pId c.players
  err = unsafeThrow "player name not found. this is a bug :/"

liftEff :: forall st outg a. Effect a -> ClientM st outg a
liftEff = lift

startClient :: forall st inc outg rep1 rep2.
  Generic inc rep1 =>
  DecodeRep rep1 =>
  Generic outg rep2 =>
  EncodeRep rep2 =>
  st -> ClientCallbacks st inc outg -> String -> String
  -> Effect Unit
startClient initialState cs socketUrl playerName =
  connectSocket socketUrl playerName \sock pId msgs internals -> do
    log $ "connected. pId = " <> show pId <> ", msgs = " <> show msgs
    let cln' = mkClient initialState sock pId
    refCln <- Ref.new cln'

    for_ msgs (onMessageCallback refCln)
    for_ internals (handleInternalMessage refCln)

    let sockTarget = WS.toEventTarget sock

    onMessageListener <- eventListener \evt ->
      for_ (WSME.fromEvent evt >>= wsMessageData) (onMessageCallback refCln)
    addEventListener WSE.onMessage onMessageListener false sockTarget

    onErrorListener <- eventListener (const (runCallback refCln cs.onError))
    addEventListener WSE.onError onErrorListener false sockTarget

    onCloseListener <- eventListener (const (runCallback refCln cs.onClose))
    addEventListener WSE.onClose onCloseListener false sockTarget

    keydownListener <- eventListener \evt ->
        for_ (KBD.fromEvent evt) (runCallback refCln <<< cs.onKeyDown)
    windowTarget <- map Window.toEventTarget window
    addEventListener keydown keydownListener false windowTarget

    void $ startAnimationLoop $ do
      c <- Ref.read refCln
      runCallback refCln cs.render

  where
  onMessageCallback ref msg = do
    case decode msg :: _ inc of
      E.Right val -> runCallback ref (cs.onMessage val)
      E.Left err ->
        case decode msg :: _ InternalMessage of
          E.Right intMsg ->
            handleInternalMessage ref intMsg
          E.Left err2 ->
            error ("Unable to parse message:\n" <>
              "data: " <> msg <> "\n" <>
              "errors: " <> show err <> "\n" <> show err2)


handleInternalMessage :: forall st e.
  Ref (Client st) -> InternalMessage -> Effect Unit
handleInternalMessage refCln msg = do
  case msg of
    NewPlayer m -> do
      log $ "handling NewPlayer internal message: " <> show m
      void $ Ref.modify (\cln -> cln { players = m }) refCln
    YourPlayerIdIs _ ->
      pure unit


connectSocket :: forall e.
  String -> String
  -> (WebSocket -> PlayerId
                -> Array String
                -> Array InternalMessage
                -> Effect Unit)
  -> Effect Unit
connectSocket url playerName cont = do
  let fullUrl = url <> "?" <> playerName

  -- HACK - for messages received before the client is fully operational.
  delayedMsgs <- Ref.new ([] :: Array String)
  delayedInternals <- Ref.new ([] :: Array InternalMessage)
  sock <- WS.create fullUrl []

  let
    onMessage ev =
      for_ (WSME.fromEvent ev >>= wsMessageData) \msg ->
        case decode msg of
          E.Right (YourPlayerIdIs pId) -> do
            delayed <- Ref.read delayedMsgs
            internals <- Ref.read delayedInternals
            cont sock pId delayed internals
          E.Right i -> void $ Ref.modify (\arr -> arr <> [i]) delayedInternals
          E.Left _ -> void $ Ref.modify (\arr -> arr <> [msg]) delayedMsgs

  onMessageListener <- eventListener onMessage
  addEventListener WSE.onMessage onMessageListener false (WS.toEventTarget sock)

wsMessageData :: WSME.MessageEvent -> Maybe String
wsMessageData =
  hush <<< F.readString <<< WSME.data_
  where
  hush :: forall a. F.F a -> Maybe a
  hush = E.hush <<< unwrap <<< unwrap

foreign import data AnimationLoop :: Type

foreign import startAnimationLoop :: forall a. Effect a -> Effect AnimationLoop

foreign import stopAnimationLoop :: AnimationLoop -> Effect Unit
