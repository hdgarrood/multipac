module BaseClient where

import Prelude
import Data.Maybe
import Data.Tuple (fst, snd)
import Data.Foldable (for_)
import Data.Array (insertAt)
import Data.Map as M
import Data.Either as E
import Data.Generic.Rep (class Generic)
import Control.Monad.RWS.Trans
import Control.Monad.Writer.Class as W
import Control.Monad.Reader.Class as R
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Console
import Effect.Ref (newRef, readRef, writeRef, modifyRef, REF, Ref)
import Web.DOM (DOM)
import Graphics.Canvas (CANVAS)

import Types
import BaseCommon
import Utils

type Client st =
  { conn        :: WS.Connection
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

type ClientCallbacks st inc outg e =
  { onMessage     :: inc -> ClientM st outg e Unit
  , onKeyDown     :: DOMEvent -> ClientM st outg e Unit
  , render        :: ClientM st outg e Unit
  , onError       :: ClientM st outg e Unit
  , onClose       :: ClientM st outg e Unit
  }

type ClientM st outg e a =
  RWST ClientMReader (Array outg) st (Eff (ClientEffects e)) a

type ClientMResult st outg a =
  { nextState :: st
  , messages  :: Array outg
  , result    :: a
  }

type ClientEffects e =
  (ref :: REF, console :: CONSOLE, ws :: WS.WEBSOCKET, dom :: DOM,
   canvas :: CANVAS | e)

runClientM :: forall st outg e a.
  ClientMReader -> st -> ClientM st outg e a
  -> Eff (ClientEffects e) (ClientMResult st outg a)
runClientM rdr state action = do
  RWSResult nextState result messages <- runRWST action rdr state
  pure $
    { nextState
    , result
    , messages
    }

mkClient :: forall st. st -> WS.Connection -> PlayerId -> Client st
mkClient initialState conn pId =
  { conn
  , state: initialState
  , playerId: pId
  , players: M.empty
  }

runCallback :: forall st outg e. (Generic outg) =>
  Ref (Client st) -> ClientM st outg e Unit -> Eff (ClientEffects e) Unit
runCallback refCln callback = do
  cln <- readRef refCln
  res <- runClientM (getReader cln) cln.state callback
  sendAllMessages cln res.messages
  writeRef refCln $ cln { state = res.nextState }

sendAllMessages :: forall e st outg. (Generic outg) =>
  Client st -> Array outg -> Eff (ClientEffects e) Unit
sendAllMessages cln msgs =
  for_ msgs $ \msg ->
    wsSend cln.conn (encode msg)

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

liftEff :: forall st outg e a. Eff (ClientEffects e) a -> ClientM st outg e a
liftEff = lift

startClient :: forall st inc outg e.
  Generic inc =>
  Generic outg =>
  st -> ClientCallbacks st inc outg e -> String -> String
  -> Eff (ClientEffects e) Unit
startClient initialState cs socketUrl playerName =
  connectSocket socketUrl playerName \(WS.Connection conn) pId msgs internals -> do
    log $ "connected. pId = " <> show pId <> ", msgs = " <> show msgs
    let cln' = mkClient initialState (WS.Connection conn) pId
    refCln <- newRef cln'

    for_ msgs (onMessageCallback refCln)
    for_ internals (handleInternalMessage refCln)

    set conn.onmessage (onMessageCallback refCln <<< WS.runMessage <<< WS.runMessageEvent)
    set conn.onerror   (const (runCallback refCln cs.onError))
    set conn.onclose   (const (runCallback refCln cs.onClose))

    addKeyboardEventListener
      KeydownEvent
      (\event -> runCallback refCln (cs.onKeyDown event))
      globalWindow

    void $ startAnimationLoop $ do
      c <- readRef refCln
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
  Ref (Client st) -> InternalMessage -> Eff (ClientEffects e) Unit
handleInternalMessage refCln msg = do
  case msg of
    NewPlayer m -> do
      let m' = runGenericMap m
      log $ "handling NewPlayer internal message: " <> show m'
      modifyRef refCln $ \cln -> cln { players = m' }


connectSocket :: forall e.
  String -> String
  -> (WS.Connection -> PlayerId
                -> Array String
                -> Array InternalMessage
                -> Eff (ClientEffects e) Unit)
  -> Eff (ClientEffects e) Unit
connectSocket url playerName cont = do
  let fullUrl = url <> "?" <> playerName

  -- HACK - for messages received before the client is fully operational.
  delayedMsgs <- newRef ([] :: Array String)
  delayedInternals <- newRef ([] :: Array InternalMessage)
  WS.Connection conn <- WS.newWebSocket (WS.URL fullUrl) []

  set conn.onmessage \msgEv -> do
    let msg = WS.runMessage (WS.runMessageEvent msgEv)
    case decode msg of
      E.Right (YourPlayerIdIs pId) -> do
        delayed <- readRef delayedMsgs
        internals <- readRef delayedInternals
        cont (WS.Connection conn) pId delayed internals
      E.Right i -> modifyRef delayedInternals $ \arr -> arr <> [i]
      E.Left _ -> modifyRef delayedMsgs $ \arr -> arr <> [msg]


foreign import data AnimationLoop :: Type

foreign import startAnimationLoop :: forall a e. Eff e a -> Eff e AnimationLoop

foreign import stopAnimationLoop :: forall e. AnimationLoop -> Eff e Unit

wsSend :: WS.Connection -> String -> _
wsSend (WS.Connection r) msg = r.send (WS.Message msg)
