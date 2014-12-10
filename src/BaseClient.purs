module BaseClient where

import Debug.Trace (Trace(), trace)
import Data.Maybe
import Data.Tuple (fst, snd)
import Data.Foldable (for_)
import Data.Array (insertAt)
import qualified Data.Map as M
import qualified Data.Either as E
import Data.JSON (ToJSON, FromJSON, encode, eitherDecode)
import Control.Monad.Trans
import Control.Monad.RWS.Trans
import Control.Monad.RWS.Class
import qualified Control.Monad.Writer.Class as W
import qualified Control.Monad.Reader.Class as R
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, modifyRef, Ref(),
                              RefVal())
import Data.DOM.Simple.Types (DOM(), DOMEvent())
import Data.DOM.Simple.Events (addKeyboardEventListener, KeyboardEventType(..))
import Data.DOM.Simple.Window (globalWindow)
import Graphics.Canvas (Canvas())

import Types
import qualified BrowserWebSocket as WS
import BaseCommon
import Utils

type Client st =
  { socket      :: WS.Socket
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
  }

type ClientM st outg e a =
  RWST ClientMReader [outg] st (Eff (ClientEffects e)) a

type ClientMResult st outg a =
  { nextState :: st
  , messages  :: [outg]
  , result    :: a
  }

type ClientEffects e =
  (ref :: Ref, trace :: Trace, ws :: WS.WebSocket, dom :: DOM,
   canvas :: Canvas | e)

runClientM :: forall st outg e a.
  ClientMReader -> st -> ClientM st outg e a
  -> Eff (ClientEffects e) (ClientMResult st outg a)
runClientM rdr state action = do
  r <- runRWST action rdr state
  return $
    { nextState: r.state
    , messages:  r.log
    , result:    r.result
    }

mkClient :: forall st. st -> WS.Socket -> PlayerId -> Client st
mkClient initialState socket pId =
  { socket: socket
  , state: initialState
  , playerId: pId
  , players: M.empty
  }

runCallback :: forall st outg args e. (ToJSON outg) =>
  RefVal (Client st) -> ClientM st outg e Unit -> Eff (ClientEffects e) Unit
runCallback refCln callback = do
  cln <- readRef refCln
  res <- runClientM (getReader cln) cln.state callback
  sendAllMessages cln res.messages
  writeRef refCln $ cln { state = res.nextState }

sendAllMessages :: forall e st outg. (ToJSON outg) =>
  Client st -> [outg] -> Eff (ClientEffects e) Unit
sendAllMessages cln msgs =
  for_ msgs $ \msg ->
    WS.send cln.socket (encode msg)

sendUpdate :: forall m outg. (Monad m, W.MonadWriter [outg] m) =>
  outg -> m Unit
sendUpdate m = sendUpdates [m]

sendUpdates :: forall m outg. (Monad m, W.MonadWriter [outg] m) =>
  [outg] -> m Unit
sendUpdates = W.tell

askPlayerId :: forall m. (Monad m, R.MonadReader ClientMReader m) =>
  m PlayerId
askPlayerId = (\(ClientMReader c) -> c.playerId) <$> R.ask

askPlayers :: forall m. (Monad m, R.MonadReader ClientMReader m) =>
  m (M.Map PlayerId String)
askPlayers = (\(ClientMReader c) -> c.players) <$> R.ask

askPlayerName :: forall m. (Monad m, R.MonadReader ClientMReader m) =>
  m String
askPlayerName = do
  pId <- askPlayerId
  f pId <$> R.ask
  where
  f pId (ClientMReader c) =
    fromMaybe err $ M.lookup pId c.players
  err = error "player name not found. this is a bug :/"

liftEff :: forall st outg e a. Eff (ClientEffects e) a -> ClientM st outg e a
liftEff = lift

startClient :: forall st inc outg e. (FromJSON inc, ToJSON outg) =>
  st -> ClientCallbacks st inc outg e -> String -> String
  -> Eff (ClientEffects e) Unit
startClient initialState cs socketUrl playerName =
  connectSocket socketUrl playerName $ \socket pId msgs -> do
    trace $ "connected. pId = " <> show pId <> ", msgs = " <> show msgs
    let cln' = mkClient initialState socket pId
    refCln <- newRef cln'

    for_ msgs (onMessageCallback refCln)
    WS.onMessage socket (onMessageCallback refCln)

    addKeyboardEventListener
      KeydownEvent
      (\event -> runCallback refCln (cs.onKeyDown event))
      globalWindow

    void $ startAnimationLoop $ do
      c <- readRef refCln
      runCallback refCln $ cs.render

  where
  onMessageCallback ref msg =
    case eitherDecode msg of
      E.Right val -> runCallback ref (cs.onMessage val)
      E.Left err ->
        case eitherDecode msg of
          E.Right intMsg -> handleInternalMessage ref intMsg
          E.Left _ -> trace err


handleInternalMessage :: forall st e.
  RefVal (Client st) -> InternalMessage -> Eff (ClientEffects e) Unit
handleInternalMessage refCln msg = do
  case msg of
    NewPlayer pId name -> do
      modifyRef refCln $ \cln ->
        cln { players = M.insert pId name cln.players }


connectSocket :: forall e.
  String -> String
  -> (WS.Socket -> PlayerId -> [String] -> Eff (ClientEffects e) Unit)
  -> Eff (ClientEffects e) Unit
connectSocket url playerName cont = do
  let fullUrl = url <> "?" <> playerName

  -- HACK - for messages received before the client is fully operational.
  delayedMsgs <- newRef ([] :: [String])
  sock <- WS.mkWebSocket fullUrl

  WS.onMessage sock $ \msg ->
    case eitherDecode msg of
      E.Right (NewPlayer pId _) -> readRef delayedMsgs >>= cont sock pId
      E.Left _ -> modifyRef delayedMsgs $ \arr -> arr <> [msg]


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
