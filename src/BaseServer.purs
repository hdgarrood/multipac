module BaseServer where

import Debug.Trace (Trace(), trace)
import Data.Maybe
import Data.Tuple (fst, snd)
import Data.Foldable (for_)
import Data.Array (head, null, filter, (\\))
import qualified Data.Either as E
import qualified Data.String as S
import qualified Data.Map as M
import Data.Monoid (Monoid, mempty)
import Data.JSON (ToJSON, FromJSON, encode, eitherDecode)
import Control.Monad (when)
import Control.Monad.State (State(), runState)
import Control.Monad.Writer.Trans (WriterT(), runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, modifyRef, Ref(),
                              RefVal())
import Control.Reactive.Timer (Timer(), interval)
import Control.Lens (lens, (^.), (%~), (.~), at, LensP())

import Types
import qualified NodeWebSocket as WS
import Utils (unionWith)

type Server st =
  { connections :: [Connection]
  , state :: st
  }

connections = lens (\s -> s.connections) (\s x -> s { connections = x })

type Connection =
  { wsConn :: WS.Connection
  , pId :: PlayerId
  , name :: String
  }

type ServerCallbacks st inc outg =
  { step        :: ServerM st outg Unit
  , onNewPlayer :: PlayerId -> ServerM st outg Unit
  , onMessage   :: inc -> PlayerId -> ServerM st outg Unit
  , onClose     :: PlayerId -> ServerM st outg Unit
  }

newtype SendMessages outg
  = SendMessages
    { toAll :: [outg]
    , toOne :: M.Map PlayerId [outg]
    }

instance semigroupSendMsgs :: Semigroup (SendMessages outg) where
  (<>) (SendMessages a) (SendMessages b) =
    SendMessages { toAll: a.toAll <> b.toAll
                 , toOne: unionWith (<>) a.toOne b.toOne
                 }

instance monoidSendMsgs :: Monoid (SendMessages outg) where
  mempty = SendMessages { toAll: [], toOne: M.empty }

{-
Monad for server callbacks to run in. Rather than allowing Eff actions, we
just provide Writer and State as an interface to ask the server to do things.
This keeps all the game-specific server code pure! Hooray!

Note that, since you will usually want to send a bunch of outgoing messages at
a time, the type of message you're sending will end up being `[outg]` rather
than `outg`, so that's the type you should attempt to parse on the client.
-}
type ServerM st outg a =
  WriterT (SendMessages outg) (State st) a

type ServerMResult st outg a =
  { nextState :: st
  , messages  :: SendMessages outg
  , result    :: a
  }

runServerM :: forall st outg a.
  st -> ServerM st outg a -> ServerMResult st outg a
runServerM state action =
  let r1 = runWriterT action
      r2 = runState r1 state
  in { nextState: snd r2
     , messages: snd (fst r2)
     , result: fst (fst r2)
     }

mkServer :: forall st. st -> Server st
mkServer initialState =
  { connections: []
  , state: initialState
  }

runCallback :: forall st outg args e. (ToJSON outg) =>
  RefVal (Server st) -> ServerM st outg Unit 
  -> Eff (ServerEffects e) Unit
runCallback refSrv callback = do
  srv <- readRef refSrv
  let res = runServerM srv.state callback
  sendAllMessages srv res.messages
  writeRef refSrv $ srv { state = res.nextState }


messagesFor :: forall outg. PlayerId -> SendMessages outg -> [outg]
messagesFor pId (SendMessages sm) =
  sm.toAll <> fromMaybe [] (M.lookup pId sm.toOne)

sendAllMessages :: forall e st outg. (ToJSON outg) =>
  Server st -> SendMessages outg
  -> Eff (ServerEffects e) Unit
sendAllMessages srv sm = do
  for_ srv.connections $ \conn -> 
    let msgs = messagesFor conn.pId sm
    in when (not (null msgs)) $ do
      -- TODO: send as array. Need client implementation too though.
      for_ msgs $ \msg ->
        WS.send conn.wsConn (encode msg)

sendUpdate :: forall m outg. (Monad m, MonadWriter (SendMessages outg) m) =>
  outg -> m Unit
sendUpdate m = sendUpdates [m]

sendUpdates :: forall m outg. (Monad m, MonadWriter (SendMessages outg) m) =>
  [outg] -> m Unit
sendUpdates ms =
  tell $ SendMessages { toAll: ms, toOne: M.empty }

sendUpdateTo :: forall m outg. (Monad m, MonadWriter (SendMessages outg) m) =>
  PlayerId -> outg -> m Unit
sendUpdateTo pId m =
  tell $ SendMessages { toAll: [], toOne: M.insert pId [m] M.empty }

stepsPerSecond = 30

type ServerEffects e =
  (timer :: Timer, ref :: Ref, trace :: Trace, ws :: WS.WebSocket | e)

startServer :: forall st inc outg e. (FromJSON inc, ToJSON outg) =>
  ServerCallbacks st inc outg -> RefVal (Server st)
  -> Eff (ServerEffects e) WS.Server
startServer cs refSrv = do
  server <- WS.mkServer

  WS.onRequest server $ \req -> do
    trace "got a request"
    let playerName = S.drop 1 (WS.resourceUrl req).search

    if S.null playerName
      then WS.reject req
      else do
        conn <- WS.accept req
        maybePId <- tryAddPlayer conn refSrv playerName

        case maybePId of
          Just pId -> do
            trace $ "opened connection for player " <>
                        show pId <> ": " <> playerName
            runCallback refSrv $ cs.onNewPlayer pId 

            WS.onMessage conn $ \msg ->
              case eitherDecode msg of
                E.Right val -> runCallback refSrv $ cs.onMessage val pId
                E.Left err  -> trace err
            WS.onClose   conn $ \close -> do
              closeConnection refSrv pId
              runCallback refSrv $ cs.onClose pId
          Nothing -> do
            trace "rejecting connection, no player ids available"
            WS.close conn


  void $ interval (1000 / stepsPerSecond) $
    runCallback refSrv $ cs.step

  return server


tryAddPlayer conn refSrv name = do
  srv <- readRef refSrv
  case getNextPlayerId srv of
    Just pId -> do
      let srv' = srv { connections = srv.connections <>
                              [{ pId: pId, wsConn: conn, name: name }] }
      writeRef refSrv srv'
      return (Just pId)
    Nothing ->
      return Nothing

getNextPlayerId :: forall st. Server st -> Maybe PlayerId
getNextPlayerId srv =
  let playerIdsInUse = (\c -> c.pId) <$> srv.connections
  in  head $ allPlayerIds \\ playerIdsInUse


closeConnection :: forall st e.
  RefVal (Server st) -> PlayerId -> Eff (ServerEffects e) Unit
closeConnection refSrv pId = do
  modifyRef refSrv $ connections %~ filter (\c -> pId /= c.pId)
  trace $ "closed connection for " <> show pId
