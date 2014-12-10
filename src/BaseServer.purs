module BaseServer where

import Debug.Trace (Trace(), trace)
import Data.Maybe
import Data.Tuple
import Data.Foldable (for_, find)
import Data.Array (map, head, null, filter, (\\))
import qualified Data.Either as E
import qualified Data.String as S
import qualified Data.Map as M
import Data.Monoid (Monoid, mempty)
import Data.JSON
import Control.Monad (when)
import Control.Monad.RWS
import Control.Monad.RWS.Class
import qualified Control.Monad.Writer.Class as W
import qualified Control.Monad.Reader.Class as R
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, modifyRef, Ref(),
                              RefVal())
import Control.Reactive.Timer (Timer(), interval)
import Control.Lens (lens, (^.), (%~), (.~), at, LensP())

import Types
import BaseCommon
import qualified NodeWebSocket as WS
import Utils (unionWith)

type Server st =
  { connections :: [Connection]
  , state :: st
  }

connections = lens (\s -> s.connections) (\s x -> s { connections = x })

newtype Connection =
  Connection
    { wsConn :: WS.Connection
    , pId :: PlayerId
    , name :: String
    }

cWsConn = lens
  (\(Connection c) -> c.wsConn)
  (\(Connection c) x -> Connection $ c { wsConn = x })


cPId = lens
  (\(Connection c) -> c.pId)
  (\(Connection c) x -> Connection $ c { pId = x })

cName = lens
  (\(Connection c) -> c.name)
  (\(Connection c) x -> Connection $ c { name = x })

connectionsToPlayersMap :: [Connection] -> M.Map PlayerId String
connectionsToPlayersMap =
  map (\(Connection c) -> Tuple c.pId c.name) >>> M.fromList

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

type ServerM st outg a =
  RWS [Connection] (SendMessages outg) st a

type ServerMResult st outg a =
  { nextState :: st
  , messages  :: SendMessages outg
  , result    :: a
  }

runServerM :: forall st outg a.
  [Connection] -> st -> ServerM st outg a -> ServerMResult st outg a
runServerM conns state action =
  let r = runRWS action conns state
  in { nextState: r.state
     , messages:  r.log
     , result:    r.result
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
  let res = runServerM srv.connections srv.state callback
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
    let msgs = messagesFor (conn ^. cPId) sm
    in when (not (null msgs)) $ do
      for_ msgs $ \msg ->
        WS.send (conn ^. cWsConn) (encode msg)

sendUpdate :: forall m outg. (Monad m, W.MonadWriter (SendMessages outg) m) =>
  outg -> m Unit
sendUpdate m = sendUpdates [m]

sendUpdates :: forall m outg. (Monad m, W.MonadWriter (SendMessages outg) m) =>
  [outg] -> m Unit
sendUpdates ms =
  W.tell $ SendMessages { toAll: ms, toOne: M.empty }

sendUpdateTo :: forall m outg. (Monad m, W.MonadWriter (SendMessages outg) m) =>
  PlayerId -> outg -> m Unit
sendUpdateTo pId m =
  W.tell $ SendMessages { toAll: [], toOne: M.insert pId [m] M.empty }

getPlayerName :: forall m. (Monad m, R.MonadReader [Connection] m) =>
  PlayerId -> m (Maybe String)
getPlayerName pId =
  let f conns = (\c -> c ^. cName) <$>
                    find (\c -> c ^. cPId == pId) (conns :: [Connection])
  in f <$> R.ask

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
            handleNewPlayer refSrv pId
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
                              [Connection { pId: pId
                                          , wsConn: conn
                                          , name: name }] }
      writeRef refSrv srv'
      return (Just pId)
    Nothing ->
      return Nothing

getNextPlayerId :: forall st. Server st -> Maybe PlayerId
getNextPlayerId srv =
  let playerIdsInUse = (\c -> c ^. cPId) <$> srv.connections
  in  head $ allPlayerIds \\ playerIdsInUse


handleNewPlayer :: forall st e.
  RefVal (Server st) -> PlayerId -> Eff (ServerEffects e) Unit
handleNewPlayer refSrv pId = do
  srv <- readRef refSrv
  let playersMap = connectionsToPlayersMap srv.connections
  let msgAll = NewPlayer playersMap
  let msgOne = YourPlayerIdIs pId
  let msgs = SendMessages { toAll: [msgAll]
                          , toOne: M.singleton pId [msgOne]
                          }
  trace $ "new player connected, sending to all: " <> encode msgAll
  trace $ "                      sending to one: " <> encode msgOne
  sendAllMessages srv msgs


closeConnection :: forall st e.
  RefVal (Server st) -> PlayerId -> Eff (ServerEffects e) Unit
closeConnection refSrv pId = do
  modifyRef refSrv $ connections %~ filter (\c -> pId /= c ^. cPId)
  trace $ "closed connection for " <> show pId
