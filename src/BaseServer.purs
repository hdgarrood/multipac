module BaseServer where

import Prelude
import Data.Maybe
import Data.Tuple
import Data.Foldable (for_, find)
import Data.Array (head, null, filter, (\\))
import Data.List as List
import Data.Either as E
import Data.String as S
import Data.Map as M
import Data.Monoid (Monoid, mempty)
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Control.Monad (when)
import Control.Monad.RWS
import Control.Monad.RWS.Trans
import Control.Monad.RWS.Class
import Control.Monad.Writer.Class as W
import Control.Monad.Reader.Class as R
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref (newRef, readRef, writeRef, modifyRef, REF(),
                              Ref())
import DOM.Timer (Timer(), interval)
import Data.Lens (lens, LensP())
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~))
import Data.Lens.At (at)
import Global (decodeURIComponent)

import Types
import GenericMap
import BaseCommon
import NodeWebSocket as WS
import Utils

idleInterval = 30 * 1000

type Server st =
  { connections :: Array Connection
  , state       :: st
  , timeCounter :: Int -- for tracking whether players are idle
  }

connections = lens (\s -> s.connections) (\s x -> s { connections = x })
timeCounter = lens (\s -> s.timeCounter) (\s x -> s { timeCounter = x })

newtype Connection =
  Connection
    { wsConn      :: WS.Connection
    , pId         :: PlayerId
    , name        :: String
    , timeCounter :: Int -- for tracking whether the player is idle
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

cTimeCounter = lens
  (\(Connection c) -> c.timeCounter)
  (\(Connection c) x -> Connection $ c { timeCounter = x })

connectionsToPlayersMap :: Array Connection -> M.Map PlayerId String
connectionsToPlayersMap =
  map (\(Connection c) -> Tuple c.pId c.name) >>> List.fromFoldable >>> M.fromList

type ServerCallbacks st inc outg =
  { step        :: ServerM st outg Unit
  , onNewPlayer :: PlayerId -> ServerM st outg Unit
  , onMessage   :: inc -> PlayerId -> ServerM st outg Unit
  , onClose     :: PlayerId -> ServerM st outg Unit
  }

newtype SendMessages outg
  = SendMessages
    { toAll :: Array outg
    , toOne :: M.Map PlayerId (Array outg)
    }

instance semigroupSendMsgs :: Semigroup (SendMessages outg) where
  append (SendMessages a) (SendMessages b) =
    SendMessages { toAll: a.toAll <> b.toAll
                 , toOne: unionWith (<>) a.toOne b.toOne
                 }

instance monoidSendMsgs :: Monoid (SendMessages outg) where
  mempty = SendMessages { toAll: [], toOne: M.empty }

type ServerM st outg a =
  RWS (Array Connection) (SendMessages outg) st a

type ServerMResult st outg a =
  { nextState :: st
  , messages  :: SendMessages outg
  , result    :: a
  }

runServerM :: forall st outg a.
  Array Connection -> st -> ServerM st outg a -> ServerMResult st outg a
runServerM conns state action =
  case runRWS action conns state of
    RWSResult nextState result messages ->
      { nextState
      , messages
      , result
      }

mkServer :: forall st. st -> Server st
mkServer initialState =
  { connections: []
  , state: initialState
  , timeCounter: 0
  }

runCallback :: forall st outg args e. (EncodeJson outg) =>
  Ref (Server st) -> ServerM st outg Unit
  -> Eff (ServerEffects e) Unit
runCallback refSrv callback = do
  srv <- readRef refSrv
  let res = runServerM srv.connections srv.state callback
  sendAllMessages srv res.messages
  writeRef refSrv $ srv { state = res.nextState }

messagesFor :: forall outg. PlayerId -> SendMessages outg -> Array outg
messagesFor pId (SendMessages sm) =
  sm.toAll <> fromMaybe [] (M.lookup pId sm.toOne)

sendAllMessages :: forall e st outg. (EncodeJson outg) =>
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
  Array outg -> m Unit
sendUpdates ms =
  W.tell $ SendMessages { toAll: ms, toOne: M.empty }

sendUpdateTo :: forall m outg. (Monad m, W.MonadWriter (SendMessages outg) m) =>
  PlayerId -> outg -> m Unit
sendUpdateTo pId m =
  W.tell $ SendMessages { toAll: [], toOne: M.insert pId [m] M.empty }

askPlayers :: forall m. (Monad m, R.MonadReader (Array Connection) m) =>
  m (M.Map PlayerId String)
askPlayers =
  connectionsToPlayersMap <$> R.ask

stepsPerSecond = 30

type ServerEffects e =
  (timer :: Timer, ref :: REF, console :: CONSOLE, ws :: WS.WebSocket | e)

startServer :: forall st inc outg e. (DecodeJson inc, EncodeJson outg) =>
  ServerCallbacks st inc outg -> Ref (Server st)
  -> Eff (ServerEffects e) WS.Server
startServer cs refSrv = do
  server <- WS.mkServer

  WS.onRequest server $ \req -> do
    log "got a request"
    let playerName = decodeURIComponent (S.drop 1 (WS.resourceUrl req).search)

    if S.null playerName
      then WS.reject req
      else do
        conn <- WS.accept req
        maybePId <- tryAddPlayer conn refSrv playerName

        case maybePId of
          Just pId -> do
            log $ "opened connection for player " <>
                        show pId <> ": " <> playerName
            handleNewPlayer refSrv pId
            runCallback refSrv $ cs.onNewPlayer pId

            WS.onMessage conn $ \msg -> do
              updatePlayerTimeCounter refSrv pId
              case decode msg of
                E.Right val -> runCallback refSrv $ cs.onMessage val pId
                E.Left err  -> log err

            WS.onClose   conn $ \close -> do
              closeConnection refSrv pId
              runCallback refSrv $ cs.onClose pId

          Nothing -> do
            log "rejecting connection, no player ids available"
            WS.close conn


  void $ interval (1000 / stepsPerSecond) $
    runCallback refSrv $ cs.step

  interval idleInterval $ do
    kickIdlePlayers refSrv
    updateServerTimeCounter refSrv

  return server


tryAddPlayer conn refSrv name = do
  srv <- readRef refSrv
  case getNextPlayerId srv of
    Just pId -> do
      let srv' = srv { connections = srv.connections <>
                              [Connection { pId: pId
                                          , wsConn: conn
                                          , name: name
                                          , timeCounter: srv.timeCounter
                                          }] }
      writeRef refSrv srv'
      return (Just pId)
    Nothing ->
      return Nothing

getNextPlayerId :: forall st. Server st -> Maybe PlayerId
getNextPlayerId srv =
  let playerIdsInUse = (\c -> c ^. cPId) <$> srv.connections
  in  head $ allPlayerIds \\ playerIdsInUse


handleNewPlayer :: forall st e.
  Ref (Server st) -> PlayerId -> Eff (ServerEffects e) Unit
handleNewPlayer refSrv pId = do
  srv <- readRef refSrv
  let playersMap = connectionsToPlayersMap srv.connections
  let msgAll = NewPlayer (GenericMap playersMap)
  let msgOne = YourPlayerIdIs pId
  let msgs = SendMessages { toAll: [msgAll]
                          , toOne: M.singleton pId [msgOne]
                          }
  log $ "new player connected, sending to all: " <> encode msgAll
  log $ "                      sending to one: " <> encode msgOne
  sendAllMessages srv msgs


closeConnection :: forall st e.
  Ref (Server st) -> PlayerId -> Eff (ServerEffects e) Unit
closeConnection refSrv pId = do
  modifyRef refSrv $ connections %~ filter (\c -> pId /= c ^. cPId)
  log $ "closed connection for " <> show pId


updatePlayerTimeCounter refSrv pId = do
  srv <- readRef refSrv
  let mConn = find (\c -> c ^. cPId == pId) srv.connections
  whenJust mConn $ \conn -> do
    let conn' = conn # cTimeCounter .~ srv.timeCounter
    let conns' = filter (\c -> pId /= c ^. cPId) srv.connections
    let conns'' = conns' <> [conn']
    modifyRef refSrv $ connections .~ conns''


kickIdlePlayers refSrv = do
  srv <- readRef refSrv
  let idles = filter (\c -> c ^. cTimeCounter /= srv.timeCounter)
                     srv.connections
  for_ idles $ \c -> do
    WS.close (c ^. cWsConn)

updateServerTimeCounter refSrv =
  modifyRef refSrv $ timeCounter %~ ((+) 1)
