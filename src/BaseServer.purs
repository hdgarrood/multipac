module BaseServer where

import BaseCommon
import Control.Monad.RWS
import Control.Monad.RWS.Trans
import Data.Maybe
import Data.Tuple
import Effect.Console
import Prelude
import Types
import Utils

import Control.Monad (when)
import Control.Monad.Reader.Class as R
import Control.Monad.Writer.Class as W
import Data.Array (head, null, filter, (\\))
import Data.Either as E
import Data.Foldable (for_, find)
import Data.Lens (lens, Lens')
import Data.Lens.At (at)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~))
import Data.List as List
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Map as M
import Data.String as S
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import Global.Unsafe (unsafeDecodeURIComponent)
import NodeWebSocket as WS

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
  map (\(Connection c) -> Tuple c.pId c.name) >>> M.fromFoldable

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
                 , toOne: M.unionWith (<>) a.toOne b.toOne
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

runCallback :: forall st outg rep.
  Generic outg rep =>
  EncodeRep rep =>
  Ref (Server st) -> ServerM st outg Unit
  -> Effect Unit
runCallback refSrv callback = do
  srv <- Ref.read refSrv
  let res = runServerM srv.connections srv.state callback
  sendAllMessages srv res.messages
  Ref.write (srv { state = res.nextState }) refSrv

messagesFor :: forall outg. PlayerId -> SendMessages outg -> Array outg
messagesFor pId (SendMessages sm) =
  sm.toAll <> fromMaybe [] (M.lookup pId sm.toOne)

sendAllMessages :: forall st outg rep.
  Generic outg rep =>
  EncodeRep rep =>
  Server st -> SendMessages outg
  -> Effect Unit
sendAllMessages srv sm = do
  for_ srv.connections $ \conn ->
    let msgs = messagesFor (conn ^. cPId) sm
    in when (not (null msgs)) $ do
      for_ msgs $ \msg ->
        WS.send (conn ^. cWsConn) (encode msg)

sendUpdate :: forall m outg.
  Monad m =>
  W.MonadWriter (SendMessages outg) m =>
  outg -> m Unit
sendUpdate m = sendUpdates [m]

sendUpdates :: forall m outg.
  Monad m =>
  W.MonadWriter (SendMessages outg) m =>
  Array outg -> m Unit
sendUpdates ms =
  W.tell $ SendMessages { toAll: ms, toOne: M.empty }

sendUpdateTo :: forall m outg.
  Monad m =>
  W.MonadWriter (SendMessages outg) m =>
  PlayerId -> outg -> m Unit
sendUpdateTo pId m =
  W.tell $ SendMessages { toAll: [], toOne: M.insert pId [m] M.empty }

askPlayers :: forall m.
  Monad m =>
  R.MonadReader (Array Connection) m =>
  m (M.Map PlayerId String)
askPlayers =
  connectionsToPlayersMap <$> R.ask

stepsPerSecond = 30

startServer :: forall st inc rep1 rep2 outg e.
  Generic inc rep1 =>
  DecodeRep rep1 =>
  Generic outg rep2 =>
  EncodeRep rep2 =>
  ServerCallbacks st inc outg -> Ref (Server st)
  -> Effect WS.Server
startServer cs refSrv = do
  server <- WS.mkServer

  WS.onRequest server $ \req -> do
    log "got a request"
    let playerName = unsafeDecodeURIComponent (S.drop 1 (WS.resourceUrl req).search)

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
                E.Left err  -> log (show err)

            WS.onClose   conn $ \close -> do
              closeConnection refSrv pId
              runCallback refSrv $ cs.onClose pId

          Nothing -> do
            log "rejecting connection, no player ids available"
            WS.close conn


  void $ setInterval (1000 / stepsPerSecond) $
    runCallback refSrv $ cs.step

  void $ setInterval idleInterval $ do
    void $ kickIdlePlayers refSrv
    void $ updateServerTimeCounter refSrv

  pure server


tryAddPlayer conn refSrv name = do
  srv <- Ref.read refSrv
  case getNextPlayerId srv of
    Just pId -> do
      let srv' = srv { connections = srv.connections <>
                              [Connection { pId: pId
                                          , wsConn: conn
                                          , name: name
                                          , timeCounter: srv.timeCounter
                                          }] }
      Ref.write srv' refSrv
      pure (Just pId)
    Nothing ->
      pure Nothing

getNextPlayerId :: forall st. Server st -> Maybe PlayerId
getNextPlayerId srv =
  let playerIdsInUse = (\c -> c ^. cPId) <$> srv.connections
  in  head $ allPlayerIds \\ playerIdsInUse


handleNewPlayer :: forall st.
  Ref (Server st) -> PlayerId -> Effect Unit
handleNewPlayer refSrv pId = do
  srv <- Ref.read refSrv
  let playersMap = connectionsToPlayersMap srv.connections
  let msgAll = NewPlayer playersMap
  let msgOne = YourPlayerIdIs pId
  let msgs = SendMessages { toAll: [msgAll]
                          , toOne: M.singleton pId [msgOne]
                          }
  -- log $ "new player connected, sending to all: " <> encode msgAll
  -- log $ "                      sending to one: " <> encode msgOne
  -- sendAllMessages srv msgs
  -- FIXME
  pure unit


closeConnection :: forall st.
  Ref (Server st) -> PlayerId -> Effect Unit
closeConnection refSrv pId = do
  void $ Ref.modify (connections %~ filter (\c -> pId /= c ^. cPId)) refSrv
  log $ "closed connection for " <> show pId


updatePlayerTimeCounter refSrv pId = do
  srv <- Ref.read refSrv
  let mConn = find (\c -> c ^. cPId == pId) srv.connections
  whenJust mConn $ \conn -> do
    let conn' = conn # cTimeCounter .~ srv.timeCounter
    let conns' = filter (\c -> pId /= c ^. cPId) srv.connections
    let conns'' = conns' <> [conn']
    void $ Ref.modify (connections .~ conns'') refSrv


kickIdlePlayers refSrv = do
  srv <- Ref.read refSrv
  let idles = filter (\c -> c ^. cTimeCounter /= srv.timeCounter)
                     srv.connections
  for_ idles $ \c -> do
    WS.close (c ^. cWsConn)

updateServerTimeCounter =
  Ref.modify (timeCounter %~ ((+) 1))
