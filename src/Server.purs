module Server where

import Debug.Trace
import Data.Tuple
import Data.JSON
import Data.Function
import Data.Maybe
import Data.Array
import qualified Data.String as S
import qualified Data.Either as E
import qualified Data.Map as M
import Data.Foldable (for_, all, find)
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer
import Control.Lens (lens, (^.), (%~), (.~), at, LensP())

import qualified NodeWebSocket as WS
import qualified NodeHttp as Http
import Types
import Game
import Utils

initialState :: ServerState
initialState =
  { gameState: WaitingForPlayers M.empty
  , connections: []
  }

stepsPerSecond = 30

main = do
  refState <- newRef initialState
  port <- portOrDefault 8080
  httpServer <- createHttpServer
  wsServer <- createWebSocketServer refState

  WS.mount wsServer httpServer
  Http.listen httpServer port
  trace $ "listening on " <> show port <> "..."

  void $ interval (1000 / stepsPerSecond) $
    runCallback refState step {}

createHttpServer =
  Http.createServer $ \req res -> do
    let path = (Http.getUrl req).pathname
    let reply =
      case path of
          "/"           -> Http.sendFile "static/index.html"
          "/js/game.js" -> Http.sendFile "static/js/game.js"
          _             -> Http.send404
    reply res


createWebSocketServer refState = do
  server <- WS.mkServer
  WS.onRequest server $ \req -> do
    trace "got a request"
    let playerName = S.drop 1 (WS.resourceUrl req).search

    if S.null playerName
      then WS.reject req
      else do
        conn <- WS.accept req
        maybePId <- tryAddPlayer conn refState playerName

        case maybePId of
          Just pId -> do
            trace $ "opened connection for player " <>
                        show pId <> ": " <> playerName
            runCallback refState onNewPlayer {pId:pId}

            WS.onMessage conn $ \msg ->
              runCallback refState onMessage {msg:msg, pId:pId}
            WS.onClose   conn $ \close ->
              runCallback refState onClose {pId:pId}
          Nothing -> do
            trace "rejecting connection, no player ids available"
            WS.close conn

  return server


runCallback refState callback args = do
  state <- readRef refState
  state' <- callback args state
  writeRef refState state'


tryAddPlayer conn refState name = do
  state <- readRef refState
  case getNextPlayerId state of
    Just pId -> do
      let state' = state { connections =
                            state.connections <>
                              [{ pId: pId, wsConn: conn, name: name }] }
      writeRef refState state'
      return (Just pId)
    Nothing ->
      return Nothing


getNextPlayerId state =
  let playerIdsInUse = map (\c -> c.pId) state.connections
  in  head $ allPlayerIds \\ playerIdsInUse

gameState = lens (\s -> s.gameState) (\s x -> s { gameState = x })

step args state = do
  case state ^. gameState of
    InProgress g -> do
      let r = stepGame g.input g.game
      let game' = fst r
      let updates = snd r
      let newState =
            if isEnded game'
              then WaitingForPlayers M.empty
              else InProgress { game: game', input: M.empty }

      sendUpdates state updates
      return $ state { gameState = newState }

    WaitingForPlayers m -> do
      if readyToStart m
        then do
          trace "all players are ready; starting game"
          let game = makeGame (M.keys m)
          sendUpdates state $ GameStarting game
          let newState = InProgress { game: game, input: M.empty }
          return $ state { gameState = newState }
        else
          return state
      where
      minPlayers = 2
      readyToStart m =
        let ps = M.values m
        in length ps >= minPlayers && all id ps

onMessage args state = do
  case state ^. gameState of
    InProgress g ->
      case (eitherDecode args.msg :: E.Either String Direction) of
        E.Right newDir -> do
          let newInput = M.insert args.pId (Just newDir) g.input
          return $ state
              { gameState = InProgress { game: g.game, input: newInput }}
        E.Left err -> do
          trace $ "failed to parse message: " <> err
          return state

    WaitingForPlayers m -> do
      case (eitherDecode args.msg :: E.Either String Boolean) of
         E.Right isReady -> do
           trace $ "updated ready state for " <> show args.pId <>
                     ": " <> show isReady
           let m' = M.insert args.pId isReady m
           return $ state { gameState = WaitingForPlayers m' }
         E.Left err -> do
           trace $ "failed to parse message: " <> err
           return state

onNewPlayer args state = do
  sendUpdateTo state args.pId (YourPlayerIdIs args.pId)
  return state

onClose args state = do
  let conns = filter (\c -> (args.pId::PlayerId) /= c.pId) state.connections
  let s' = state { connections = conns }
  trace $ "closed connection for " <> show args.pId
  return s'


sendUpdates :: forall e a. (ToJSON a) =>
  ServerState -> a -> Eff (ws :: WS.WebSocket | e) Unit
sendUpdates state updates = do
  for_ state.connections $ \c ->
    WS.send c.wsConn $ encode updates

sendUpdateTo :: forall e a. (ToJSON a) =>
  ServerState -> PlayerId -> a -> Eff (ws :: WS.WebSocket | e) Unit
sendUpdateTo state pId update = do
  let mConn = find (\c -> c.pId == pId) state.connections
  whenJust mConn $ \conn ->
    WS.send conn.wsConn $ encode update

foreign import chdir
  """
  function chdir(path) {
    return function() {
      process.chdir(path)
    }
  }
  """ :: forall e. String -> Eff (process :: Process | e) Unit

foreign import getEnvImpl
  """
  function getEnvImpl(just, nothing, key) {
    return function() {
      var v = process.env[key]
      return v ? just(v) : nothing
    }
  }
  """ :: forall e a.
  Fn3
    (a -> Maybe a)
    (Maybe a)
    String
    (Eff (process :: Process | e) (Maybe String))

getEnv :: forall e.
  String -> Eff (process :: Process | e) (Maybe String)
getEnv key =
  runFn3 getEnvImpl Just Nothing key

parseNumber :: String -> Maybe Number
parseNumber = decode

portOrDefault :: forall e.
  Number -> Eff (process :: Process | e) Number
portOrDefault default = do
  port <- getEnv "PORT"
  return $ fromMaybe default (port >>= parseNumber)
