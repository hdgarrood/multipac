module Server where

import Debug.Trace
import Data.Tuple
import Data.JSON
import Data.Function
import Data.Maybe
import qualified Data.Either as E
import qualified Data.Map as M
import Data.Foldable (for_)
import Data.Foreign.EasyFFI
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Ref
import Control.Reactive.Timer

import qualified NodeWebSocket as WS
import qualified NodeHttp as Http
import Types
import Game
import Utils

initialState :: ServerState
initialState =
  { game: initialGame
  , input: M.empty
  , connections: []
  , lastUsedPlayerId: Nothing
  }


main = do
  refState <- newRef initialState

  unsafeForeignFunction [""] "process.chdir('../static')"
  port <- unsafeForeignFunction [""] "process.env.PORT || 8080"

  httpServer <- createHttpServer 
  wsServer <- createWebSocketServer refState

  WS.mount wsServer httpServer
  Http.listen httpServer port
  trace $ "listening on " <> show port <> "..."

  startMainLoop refState


createHttpServer = 
  Http.createServer $ \req res -> do
    let path = (Http.getUrl req).pathname
    let reply =
      case path of
          "/"           -> Http.sendFile "index.html"
          "/js/game.js" -> Http.sendFile "js/game.js"
          _             -> Http.send404
    reply res


createWebSocketServer refState = do
  server <- WS.mkServer
  WS.onRequest server $ \req -> do
    trace "got a request"
    conn <- WS.accept req
    trace "opened connection"

    maybePId <- addPlayer conn refState

    whenJust maybePId $ \pId -> do
      WS.onMessage conn (handleMessage refState pId)

    WS.onClose conn handleClose

  return server


startMainLoop refState =
  void $ interval 35 $ do
    s <- readRef refState

    let result = stepGame s.input s.game
    let game' = fst result
    let updates = snd result

    let s' = s { game = game', input = M.empty }
    writeRef refState s'

    sendUpdates refState updates


addPlayer conn refState = do
  state <- readRef refState
  case nextPlayerId state.lastUsedPlayerId of
    Just pId -> do
      let state' = state
                    { connections = state.connections <> [Tuple conn pId]
                    , lastUsedPlayerId = Just pId
                    }
      writeRef refState state'
      return (Just pId)
    Nothing ->
      return Nothing


nextPlayerId :: Maybe PlayerId -> Maybe PlayerId
nextPlayerId Nothing = Just P1
nextPlayerId (Just P1) = Just P2
nextPlayerId (Just P2) = Just P3
nextPlayerId (Just P3) = Just P4
nextPlayerId (Just P4) = Nothing


handleMessage :: forall e.
  RefVal ServerState
  -> PlayerId
  -> WS.Message
  -> Eff (trace :: Trace, ws :: WS.WebSocket, ref :: Ref | e) Unit
handleMessage refState pId msg = do
  trace $ "got message: " <> msg
  case eitherDecode msg of
    E.Right newDir ->
      modifyRef refState $ \s ->
        let newInput = M.insert pId (Just newDir) s.input
        in  s { input = newInput }
    E.Left err ->
      trace err


handleClose :: forall a e.
  a -> Eff (ws :: WS.WebSocket, trace :: Trace | e) Unit
handleClose = const (trace "closed connection")


sendUpdates :: forall e.
  RefVal ServerState
  -> [GameUpdate]
  -> Eff (ref :: Ref, ws :: WS.WebSocket | e) Unit
sendUpdates refState updates = do
  state <- readRef refState
  for_ updates $ \update ->
    for_ state.connections $ \(Tuple conn _) ->
      sendUpdate conn update

sendUpdate :: forall e.
  WS.Connection -> GameUpdate -> Eff (ws :: WS.WebSocket | e) Unit
sendUpdate conn update =
  when (shouldBroadcast update) $
    WS.send conn $ encode update

shouldBroadcast :: GameUpdate -> Boolean
shouldBroadcast (GUPU _ (ChangedPosition _)) = true
shouldBroadcast _ = false
