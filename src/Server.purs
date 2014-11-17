module Server where

import Debug.Trace
import Data.Tuple
import Data.Function
import Data.Maybe
import Data.Foldable (for_)
import Data.Foreign.EasyFFI
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Ref
import Control.Reactive.Timer

import qualified NodeWebSocket as WS
import qualified NodeHttp as Http
import Types
import Game
import Utils

main = do
  unsafeForeignFunction [""] "process.chdir('../static')"
  port <- unsafeForeignFunction [""] "process.env.PORT || 8080"

  httpServer <- Http.createServer $ \req res -> do
    let path = (Http.getUrl req).pathname
    let reply =
      case path of
          "/"           -> Http.sendFile "index.html"
          "/js/game.js" -> Http.sendFile "js/game.js"
          _             -> Http.send404
    reply res

  wsServer <- mkWebSocketServer
  WS.mount wsServer httpServer

  Http.listen httpServer port
  trace $ "listening on " <> show port <> "..."

mkWebSocketServer :: forall e.
  Eff ( ws :: WS.WebSocket
      , timer :: Timer
      , ref :: Ref
      , trace :: Trace | e
      ) WS.Server
mkWebSocketServer = do
  server <- WS.mkServer
  WS.onRequest server handleRequest
  return server

handleRequest :: forall e.
  WS.Request
  -> Eff ( ws :: WS.WebSocket
         , timer :: Timer
         , ref :: Ref
         , trace :: Trace | e
         ) Unit
handleRequest req = do
  conn <- WS.accept req
  trace "opened connection"
  gameRef <- newRef initialGame
  inputRef <- newRef $ Input Nothing

  WS.onMessage conn (handleMessage inputRef)
  WS.onClose conn handleClose

  void $ interval 100 $ do
    input <- readRef inputRef
    game <- readRef gameRef

    let result = stepGame input game
    let game' = fst result
    let updates = snd result

    writeRef gameRef game'
    writeRef inputRef $ Input Nothing
    sendUpdates conn updates

handleMessage :: forall e.
  RefVal Input
  -> WS.Message
  -> Eff (trace :: Trace, ref :: Ref | e) Unit
handleMessage inputRef msg = do
  trace $ "got message: " <> msg
  whenJust (parseDirection msg) $ \newDirection ->
    writeRef inputRef (Input (Just newDirection))

handleClose :: forall a e.
  a -> Eff (ws :: WS.WebSocket, trace :: Trace | e) Unit
handleClose = const (trace "closed connection")

sendUpdates :: forall e.
  WS.Connection -> [GameUpdate] -> Eff (ws :: WS.WebSocket | e) Unit
sendUpdates conn updates =
  for_ updates (sendUpdate conn)

sendUpdate :: forall e.
  WS.Connection -> GameUpdate -> Eff (ws :: WS.WebSocket | e) Unit
sendUpdate conn (ChangedPosition p) = WS.send conn $ serializeChangedPosition p
sendUpdate _ _ = return unit

serializeChangedPosition :: Position -> String
serializeChangedPosition (Position p) =
  "{\"changedPosition\":{\"x\":" <> show p.x <> ",\"y\":" <> show p.y <> "}}"

parseDirection :: String -> Maybe Direction
parseDirection d =
  case d of
    "up"    -> Just Up
    "down"  -> Just Down
    "left"  -> Just Left
    "right" -> Just Right
    _       -> Nothing

