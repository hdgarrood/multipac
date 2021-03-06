module Server where

import Prelude
import Data.Tuple
import Data.Function
import Data.Maybe
import Data.String as String
import Data.Array hiding ((..))
import Data.List as List
import Data.Either as E
import Data.Map as M
import Data.Foldable (for_, all, find)
import Control.Monad
import Control.Monad.State.Class (get, put, modify)
import Effect
import Effect.Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (throw, message)
import Effect.Timer
import Data.Lens (lens, Lens'())
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~))
import Data.Lens.At (at)
import Node.HTTP as Http
import Node.Stream as Stream
import Node.FS.Async as FS
import Node.Encoding (Encoding(..))
import Text.Smolder.Renderer.String (render)

import NodeWebSocket as WS
import NodeUrl
import Types
import Game
import Utils
import BaseServer
import HtmlViews as HtmlViews

initialState :: GameState
initialState = WaitingForPlayers M.empty

main = do
  refSrv <- Ref.new (mkServer initialState)
  port <- portOrDefault 8080
  httpServer <- createHttpServer
  wsServer <- startServer serverCallbacks refSrv

  WS.mount wsServer httpServer
  Http.listen httpServer { hostname: "::", port, backlog: Nothing } $
    log $ "listening on " <> show port <> "..."


createHttpServer =
  Http.createServer $ \req res -> do
    let path = (parseUrl (Http.requestURL req)).pathname
    case path of
      "/" ->
        sendHtml res (render HtmlViews.indexHtml)
      "/js/client.js" ->
        sendFile res "dist/client.js"
      "/style.css" ->
        sendCss res HtmlViews.styles
      _ ->
        send404  res

serverCallbacks =
  { step: step
  , onMessage: onMessage
  , onNewPlayer: onNewPlayer
  , onClose: onClose
  }

type SM a = ServerM GameState ServerOutgoingMessage a

step :: SM Unit
step = do
  state <- get
  case state of
    InProgress g -> do
      let r = stepGame g.input g.game
      let game' = fst r
      let updates = snd r
      sendUpdate $ SOInProgress updates

      if isEnded game'
        then do
          players <- askPlayers
          put $ WaitingForPlayers (const NotReady <$> players)
        else do
          put $ InProgress { game: game', input: M.empty }

    WaitingForPlayers m -> do
      sendUpdate $ SOWaiting $ NewReadyStates m
      when (readyToStart m) do
        let game = makeGame (M.keys m)
        sendUpdate $ SOWaiting $ GameStarting $ WrappedGame game
        put $ InProgress { game: game, input: M.empty }
      where
      readyToStart m =
        let ps = M.values m
        in List.length ps >= minPlayers && all (_ == Ready) ps


matchInProgress :: ServerIncomingMessage -> (Direction -> SM Unit) -> SM Unit
matchInProgress = matchMessage asInProgressMessage

matchWaiting :: ServerIncomingMessage -> (Unit -> SM Unit) -> SM Unit
matchWaiting = matchMessage asWaitingMessage

onMessage :: ServerIncomingMessage -> PlayerId -> SM Unit
onMessage msg pId = do
  state <- get
  case state of
    InProgress g ->
      matchInProgress msg $ \newDir -> do
        let g' = g # input_ %~ M.insert pId (Just newDir)
        put $ InProgress g'

    WaitingForPlayers m -> do
      matchWaiting msg $ \_ -> do
        let m' = M.alter (map invertReadyState) pId m
        put $ WaitingForPlayers m'

onNewPlayer :: PlayerId -> SM Unit
onNewPlayer pId = do
  state <- get
  case state of
    WaitingForPlayers m -> do
      let m' = M.insert pId NotReady m
      put $ WaitingForPlayers m'
    _ -> pure unit


onClose :: PlayerId -> SM Unit
onClose pId = do
  state <- get
  case state of
    InProgress g -> do
      sendUpdate <<< SOInProgress <<< singleton $ GUPU pId PlayerLeft
      let g' = g # game_ %~ removePlayer pId
      put $ InProgress g'

    WaitingForPlayers m -> do
      let m' = M.delete pId m
      put $ WaitingForPlayers m'

sendHtml res html =
  sendContent res "text/html" html

sendCss res css =
  sendContent res "text/css" css

sendFile res path = do
  let mimeType = fromMaybe "text/plain" (detectMime path)
  FS.readTextFile UTF8 path \r ->
    case r of
      E.Right fileData ->
        void (sendContent res mimeType fileData)
      E.Left err ->
        throw ("While trying to read " <> path <> ": " <> message err)

sendContent res contentType contentData = do
  Http.setStatusCode res 200
  Http.setHeader res "Content-Type" contentType
  let stream = Http.responseAsStream res
  void $
    Stream.writeString stream UTF8 contentData $
      Stream.end stream (pure unit)

send404 res = do
  Http.setStatusCode res 404
  Http.setHeader res "Content-Type" "text/plain"
  let stream = Http.responseAsStream res
  void $
    Stream.writeString stream UTF8 "404: File not found" $
      Stream.end stream (pure unit)

-- detect the most likely mime type for a given filename
detectMime :: String -> Maybe String
detectMime str = do
  ext <- extension str
  case ext of
    "txt"  -> Just "text/plain"
    "html" -> Just "text/html"
    "css"  -> Just "text/css"
    "js"   -> Just "text/javascript"
    _      -> Nothing

extension :: String -> Maybe String
extension str =
  let arr = String.split (String.Pattern ".") str
      len = length arr
  in  arr !! (len - 1)
