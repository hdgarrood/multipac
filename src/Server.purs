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
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer
import Control.Lens (lens, (^.), (%~), (.~), (%=), at, LensP())

import qualified NodeWebSocket as WS
import qualified NodeHttp as Http
import Types
import Game
import Utils
import ServerState

initialState :: GameState
initialState = WaitingForPlayers M.empty

main = do
  refSrv <- newRef (mkServer initialState)
  port <- portOrDefault 8080
  httpServer <- createHttpServer
  wsServer <- startServer serverCallbacks refSrv

  WS.mount wsServer httpServer
  Http.listen httpServer port
  trace $ "listening on " <> show port <> "..."


createHttpServer =
  Http.createServer $ \req res -> do
    let path = (Http.getUrl req).pathname
    let reply =
      case path of
          "/"           -> Http.sendFile "static/index.html"
          "/js/game.js" -> Http.sendFile "static/js/game.js"
          _             -> Http.send404
    reply res


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

      put $ if isEnded game'
              then WaitingForPlayers M.empty
              else InProgress { game: game', input: M.empty }

    WaitingForPlayers m -> do
      when (readyToStart m) do
        -- tracePM "all players are ready; starting game"
        let game = makeGame (M.keys m)
        sendUpdate $ SOWaiting $ GameStarting game
        put $ InProgress { game: game, input: M.empty }
      where
      readyToStart m =
        let ps = M.values m
        in length ps >= minPlayers && all id ps


matchInProgress :: ServerIncomingMessage -> (Direction -> SM Unit) -> SM Unit
matchInProgress = matchMessage asInProgressMessage

matchWaiting :: ServerIncomingMessage -> (Boolean -> SM Unit) -> SM Unit
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
      matchWaiting msg $ \isReady -> do
        -- tracePM $ "updated ready state for " <> show pId <>
        --          ": " <> show isReady
        let m' = M.insert pId isReady m
        put $ WaitingForPlayers m'

onNewPlayer :: PlayerId -> SM Unit
onNewPlayer pId =
  sendUpdateTo pId $ SOConnecting $ YourPlayerIdIs pId

onClose :: PlayerId -> SM Unit
onClose pId = do
  state <- get
  case state of
    InProgress g -> do
      sendUpdate $ SOInProgress [GUPU pId PlayerLeft]
      let g' = g # game_ %~ removePlayer pId
      put $ InProgress g'

    WaitingForPlayers m -> do
      let m' = M.delete pId m
      put $ WaitingForPlayers m'

