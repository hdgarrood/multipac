module Client where

import Debug.Trace
import Data.Maybe
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer

import qualified BrowserWebSocket as WS
import Rendering
import Game
import Types hiding ((.:))
import Utils

main = do
  ctx <- setupRendering
  game <- newRef initialGame

  socket <- WS.mkWebSocket "ws://localhost:8080/"
  WS.onMessage socket $ \msg -> do
    trace $ "received message: " <> msg
    case eitherDecode msg of
      E.Left err -> trace $ "failed to parse message from server: " <> err
      E.Right update -> modifyRef game (applyGameUpdate update)

  startAnimationLoop (readRef game >>= renderGame ctx)


foreign import startAnimationLoop
  """
  function startAnimationLoop(action) {
    return function go() {
      window.requestAnimationFrame(go);
      action();
    }
  }
  """ :: forall a e. Eff e a -> Eff e Unit
