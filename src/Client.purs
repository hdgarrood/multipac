module Client where

import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Reactive.Timer

import Rendering
import Game
import Types
import Utils

main = do
  ctx <- setupRendering
  game <- newRef initialGame

  startAnimationLoop $ do
    g <- readRef game
    let result = stepGame (Input (Just Down)) g
    let g' = fst result
    renderGame ctx g'
    writeRef game g'

foreign import startAnimationLoop
  """
  function startAnimationLoop(action) {
    return function go() {
      window.requestAnimationFrame(go);
      action();
    }
  }
  """ :: forall a e. Eff e a -> Eff e Unit
