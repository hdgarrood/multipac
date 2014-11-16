module Client where

import Data.Maybe
import Control.Monad.Eff

import Rendering
import Game

main = do
  ctx <- setupRendering
  renderBackground ctx
  renderGame ctx initialGame
