module Client where

import Data.Maybe
import Control.Monad.Eff

import Rendering

main = do
  ctx <- setupRendering
  renderBackground ctx
