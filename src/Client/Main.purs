module Client where

import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Data.DOM.Simple.Document
import Data.DOM.Simple.Window
import Data.Maybe
import Control.Monad.Eff

import Rendering
import ExtraDom

main = do
  mapElem <- renderMap LevelMap.basicMap
  doc <- document globalWindow
  Just container <- getElementById "multipac" doc
  appendChild container mapElem
