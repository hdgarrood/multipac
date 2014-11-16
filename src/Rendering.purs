module Rendering where

import Data.Array
import Data.Maybe
import Data.Foldable
import Graphics.Canvas
import Control.Monad.Eff
import Control.Monad (when)
import Control.Lens ((^.))

import ExtraDom
import LevelMap
import Types
import Utils

pxPerBlock :: Number
pxPerBlock = 3

getRectAt ::
  Position -> { x :: Number, y :: Number, w :: Number, h :: Number }
getRectAt (Position p) =
  {x: p.x * pxPerBlock, y: p.y * pxPerBlock, h: pxPerBlock, w: pxPerBlock}

-- the height and width of the canvas
canvasSize :: Number
canvasSize = pxPerBlock * LevelMap.mapSize

-- set up the canvas and return a canvas context
setupRendering :: forall e. Eff (canvas :: Canvas | e) Context2D
setupRendering =
  getCanvasElementById "canvas"
    >>= setCanvasHeight canvasSize
    >>= setCanvasWidth canvasSize
    >>= getContext2D

renderBackground :: forall e.
  Context2D -> Eff (canvas :: Canvas | e) Context2D
renderBackground ctx =
  let r = {x: 0, y: 0, h: canvasSize, w: canvasSize}
  in fillRect ctx r

-- render a row at the given y-coordinate on the canvas
renderRow :: forall e.
  Context2D
  -> [Block]
  -> Number
  -> Eff (canvas :: Canvas | e) Unit
renderRow ctx blocks y =
  eachWithIndex_ blocks $ \block n ->
    when (not (isWall block)) $ void (fillRect ctx (getRect n))
  where
  getRect n = getRectAt (Position {x: n, y: y})

renderMap :: forall e.
  Context2D
  -> LevelMap
  -> Eff (canvas :: Canvas | e) Unit
renderMap ctx map =
  withContext ctx go
  where
  go = do
    setFillStyle "gray" ctx
    eachWithIndex_ map.blocks $ \row n -> do
      renderRow ctx row n

renderPlayer :: forall e.
  Context2D
  -> Player
  -> Eff (canvas :: Canvas | e) Unit
renderPlayer ctx player =
  void $ withContext ctx $ do
    setFillStyle "yellow" ctx
    fillRect ctx (getRectAt (player ^. position))

renderGame :: forall e.
  Context2D
  -> Game
  -> Eff (canvas :: Canvas | e) Unit
renderGame ctx game = do
  renderMap ctx game.map
  renderPlayer ctx $ game ^. player
