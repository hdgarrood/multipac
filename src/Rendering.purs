module Rendering where

import Data.Array hiding (map, (..))
import Data.Function
import qualified Data.Map as M
import Data.Tuple
import Data.Maybe
import Data.Foldable
import Graphics.Canvas
import Control.Monad.Eff
import Control.Monad (when)
import Control.Lens ((^.), (..))

import LevelMap
import Types
import Utils

pxPerBlock :: Number
pxPerBlock = 4

getRectAt ::
  Position -> Rectangle
getRectAt (Position p) =
  {x: p.x * pxPerBlock, y: p.y * pxPerBlock, h: pxPerBlock, w: pxPerBlock}

getRectAt' ::
  Number -> Number -> Rectangle
getRectAt' x y = getRectAt (Position {x: x, y: y})

-- the height and width of the canvas
canvasSize :: Number
canvasSize = pxPerBlock * LevelMap.mapSize

setupRendering :: forall e. Eff (canvas :: Canvas | e) RenderingContext
setupRendering = do
  fg <- setupRenderingById "foreground"
  bg <- setupRenderingById "background"
  return { foreground: fg, background: bg }

-- set up a canvas with the correct dimensions and return its context
setupRenderingById :: forall e.
  String -> Eff (canvas :: Canvas | e) Context2D
setupRenderingById elId =
  getCanvasElementById elId
    >>= setCanvasHeight canvasSize
    >>= setCanvasWidth canvasSize
    >>= getContext2D

clearBackground :: forall e.
  Context2D -> Eff (canvas :: Canvas | e) Unit
clearBackground ctx = do
  setFillStyle "black" ctx
  void $ fillRect ctx {x: 0, y: 0, h: canvasSize, w: canvasSize}

foreign import renderMapFFI
  """
  function renderMapFFI(isEmpty, getRect, ctx, map) {
    return function() {
      ctx.fillStyle = 'grey'
      var b = map.blocks
      for (var i = 0; i < b.length; i++) {
        for (var j = 0; j < b[i].length; j++) {
          if (isEmpty(b[i][j])) {
            var r = getRect(i)(j)
            ctx.fillRect(r.x, r.y, r.w, r.h)
          }
        }
      }
    }
  }
  """ :: forall e.
  Fn4
    (Block -> Boolean)
    (Number -> Number -> Rectangle)
    Context2D
    LevelMap
    (Eff (canvas :: Canvas | e) Unit)

renderMap :: forall e.
  Context2D
  -> LevelMap
  -> Eff (canvas :: Canvas | e) Unit
renderMap ctx map = do
  clearBackground ctx
  runFn4 renderMapFFI (not .. isWall) getRectAt' ctx map

renderPlayer :: forall e.
  Context2D
  -> PlayerId
  -> Player
  -> Eff (canvas :: Canvas | e) Unit
renderPlayer ctx pId player =
  void $ do
    setFillStyle (fillStyleFor pId) ctx
    fillRect ctx (getRectAt (player ^. position))

clearPlayer :: forall e.
  Context2D
  -> PlayerId
  -> Player
  -> Eff (canvas :: Canvas | e) Unit
clearPlayer ctx pId player =
  void $ clearRect ctx (expand $ getRectAt (player ^. position))
  where
  -- just to mop up any bits that accidentally don't get cleared
  delta = 5
  expand r = { x: r.x - delta, y: r.y - delta, w: r.w + delta, h: r.h + delta }


fillStyleFor :: PlayerId -> String
fillStyleFor P1 = "yellow"
fillStyleFor P2 = "red"
fillStyleFor P3 = "green"
fillStyleFor P4 = "blue"

renderPlayers :: forall e.
  Context2D
  -> Game
  -> Eff (canvas :: Canvas | e) Unit
renderPlayers ctx game = do
  eachPlayer' game $ renderPlayer ctx

clearPreviousPlayers :: forall e.
  Context2D
  -> Game
  -> Eff (canvas :: Canvas | e) Unit
clearPreviousPlayers ctx game = do
  eachPlayer' game $ clearPlayer ctx

render :: forall e.
  RenderingContext
  -> ClientState
  -> Eff (canvas :: Canvas | e) Unit
render ctx state = do
  when (state.redrawMap) $
    renderMap ctx.background state.game.map
  clearPreviousPlayers ctx.foreground state.prevGame
  renderPlayers ctx.foreground state.game
