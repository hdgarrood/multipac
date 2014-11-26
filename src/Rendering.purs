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
import Math (pi, floor)

import LevelMap
import Types
import Utils

pxPerBlock :: Number
pxPerBlock = 3

halfBlock :: Number
halfBlock = floor (pxPerBlock / 2)

pxPerTile :: Number
pxPerTile = pxPerBlock * tileSize

scaleRect :: Number -> Position -> Rectangle
scaleRect scale (Position p) =
  {x: p.x * scale, y: p.y * scale, h: scale, w: scale}

getRectAt :: Position -> Rectangle
getRectAt = scaleRect pxPerBlock

getCentredRectAt :: Position -> Rectangle
getCentredRectAt p =
  let r = getRectAt p
  in  r {x = r.x + halfBlock, y = r.y + halfBlock}

getRectAt' :: Number -> Number -> Rectangle
getRectAt' x y = getRectAt (Position {x: x, y: y})

getTileRectAt :: Position -> Rectangle
getTileRectAt = scaleRect pxPerTile

getTileRectAt' :: Number -> Number -> Rectangle
getTileRectAt' x y = getTileRectAt (Position {x: x, y: y})

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
  setFillStyle "hsl(200, 10%, 75%)" ctx
  void $ fillRect ctx {x: 0, y: 0, h: canvasSize, w: canvasSize}

foreign import renderMapFFI
  """
  function renderMapFFI(isEmpty, getBlockRect, getTileRect, ctx, map) {
    return function() {
      ctx.strokeStyle = 'hsl(40, 70%, 50%)'
      var t = map.tiles
      for (var i = 0; i < t.length; i++) {
        for (var j = 0; j < t[i].length; j++) {
          var r = getTileRect(i)(j)
          ctx.strokeRect(r.x, r.y, r.w, r.h)
        }
      }

      ctx.fillStyle = 'hsl(320, 20%, 10%)'
      var b = map.blocks
      for (var i = 0; i < b.length; i++) {
        for (var j = 0; j < b[i].length; j++) {
          if (isEmpty(b[i][j])) {
            var r = getBlockRect(i)(j)
            ctx.fillRect(r.x, r.y, r.w, r.h)
          }
        }
      }
    }
  }
  """ :: forall e.
  Fn5
    (Block -> Boolean)
    (Number -> Number -> Rectangle)
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
  runFn5 renderMapFFI (not .. isWall) getRectAt' getTileRectAt' ctx map

renderPlayer :: forall e.
  Context2D
  -> PlayerId
  -> Player
  -> Eff (canvas :: Canvas | e) Unit
renderPlayer ctx pId player =
  void $ do
    setFillStyle (fillStyleFor pId) ctx
    let centre = getCentredRectAt (player ^. position)
    beginPath ctx
    arc ctx {x: centre.x, y: centre.y, start: 0, end: 2 * pi, r: 9}
    fill ctx

clearPlayer :: forall e.
  Context2D
  -> PlayerId
  -> Player
  -> Eff (canvas :: Canvas | e) Unit
clearPlayer ctx pId player =
  void $ clearRect ctx (enlargeRect 5 $ getRectAt (player ^. position))

enlargeRect :: Number -> Rectangle -> Rectangle
enlargeRect delta r =
  { x: r.x - delta
  , y: r.y - delta
  , w: r.w + (2 * delta)
  , h: r.h + (2 * delta)
  }

fillStyleFor :: PlayerId -> String
fillStyleFor P1 = "hsl(0, 100%, 60%)"
fillStyleFor P2 = "hsl(90, 100%, 60%)"
fillStyleFor P3 = "hsl(180, 100%, 60%)"
fillStyleFor P4 = "hsl(270, 100%, 60%)"

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
clearPreviousPlayers ctx game =
  void $ clearRect ctx {x: 0, y: 0, h: canvasSize, w: canvasSize}

render :: forall e.
  RenderingContext
  -> ClientState
  -> Eff (canvas :: Canvas | e) Unit
render ctx state = do
  when (state.redrawMap) $
    renderMap ctx.background state.game.map
  clearPreviousPlayers ctx.foreground state.prevGame
  renderPlayers ctx.foreground state.game
