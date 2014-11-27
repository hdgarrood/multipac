module Rendering where

import Debug.Trace
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

debug = false

pxPerBlock :: Number
pxPerBlock = 3

halfBlock :: Number
halfBlock = floor (pxPerBlock / 2)

pxPerTile :: Number
pxPerTile = pxPerBlock * tileSize

halfPxPerTile :: Number
halfPxPerTile = floor (pxPerTile / 2)

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

foreign import renderMapDebugFFI
  """
  function renderMapDebugFFI(isEmpty, getBlockRect, getTileRect, ctx, map) {
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

renderMapDebug :: forall e.
  Context2D
  -> LevelMap
  -> Eff (canvas :: Canvas | e) Unit
renderMapDebug ctx map = do
  clearBackground ctx
  runFn5 renderMapDebugFFI (not .. isWall) getRectAt' getTileRectAt' ctx map

data CornerType
  = CRO -- rounded outer
  | CRI -- rounded inner
  | CSH -- straight horizontal
  | CSV -- straight vertical

instance showCornerType :: Show CornerType where
  show CRO = "CRO"
  show CRI = "CRI"
  show CSH = "CSH"
  show CSV = "CSV"

type Corners =
  { tl :: CornerType, tr :: CornerType, bl :: CornerType, br :: CornerType }

toBasic :: Tile -> BasicTile
toBasic Inaccessible = W
toBasic _ = E

renderMap :: forall e.
  Context2D
  -> LevelMap
  -> Eff (trace :: Trace, canvas :: Canvas | e) Unit
renderMap ctx map = do
  setStrokeStyle "hsl(200, 80%, 40%)" ctx
  strokeRect ctx {x:0, y:0, w: canvasSize, h: canvasSize}

  let tileIndices = range 0 (tilesAlongSide - 1)
  let getTile i j = map.tiles !! i >>= (\r -> r !! j)
  let t i j = toBasic <$> getTile i j

  for_ tileIndices $ \i ->
    for_ tileIndices $ \j -> do
      let above = fromMaybe W $ t i (j-1)
      let below = fromMaybe W $ t i (j+1)
      let right = fromMaybe W $ t (i+1) j
      let left  = fromMaybe W $ t (i-1) j

      case t i j of
        Just W -> do
          let cs = getCorners above right below left
          let es = getEdges above right below left
          withContext ctx $ do
            translate {translateX: (i + 0.5) * pxPerTile
                      , translateY: (j + 0.5) * pxPerTile} ctx
            renderCorners ctx cs
            renderEdges ctx es
        _ ->
          return unit

showCorners :: Corners -> String
showCorners cs =
  showRecord "Corners"
    ["tl" .:: cs.tl, "tr" .:: cs.tr, "br" .:: cs.br, "bl" .:: cs.bl]

getCorners :: BasicTile -> BasicTile -> BasicTile -> BasicTile -> Corners
getCorners above right below left =
  { tl: getCorner left above
  , tr: getCorner right above
  , br: getCorner right below
  , bl: getCorner left below
  }

-- order is: horizontal vertical
getCorner :: BasicTile -> BasicTile -> CornerType
getCorner W W = CRI
getCorner W E = CSH
getCorner E W = CSV
getCorner E E = CRO

cornerSize = 9
cornerMid = floor (cornerSize / 2)
cornerRadius = cornerMid

renderCorners :: forall e.
  Context2D
  -> Corners
  -> Eff (trace :: Trace, canvas :: Canvas | e) Unit
renderCorners ctx cs = do
  let renderCorner ctx c a t =
    case c of
        CRO ->
          { prep: do
              translate t ctx
              rotate a ctx
          , go:
              arc ctx
                { start: pi
                , end:   3*pi/2
                , x:     cornerMid
                , y:     cornerMid
                , r:     cornerRadius
                }
          }  
        CRI ->
          { prep: do
              translate t ctx
              rotate a ctx
          , go:
              arc ctx
                { start: 0
                , end:   pi/2
                , x:     -cornerMid
                , y:     -cornerMid
                , r:     cornerRadius
                }
          }  
        CSH ->
          { prep: translate t ctx
          , go: do
              moveTo ctx (-cornerMid - 1) 0
              lineTo ctx (cornerMid + 1) 0
          }
        CSV ->
          { prep: translate t ctx
          , go: do
              moveTo ctx 0 (-cornerMid - 1)
              lineTo ctx 0 (cornerMid + 1)
          }

  let s = (pxPerTile - cornerSize) / 2
  let cs' =
      [ { c: cs.tl
        , a: 0
        , t: {translateX: -s, translateY: -s}
        },

        { c: cs.tr
        , a: pi/2
        , t: {translateX: s, translateY: -s}
        },

        { c: cs.br
        , a: pi
        , t: {translateX: s, translateY: s}
        },

        { c: cs.bl
        , a: 3*pi/2
        , t: {translateX: -s, translateY: s}
        }
        ]

  for_ cs' $ \c -> do
    withContext ctx $ do
      let r = renderCorner ctx c.c c.a c.t
      r.prep
      beginPath ctx
      r.go
      stroke ctx


getEdges above right below left =
  { t: above, r: right, b: below, l: left }


renderEdges ctx es =
  let s = (pxPerTile / 2) - cornerSize
      x1 = -s - 1
      y1 = -s - (cornerSize / 2)
      x2 = s
      y2 = y1

      renderEdge ctx e =
        case e of
          W -> return unit
          E -> do
            moveTo ctx x1 y1
            void $ lineTo ctx x2 y2

      es' =
        [ { e: es.t, a: 0 }
        , { e: es.r, a: pi/2 }
        , { e: es.b, a: pi }
        , { e: es.l, a: 3*pi/2 }
        ]

  in
  for_ es' $ \e ->
    withContext ctx $ do
      rotate e.a ctx
      beginPath ctx
      renderEdge ctx e.e
      stroke ctx
         

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
    arc ctx {x: centre.x, y: centre.y, start: 0, end: 2 * pi, r: 13}
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
  -> Eff (trace :: Trace, canvas :: Canvas | e) Unit
render ctx state = do
  when state.redrawMap $ do
    when debug $ renderMapDebug ctx.background state.game.map
    renderMap ctx.background state.game.map
  clearPreviousPlayers ctx.foreground state.prevGame
  renderPlayers ctx.foreground state.game
