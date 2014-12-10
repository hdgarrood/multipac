module Rendering where

import Debug.Trace
import Data.Array hiding (map, (..))
import Data.Function
import qualified Data.Map as M
import Data.Tuple
import Data.Maybe
import Data.Foldable
import Graphics.Canvas
  (getContext2D, setCanvasHeight, setCanvasWidth, Rectangle(), Arc(),
  Context2D(), Canvas(), getCanvasElementById, TextAlign(..), LineCap(..))
import Control.Monad.Eff
import Control.Monad (when)
import Control.Monad.Reader.Class (reader)
import Control.Lens ((^.), (..), (~), (^?))
import Math (pi, floor, ceil)

import LevelMap
import Types
import CanvasM
import Utils
import Game
import Style


halfBlock     = floor (pxPerBlock / 2)
halfPxPerTile = floor (pxPerTile / 2)
halfCanvas    = floor (canvasSize / 2)

scaleRect :: Number -> Position -> Rectangle
scaleRect scale (Position p) =
  {x: p.x * scale, y: p.y * scale, h: scale, w: scale}

toPosition :: Rectangle -> Position
toPosition r = Position {x:r.x, y:r.y}

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
    >>= justOrError
    >>= setCanvasHeight canvasSize
    >>= setCanvasWidth canvasSize
    >>= getContext2D

  where
  justOrError (Just x) = return x
  justOrError Nothing = error $ "no canvas element found with id = " <> elId

clearBackground :: forall e. CanvasM e Unit
clearBackground = do
  setFillStyle backgroundColor
  fillRect {x: 0, y: 0, h: canvasSize, w: canvasSize}

data CornerType
  = CRO -- rounded outer
  | CRI -- rounded inner
  | CSH -- straight horizontal
  | CSV -- straight vertical
  | NON -- nothing (inside a block)

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

renderMap :: forall e. LevelMap -> CanvasM e Unit
renderMap map = do
  setStrokeStyle tileColor

  let tileIndices = range 0 (tilesAlongSide - 1)
  let getTile i j = map.tiles !! i >>= (\r -> r !! j)
  let t i j = toBasic <$> getTile i j

  for_ tileIndices $ \i ->
    for_ tileIndices $ \j -> do
      let above      = fromMaybe W $ t i (j-1)
      let aboveRight = fromMaybe W $ t (i+1) (j-1)
      let right      = fromMaybe W $ t (i+1) j
      let belowRight = fromMaybe W $ t (i+1) (j+1)
      let below      = fromMaybe W $ t i (j+1)
      let belowLeft  = fromMaybe W $ t (i-1) (j+1)
      let left       = fromMaybe W $ t (i-1) j
      let aboveLeft  = fromMaybe W $ t (i-1) (j-1)

      case t i j of
        Just W -> do
          let cs = getCorners above aboveRight right belowRight
                              below belowLeft left aboveLeft
          let es = getEdges above right below left
          withContext $ do
            translate { translateX: (i + 0.5) * pxPerTile
                      , translateY: (j + 0.5) * pxPerTile
                      }
            renderCorners cs
            renderEdges es
        _ ->
          return unit

showCorners :: Corners -> String
showCorners cs =
  showRecord "Corners"
    ["tl" .:: cs.tl, "tr" .:: cs.tr, "br" .:: cs.br, "bl" .:: cs.bl]

getCorners above aboveRight right belowRight below belowLeft left aboveLeft =
  { tl: getCorner left above aboveLeft
  , tr: getCorner right above aboveRight
  , br: getCorner right below belowRight
  , bl: getCorner left below belowLeft
  }

-- order is: horizontal vertical corner
getCorner W W E = CRI
getCorner W W W = NON
getCorner W E _ = CSH
getCorner E W _ = CSV
getCorner E E _ = CRO

renderCorners :: forall e.  Corners -> CanvasM e Unit
renderCorners cs = do
  let renderCorner c a t =
    case c of
        CRO ->
          { prep: do
              translate t
              rotate a
          , go:
              arc { start: pi
                  , end:   3*pi/2
                  , x:     cornerMid
                  , y:     cornerMid
                  , r:     cornerRadius
                  }
          }
        CRI ->
          { prep: do
              translate t
              rotate a
          , go:
              arc { start: 0
                  , end:   pi/2
                  , x:     -cornerMid
                  , y:     -cornerMid
                  , r:     cornerRadius
                  }
          }
        CSH ->
          { prep: translate t
          , go: do
              moveTo (-cornerMid -1) 0
              lineTo (cornerMid + 1)  0
          }
        CSV ->
          { prep: translate t
          , go: do
              moveTo 0 (-cornerMid - 1)
              lineTo 0 (cornerMid + 1)
          }
        NON ->
          { prep: return unit
          , go: return unit
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
    withContext $ do
      setLineCap Square
      let r = renderCorner c.c c.a c.t
      r.prep
      beginPath
      r.go
      stroke

getEdges above right below left =
  { t: above, r: right, b: below, l: left }

renderEdges es =
  let s = (pxPerTile / 2) - cornerSize
      x1 = -s - 1
      y1 = -s - (cornerSize / 2)
      x2 = s
      y2 = y1

      renderEdge e =
        when (e == E) $ do
          moveTo x1 y1
          lineTo x2 y2

      es' =
        [ { e: es.t, a: 0 }
        , { e: es.r, a: pi/2 }
        , { e: es.b, a: pi }
        , { e: es.l, a: 3*pi/2 }
        ]

  in
  for_ es' $ \e ->
    withContext $ do
      rotate e.a
      beginPath
      renderEdge e.e
      stroke

renderPlayer :: forall e.
  PlayerId
  -> Player
  -> CanvasM e Unit
renderPlayer pId player = do
  setFillStyle $ playerColor pId
  let r = playerRenderParameters player
  let centre = getCentredRectAt (player ^. pPosition)
  beginPath
  moveTo r.start.x r.start.y
  arc r.arc
  lineTo r.start.x r.start.y
  fill

playerRenderParameters player =
  let centre = getCentredRectAt (player ^. pPosition)
      direction = fromMaybe Left (player ^. pDirection)

      baseAngle = directionToRadians direction
      index = player ^. pNomIndex
      half = floor (nomIndexMax / 2)
      maxAngle = pi / 2
      multiplier = nomIndexMax / maxAngle
      delta = multiplier * (if index <= half
                                then index
                                else (nomIndexMax - index)) + 0.001

      op = dirToPos $ opposite direction
      start = add (scalePos (playerRadius / 2) op) (toPosition centre)
  in
    { arc: { x: centre.x
           , y: centre.y
           , start: (baseAngle + delta)
           , end: (baseAngle - delta)
           , r: playerRadius
           }
    , start: { x: start ^. pX
             , y: start ^. pY
             }
    }


enlargeRect :: Number -> Rectangle -> Rectangle
enlargeRect delta r =
  { x: r.x - delta
  , y: r.y - delta
  , w: r.w + (2 * delta)
  , h: r.h + (2 * delta)
  }

renderItems :: forall e. Game -> CanvasM e Unit
renderItems game =
  eachItem' game renderItem

renderItem :: forall e. Item -> CanvasM e Unit
renderItem item = do
  let centre = getCentredRectAt (item ^. iPosition)
  setFillStyle littleDotColor
  beginPath
  arc { x: centre.x
      , y: centre.y
      , start: 0
      , end: 2 * pi
      , r: littleDotRadius
      }
  fill

renderPlayers :: forall e. Game -> CanvasM e Unit
renderPlayers game =
  eachPlayer' game renderPlayer

clearCanvas :: forall e. CanvasM e Unit
clearCanvas =
  clearRect {x: 0, y: 0, h: canvasSize, w: canvasSize}

renderCountdown :: forall e. Game -> PlayerId -> CanvasM e Unit
renderCountdown game pId = do
  whenJust game.countdown $ \cd -> do
    renderCounter cd
    renderReminderArrow game pId


setFontSize :: forall e. Number -> CanvasM e Unit
setFontSize x =
  setFont $ show x <> "pt " <> fontName


renderCounter cd = do
  setFontSize $ pxPerTile * 3
  setTextAlign AlignCenter
  setLineWidth 3
  setFillStyle fontColor
  setStrokeStyle "black"
  let text = show (ceil (cd / 30))
  let x = halfCanvas
  let y = floor (halfCanvas / 2)
  fillText   text x y
  strokeText text x y


renderReminderArrow game pId = do
  whenJust (game ^? player pId) $ \pl ->
    withContext $ do
      let pos = pl ^. pPosition
      let playerX = pos ^. pX
      let y = pos ^. pY

      let greater = playerX > mapSize / 2
      let d = 1.5 * tileSize
      let x = playerX + (if greater then d else -d)

      let centre = getCentredRectAt (Position {x:x, y:y})

      setLineWidth 1
      beginPath
      arrowPath greater centre.x centre.y 30 10
      fill
      stroke

arrowPath toRight x y length width = do
  let halfLen = floor (length / 2)
  let halfWid = floor (width / 2)
  let f z = if toRight then x + z else x - z

  moveTo (f halfLen)           (y - halfWid)
  lineTo (f (- halfLen))       (y - halfWid)
  lineTo (f (- halfLen))       (y - (2 * halfWid))
  lineTo (f (- (2 * halfLen))) (y)
  lineTo (f (- halfLen))       (y + (2 * halfWid))
  lineTo (f (- halfLen))       (y + halfWid)
  lineTo (f halfLen)           (y + halfWid)
  lineTo (f halfLen)           (y - halfWid)


render :: forall e.
  RenderingContext
  -> Game
  -> PlayerId
  -> Boolean
  -> Eff (canvas :: Canvas | e) Unit
render ctx game pId redrawMap = do
  when redrawMap $ do
    runCanvasM ctx.background $
      renderMap game.map

  runCanvasM ctx.foreground $ do
    clearCanvas
    renderItems game
    renderPlayers game
    renderCountdown game pId

clearBackgroundWaiting :: forall e.
  RenderingContext
  -> Eff (canvas :: Canvas | e) Unit
clearBackgroundWaiting ctx =
  runCanvasM ctx.background clearCanvas

setTextStyle = do
  setFontSize $ floor (0.6 * pxPerTile)
  setFillStyle fontColor

renderWaitingMessage :: forall e.  Boolean -> CanvasM e Unit
renderWaitingMessage ready = do
  setTextStyle
  setTextAlign AlignCenter
  let message =
      if ready
         then "Waiting for other players..." ~ "ready: ✓"
         else "Press SPACE when you're ready" ~ "ready: ✕"
  let x = 20
  fillText (fst message) halfCanvas (halfCanvas - x)
  fillText (snd message) halfCanvas (halfCanvas + x)

renderYourPlayer :: forall e. PlayerId -> CanvasM e Unit
renderYourPlayer pId = do
  setTextStyle
  setTextAlign AlignRight
  let message = "You are: " <> show pId
  let x = halfCanvas + 40
  let y = halfCanvas + 100
  let d = 10
  fillText message (x-d) y
  renderPlayer pId (mkPlayer $ Position { x:120, y:122 })

