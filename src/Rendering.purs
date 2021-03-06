module Rendering where

import CanvasM
import Data.Array hiding ((..))
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Tuple
import Effect
import Game
import LevelMap
import Prelude
import Style
import Types
import Utils

import Control.Monad (when)
import Data.Int as Int
import Data.Lens.At (at)
import Data.Lens.Getter ((^.))
import Data.Lens.Prism.Maybe (_Just)
import Data.Map as M
import Data.Profunctor.Strong ((***))
import Data.Tuple.Nested ((/\))
import Effect.Exception (throw)
import Graphics.Canvas (Arc, Context2D, LineCap(..), TextAlign(..), Rectangle, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth)
import Math (pi, floor, ceil)


halfBlock     = floor (pxPerBlock / 2.0)
halfPxPerTile = floor (pxPerTile / 2.0)
halfCanvas    = floor (canvasSize / 2.0)

scaleRect :: Number -> Position -> Rectangle
scaleRect scale (Position p) =
  {x: p.x * scale, y: p.y * scale, height: scale, width: scale}

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

setupRendering :: Effect RenderingContext
setupRendering = do
  fg <- setupRenderingById "foreground"
  bg <- setupRenderingById "background"
  pure { foreground: fg, background: bg }

-- set up a canvas with the correct dimensions and return its context
setupRenderingById :: String -> Effect Context2D
setupRenderingById elId = do
  el <- getCanvasElementById elId >>= justOrErr
  setCanvasHeight el canvasSize
  setCanvasWidth el canvasSize
  getContext2D el

  where
  justOrErr :: forall a. Maybe a -> Effect a
  justOrErr (Just x) = pure x
  justOrErr Nothing = throw $ "no canvas element found with id = " <> elId

clearBackground :: forall e. CanvasM e Unit
clearBackground = do
  setFillStyle backgroundColor
  fillRect wholeCanvas

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
  show NON = "NON"

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
            translate { translateX: (Int.toNumber i + 0.5) * pxPerTile
                      , translateY: (Int.toNumber j + 0.5) * pxPerTile
                      }
            renderCorners cs
            renderEdges es
        _ ->
          pure unit

showCorners :: Corners -> String
showCorners cs =
  showRecord "Corners"
    ["tl" `assoc` cs.tl, "tr" `assoc` cs.tr, "br" `assoc` cs.br, "bl" `assoc` cs.bl]
  where
  assoc k v = k <> ": " <> show v

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
  let renderCorner c a t = case c of
        CRO ->
          { prep: do
              translate t
              rotate a
          , go:
              arc { start:  pi
                  , end:    1.5 * pi
                  , x:      cornerMid
                  , y:      cornerMid
                  , radius: cornerRadius
                  }
          }
        CRI ->
          { prep: do
              translate t
              rotate a
          , go:
              arc { start:  0.0
                  , end:    pi/2.0
                  , x:      -cornerMid
                  , y:      -cornerMid
                  , radius: cornerRadius
                  }
          }
        CSH ->
          { prep: translate t
          , go: do
              moveTo (-cornerMid -1.0) 0.0
              lineTo (cornerMid + 1.0) 0.0
          }
        CSV ->
          { prep: translate t
          , go: do
              moveTo 0.0 (-cornerMid - 1.0)
              lineTo 0.0 (cornerMid + 1.0)
          }
        NON ->
          { prep: pure unit
          , go: pure unit
          }

  let s = (pxPerTile - cornerSize) / 2.0
  let cs' =
        [ { c: cs.tl
          , a: 0.0
          , t: {translateX: -s, translateY: -s}
          },

          { c: cs.tr
          , a: pi/2.0
          , t: {translateX: s, translateY: -s}
          },

          { c: cs.br
          , a: pi
          , t: {translateX: s, translateY: s}
          },

          { c: cs.bl
          , a: 1.5*pi
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
  let s = (pxPerTile / 2.0) - cornerSize
      x1 = -s - 1.0
      y1 = -s - (cornerSize / 2.0)
      x2 = s
      y2 = y1

      renderEdge e =
        when (e == E) $ do
          moveTo x1 y1
          lineTo x2 y2

      es' =
        [ { e: es.t, a: 0.0 }
        , { e: es.r, a: pi/2.0 }
        , { e: es.b, a: pi }
        , { e: es.l, a: 1.5*pi }
        ]

  in
  for_ es' $ \e ->
    withContext $ do
      rotate e.a
      beginPath
      renderEdge e.e
      stroke

renderPlayer :: forall e.
  (PlayerId -> Boolean)
  -> (PlayerId -> Boolean)
  -> PlayerId
  -> Player
  -> CanvasM e Unit
renderPlayer isRampaging isFleeing pId player = do
  setFillStyle $ if isFleeing pId then fleeingFlashColor else playerColor pId
  let r = playerRenderParameters player
  let centre = getCentredRectAt (player ^. pPosition)
  beginPath
  moveTo r.start.x r.start.y
  arc r.arc
  lineTo r.start.x r.start.y
  fill

  when (isRampaging pId) $ do
    setLineWidth 2.0
    setStrokeStyle rampagePlayerColor
    stroke

playerRenderParameters player =
  let centre = getCentredRectAt (player ^. pPosition)
      direction = fromMaybe Left (player ^. pDirection)

      baseAngle = directionToRadians direction
      index = player ^. pNomIndex
      half = nomIndexMax / 2
      maxAngle = pi / 2.0
      halfAngle = maxAngle / 2.0
      multiplier = Int.toNumber nomIndexMax / maxAngle
      delta =
        case player ^. pRespawnCounter of
          Just ctr ->
            let
              ratio = Int.toNumber ctr / Int.toNumber respawnLength
            in 
              halfAngle + ((1.0 - ratio) * (pi - halfAngle))
          Nothing ->
            let
              x = if index <= half
                    then index
                    else nomIndexMax - index
            in
              multiplier * Int.toNumber x + 0.001


      op = dirToPos $ opposite direction
      start = addPos (scalePos (playerRadius / 2.0) op) (toPosition centre)
  in
    { arc: { x: centre.x
           , y: centre.y
           , start: (baseAngle + delta)
           , end: (baseAngle - delta)
           , radius: playerRadius
           }
    , start: { x: start ^. pX
             , y: start ^. pY
             }
    }


enlargeRect :: Number -> Rectangle -> Rectangle
enlargeRect delta r =
  { x: r.x - delta
  , y: r.y - delta
  , width: r.width + (2.0 * delta)
  , height: r.height + (2.0 * delta)
  }

renderItems :: forall e. Game -> CanvasM e Unit
renderItems game = do
  setFillStyle dotColor
  let rampaging = isJust game.rampage
  eachItem' game (renderItem rampaging)

renderItem :: forall e. Boolean -> Item -> CanvasM e Unit
renderItem rampaging item = do
  let centre = getCentredRectAt (item ^. iPosition)
  when (not (item ^. iType == BigDot && rampaging)) $ do
    beginPath
    arc { x: centre.x
        , y: centre.y
        , start: 0.0
        , end: 2.0 * pi
        , radius: dotRadiusFor (item ^. iType)
        }
    fill

renderPlayers :: forall e. Game -> CanvasM e Unit
renderPlayers game = do
  let isRampaging =
        maybe (const false)
              (\r -> case r of
                  Rampaging pId _ -> (==) pId
                  _ -> const false)
              game.rampage
  let isFleeing =
        maybe (const false)
              (\r -> case r of
                  Rampaging pId ctr ->
                    \pId' -> pId /= pId' && elem ctr flashCounts
                  _ ->
                    const false)
              game.rampage

  eachPlayer' game (renderPlayer isRampaging isFleeing)

  where
  flashCounts = concatMap (uncurry range) flashes

  initial = rampageLength /\ (rampageLength - step)
  f x = x - (2 * step)
  step = 8
  flashes = iterateN 5 initial (f *** f)

wholeCanvas :: Rectangle
wholeCanvas =
  {x: 0.0, y: 0.0, height: canvasSize, width: canvasSize}

clearCanvas :: forall e. CanvasM e Unit
clearCanvas =
  clearRect wholeCanvas

renderCountdown :: forall e. Game -> PlayerId -> CanvasM e Unit
renderCountdown game pId = do
  whenJust game.countdown $ \cd -> do
    renderCounter cd
    renderReminderArrow game pId


setFontSize :: forall e. Number -> CanvasM e Unit
setFontSize x =
  setFont $ show x <> "pt " <> fontName


renderCounter cd = do
  setFontSize (pxPerTile * 3.0)
  setTextAlign AlignCenter
  setLineWidth 3.0
  setFillStyle fontColor
  setStrokeStyle "black"
  let text = show (cd / 30)
  let x = halfCanvas
  let y = halfCanvas
  fillText   text x y
  strokeText text x y


renderReminderArrow :: forall e. Game -> PlayerId -> CanvasM e Unit
renderReminderArrow game pId = do
  whenJust (game ^. (players <<< at pId)) $ \pl ->
    withContext $ do
      let pos = pl ^. pPosition
      let playerX = pos ^. pX
      let y = pos ^. pY

      let greater = playerX > Int.toNumber (mapSize / 2)
      let d = 1.5 * Int.toNumber tileSize
      let x = playerX + (if greater then d else -d)

      let centre = getCentredRectAt (Position {x:x, y:y})

      setLineWidth 1.0
      beginPath
      arrowPath greater centre.x centre.y 30 10
      fill
      stroke

arrowPath toRight x y length width = do
  let halfLen = Int.toNumber (length / 2)
  let halfWid = Int.toNumber (width / 2)
  let f z = if toRight then x + z else x - z

  moveTo (f halfLen)             (y - halfWid)
  lineTo (f (- halfLen))         (y - halfWid)
  lineTo (f (- halfLen))         (y - (2.0 * halfWid))
  lineTo (f (- (2.0 * halfLen))) (y)
  lineTo (f (- halfLen))         (y + (2.0 * halfWid))
  lineTo (f (- halfLen))         (y + halfWid)
  lineTo (f halfLen)             (y + halfWid)
  lineTo (f halfLen)             (y - halfWid)


render :: forall e.
  RenderingContext
  -> Game
  -> PlayerId
  -> Boolean
  -> Effect Unit
render ctx game pId redrawMap = do
  when redrawMap $ do
    runCanvasM ctx.background $
      renderMap (unwrapLevelMap game.map)

  runCanvasM ctx.foreground $ do
    clearCanvas
    renderItems game
    renderPlayers game
    renderCountdown game pId

clearBoth :: forall e.
  RenderingContext
  -> Effect Unit
clearBoth ctx = do
  runCanvasM ctx.background clearCanvas
  runCanvasM ctx.foreground clearCanvas

setTextStyle = do
  setFontSize $ floor (0.6 * pxPerTile)
  setFillStyle fontColor
