module Style where

import Prelude
import Math (floor)
import Data.Int as Int
import LevelMap (mapSize, tileSize)
import Types

fontName = "Exo"
fontUrl = "https://fonts.googleapis.com/css?family=Exo:700"

backgroundColor        = "hsl(240, 20%, 25%)"
backgroundColorLighter = "hsl(240, 20%, 32%)"
fontColor              = "#dfd1a5"
tileColor              = "hsl(200, 80%, 40%)"
dotColor               = "#eecccc"
rampagePlayerColor     = "hsl(55, 90%, 60%)"
fleeingFlashColor      = "hsl(160, 100%, 60%)"
linkColor              = playerColor P3

playerColor :: PlayerId -> String
playerColor pId =
  let hue =
        case pId of
           P1 -> 0
           P2 -> 90
           P3 -> 180
           P4 -> 270
  in "hsl(" <> show hue <> ", 100%, 60%)"

pxPerTile = 35.0
pxPerBlock = pxPerTile / Int.toNumber tileSize
canvasSize = pxPerBlock * Int.toNumber mapSize

playerRadius = floor (pxPerTile / 2.2)
littleDotRadius = pxPerBlock
bigDotRadius = littleDotRadius * 3.0

dotRadiusFor LittleDot = littleDotRadius
dotRadiusFor BigDot = bigDotRadius

-- parameters for corners of tiles
cornerSize = floor (pxPerTile / 3.0)
cornerMid = floor (cornerSize / 2.0)
cornerRadius = cornerMid

