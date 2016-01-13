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

pxPerTile = 35
pxPerBlock = pxPerTile / tileSize
canvasSize = pxPerBlock * mapSize

playerRadius = floor (Int.toNumber pxPerTile / 2.2)
littleDotRadius = pxPerBlock
bigDotRadius = littleDotRadius * 3

dotRadiusFor LittleDot = littleDotRadius
dotRadiusFor BigDot = bigDotRadius

-- parameters for corners of tiles
cornerSize = Int.toNumber (pxPerTile / 3)
cornerMid = floor (cornerSize / 2.0)
cornerRadius = cornerMid

