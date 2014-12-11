module Style where

import Math (floor)
import LevelMap (mapSize, tileSize)
import Types

fontName = "Exo"
fontUrl = "https://fonts.googleapis.com/css?family=Exo:700"

backgroundColor        = "hsl(240, 20%, 25%)"
backgroundColorLighter = "hsl(240, 20%, 32%)"
fontColor              = "#dfd1a5"
tileColor              = "hsl(200, 80%, 40%)"
dotColor               = "#eecccc"

playerColor :: PlayerId -> String
playerColor pId =
  case pId of
     P1 -> "hsl(0, 100%, 60%)"
     P2 -> "hsl(90, 100%, 60%)"
     P3 -> "hsl(180, 100%, 60%)"
     P4 -> "hsl(270, 100%, 60%)"


pxPerTile = 35
pxPerBlock = pxPerTile / tileSize
canvasSize = pxPerBlock * mapSize

playerRadius = floor (pxPerTile / 2.2)
littleDotRadius = pxPerBlock
bigDotRadius = littleDotRadius * 3

dotRadiusFor LittleDot = littleDotRadius
dotRadiusFor BigDot = bigDotRadius

-- parameters for corners of tiles
cornerSize = floor (pxPerTile / 3)
cornerMid = floor (cornerSize / 2)
cornerRadius = cornerMid

