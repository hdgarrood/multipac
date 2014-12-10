module Style where

import Math (floor)
import LevelMap (mapSize, tileSize)
import Types (PlayerId(..))

fontName = "Raleway"
fontUrl = "https://fonts.googleapis.com/css?family=" <> fontName

backgroundColor = "#34344e"
fontColor = "#dfd1a5"
tileColor = "hsl(200, 80%, 40%)"
littleDotColor = "#eecccc"

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

-- parameters for corners of tiles
cornerSize = floor (pxPerTile / 3)
cornerMid = floor (cornerSize / 2)
cornerRadius = cornerMid

