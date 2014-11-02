module LevelMap where

import Types
import Math (floor)
import Data.Array (map, reverse)
import Utils

-- A fixed size two-dimensional array of blocks.
type Tile = [[Block]]

rotateCW :: Tile -> Tile
rotateCW = map reverse . transpose

concatTiles :: [[Tile]] -> [[Block]]
concatTiles = 

-- The number of blocks along one side of a tile in the level map. This allows
-- a simpler model, since any given object may exist in only one block.
-- Should be an odd number, since any tile should have one central block.
tileSize :: Number
tileSize = 15

halfTile :: Number
halfTile = floor (tileSize / 2)

-- the number of tiles along one side of a level map.
tilesAlongSide :: Number
tilesAlongSide = 20

-- the height or width in a level, in blocks.
mapSize :: Number
mapSize = tileSize * tilesAlongSide

mirror :: a -> a -> [[a]]
mirror x y = replicate halfTile x <> [y] <> replicate halfTile x

intersection :: Tile
intersection =
    let normalRow = mirror Wall Empty
        centralRow = replicate tileSize Empty
    in mirror normalRow centralRow

teeJunctionUp :: Tile
teeJunctionUp =
    let upperRow   = mirror Wall Empty
        centralRow = replicate tileSize Empty
        lowerRow   = replicate tileSize Wall
    in replicate halfTile upperRow <>
        [centralRow] <>
        replicate halfTile lowerRow

teeJunctionRight :: Tile
teeJunctionRight = rotateCW teeJunctionUp
teeJunctionDown :: Tile
teeJunctionDown  = rotateCW teeJunctionRight
teeJunctionLeft :: Tile
teeJunctionLeft  = rotateCW teeJunctionDown

uprightCorner :: Tile
uprightCorner =
    let normalRow = mirror Wall Empty
        centralRow = replicate halfTile Wall <> replicate (halfTile + 1) Empty
    in mirror normalRow centralRow

downrightCorner :: Tile
downrightCorner = rotateCW uprightCorner
downleftCorner :: Tile
downleftCorner = rotateCW downrightCorner
upleftCorner :: Tile
upleftCorner = rotateCW downleftCorner

basicMap :: [[Tile]]
basicMap =
    let n = tilesAlongSide - 2
        topRow =
            [downrightCorner] <>
                replicate n teeJunctionDown <>
                [downleftCorner]
        centralRow =
            [teeJunctionRight] <>
                replicate n intersection <>
                [teeJunctionLeft]
        bottomRow =
            [uprightCorner] <>
                replicate n teeJunctionUp <>
                [upleftCorner]
    in [topRow] <> replicate n centralRow <> [bottomRow]
