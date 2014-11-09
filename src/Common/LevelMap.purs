module LevelMap where

import Types
import Math (floor)
import Data.Array (map, reverse, (!!), (..), concatMap, concat)
import Data.Maybe
import Data.Traversable (sequence)
import Data.Foldable (mconcat)
import Utils

-- A fixed size two-dimensional array of blocks.
type Tile = [[Block]]

rotateCW :: Tile -> Tile
rotateCW = transpose >>> map reverse

getRow :: Number -> Tile -> Maybe [Block]
getRow n t = t !! n

concatTiles :: [[Tile]] -> Maybe [[Block]]
concatTiles = map concatTileRow >>> sequence >>> fmap concat

concatTileRow :: [Tile] -> Maybe [[Block]]
concatTileRow ts =
    let range = 0 .. tileSize - 1
        getRowComponents n = map (getRow n) ts
        maybes = map (getRowComponents >>> mconcat) range
    in  sequence maybes

-- The number of blocks along one side of a tile in the level map. This allows
-- a simpler model, since any given object may exist in only one block.
-- Should be an odd number, since any tile should have one central block.
tileSize :: Number
tileSize = 15

halfTile :: Number
halfTile = floor (tileSize / 2)

-- the number of tiles along one side of a level map.
tilesAlongSide :: Number
tilesAlongSide = 15 

-- the height or width in a level, in blocks.
mapSize :: Number
mapSize = tileSize * tilesAlongSide

mirror :: forall a. a -> a -> [a]
mirror x y =
    let xs = replicate halfTile x
    in xs <> [y] <> xs

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
    let upperRow = mirror Wall Empty
        centralRow = replicate halfTile Wall <> replicate (halfTile + 1) Empty
        lowerRow = replicate tileSize Wall
    in  replicate halfTile upperRow <>
            [centralRow] <>
            replicate halfTile lowerRow

downrightCorner :: Tile
downrightCorner = rotateCW uprightCorner
downleftCorner :: Tile
downleftCorner = rotateCW downrightCorner
upleftCorner :: Tile
upleftCorner = rotateCW downleftCorner

basicTileMap :: [[Tile]]
basicTileMap =
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

basicMap :: LevelMap
basicMap = case concatTiles basicTileMap of
    Just x  -> LevelMap x
    Nothing -> LevelMap [[]]
