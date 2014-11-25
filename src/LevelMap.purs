module LevelMap where

import Data.Tuple
import Control.Lens
import Types
import Math (floor)
import Data.Array (map, reverse, (!!), range, concatMap, concat)
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Traversable (sequence)
import Data.Foldable (mconcat)
import Utils

-- the number of tiles along one side of a level map.
tilesAlongSide :: Number
tilesAlongSide = 15

-- the height or width in a level, in blocks.
mapSize :: Number
mapSize = tileSize * tilesAlongSide

normalize :: Tile -> Tuple Number Tile
normalize t =
  case t of
    Intersection       -> (0 ~ Intersection)
    TeeJunctionUp      -> (0 ~ TeeJunctionUp)
    TeeJunctionRight   -> (1 ~ TeeJunctionUp)
    TeeJunctionDown    -> (2 ~ TeeJunctionUp)
    TeeJunctionLeft    -> (3 ~ TeeJunctionUp)
    CornerUpRight      -> (0 ~ CornerUpRight)
    CornerRightDown    -> (1 ~ CornerUpRight)
    CornerDownLeft     -> (2 ~ CornerUpRight)
    CornerLeftUp       -> (3 ~ CornerUpRight)
    StraightHorizontal -> (0 ~ StraightHorizontal)
    StraightVertical   -> (1 ~ StraightHorizontal)

applyN :: forall a. Number -> (a -> a) -> a -> a
applyN = go id
  where
  go f n _ | n <= 0 = f
  go f n g = go (f >>> g) (n - 1) g

toBlockTile :: Tile -> BlockTile
toBlockTile t =
  case normalize t of
    Tuple n t' -> applyN n rotateCW (convert t')
  where
  convert t =
    case t of
      Intersection       -> intersectionB
      TeeJunctionUp      -> teeJunctionUpB
      CornerUpRight      -> cornerUpRightB
      StraightHorizontal -> straightHorizontalB

rotateCW :: BlockTile -> BlockTile
rotateCW = transpose >>> map reverse

concatTiles :: [[Tile]] -> Maybe [[Block]]
concatTiles =
  map (map toBlockTile) >>> map concatTileRow >>> sequence >>> fmap concat

concatTileRow :: [BlockTile] -> Maybe [[Block]]
concatTileRow ts =
    let r = range 0 (tileSize - 1)
        getRow n t = t !! n
        getRowComponents n = map (getRow n) ts
        maybes = map (getRowComponents >>> mconcat) r
    in  sequence maybes

-- The number of blocks along one side of a tile in the level map. This allows
-- a simpler model, since any given object may exist in only one block.
-- Should be an odd number, since any tile should have one central block.
tileSize :: Number
tileSize = 11

halfTile :: Number
halfTile = floor (tileSize / 2)

mirror :: forall a. a -> a -> [a]
mirror x y =
    let xs = replicate halfTile x
    in xs <> [y] <> xs

intersectionB :: BlockTile
intersectionB =
    let normalRow = mirror Wall Empty
        centralRow = replicate tileSize Empty
    in mirror normalRow centralRow

teeJunctionUpB :: BlockTile
teeJunctionUpB =
    let upperRow   = mirror Wall Empty
        centralRow = replicate tileSize Empty
        lowerRow   = replicate tileSize Wall
    in replicate halfTile upperRow <>
        [centralRow] <>
        replicate halfTile lowerRow

cornerUpRightB :: BlockTile
cornerUpRightB =
    let upperRow = mirror Wall Empty
        centralRow = replicate halfTile Wall <> replicate (halfTile + 1) Empty
        lowerRow = replicate tileSize Wall
    in  replicate halfTile upperRow <>
            [centralRow] <>
            replicate halfTile lowerRow

straightHorizontalB :: BlockTile
straightHorizontalB =
  let centralRow = replicate tileSize Empty
      normalRow = replicate tileSize Wall
  in  mirror normalRow centralRow

basicTileMap :: [[Tile]]
basicTileMap =
    let n = tilesAlongSide - 2
        topRow =
            [CornerRightDown] <>
                replicate n TeeJunctionDown <>
                [CornerDownLeft]
        centralRow =
            [TeeJunctionRight] <>
                replicate n Intersection <>
                [TeeJunctionLeft]
        bottomRow =
            [CornerUpRight] <>
                replicate n TeeJunctionUp <>
                [CornerLeftUp]
    in [topRow] <> replicate n centralRow <> [bottomRow]


mkLevelMap :: [[Tile]] -> Maybe LevelMap
mkLevelMap ts = fmap (\bs -> { blocks: bs, tiles: ts }) (concatTiles ts)

basicMap :: LevelMap
basicMap = fromJust (mkLevelMap basicTileMap)

getBlockAt :: Position -> LevelMap -> Maybe Block
getBlockAt (Position pos) levelmap =
  case levelmap.blocks !! pos.y of
    Just row -> row !! pos.x
    Nothing -> Nothing
