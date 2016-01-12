module LevelMap where

import Data.Either
import Data.Tuple
import Data.Char (charString)
import Data.String (split, trim, toCharArray, joinWith)
import Optic.Getter ((^.))
import Control.Monad (unless)
import Math (floor)
import Data.Array (map, filter, null, reverse, (!!), range, concatMap,
                   concat, length, replicate)
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Traversable (for, sequence)
import Data.Foldable (mconcat, all)

import Utils
import Types hiding (Direction(..))

-- the number of tiles along one side of a level map.
tilesAlongSide :: Number
tilesAlongSide = 17

-- the height or width in a level, in blocks.
mapSize :: Number
mapSize = tileSize * tilesAlongSide

-- convert tile coordinates into block coordinates, returning coordinates of
-- the block at the centre of the given tile
tilePositionToBlock :: Position -> Position
tilePositionToBlock (Position p) =
  Position { x: f p.x, y: f p.y }
  where
  f n = halfTile + (n * tileSize)

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
    Inaccessible       -> (0 ~ Inaccessible)

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
      Inaccessible       -> inaccessibleB

mkBlockTile :: BlockTile -> BlockTile
mkBlockTile b =
  let rightLength :: forall a. Array a -> Boolean
      rightLength x = length x == tileSize
      ok = rightLength b && all rightLength b
  in  if ok then b else error "bad dimensions for BlockTile"

rotateCW :: BlockTile -> BlockTile
rotateCW = map reverse >>> transpose

debugShowBlockTile :: BlockTile -> String
debugShowBlockTile bt =
  let rows = transpose bt
      showRow = map showBlock >>> joinWith ""
      showBlock b =
        case b of
          Wall -> "#"
          Empty -> " "
  in  joinWith "\n" (map showRow rows)

concatTiles :: Array (Array Tile) -> Maybe (Array (Array Block))
concatTiles =
  map (map toBlockTile) >>> map concatTileRow >>> sequence >>> fmap concat

concatTileRow :: Array BlockTile -> Maybe (Array (Array Block))
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

mirror :: forall a. a -> a -> Array a
mirror x y =
    let xs = replicate halfTile x
    in xs <> [y] <> xs

intersectionB :: BlockTile
intersectionB =
    let normalRow = mirror Wall Empty
        centralRow = replicate tileSize Empty
    in mkBlockTile $ mirror normalRow centralRow

teeJunctionUpB :: BlockTile
teeJunctionUpB =
    let normalCol  = mirror Wall Empty
        centralCol = replicate halfTile Empty <>
                        [Empty] <>
                        replicate halfTile Wall
    in  mkBlockTile $ mirror normalCol centralCol

cornerUpRightB :: BlockTile
cornerUpRightB =
    let upperRow = mirror Wall Empty
        centralRow = replicate halfTile Wall <> replicate (halfTile + 1) Empty
        lowerRow = replicate tileSize Wall
        byRows =
          replicate halfTile upperRow <>
              [centralRow] <>
              replicate halfTile lowerRow
    in  mkBlockTile $ transpose byRows

straightHorizontalB :: BlockTile
straightHorizontalB =
  let col = mirror Wall Empty
  in  mkBlockTile $ replicate tileSize col

inaccessibleB :: BlockTile
inaccessibleB = mkBlockTile $ replicate tileSize (replicate tileSize Wall)

basicTileMap :: Array (Array Tile)
basicTileMap =
    let n = tilesAlongSide - 2
        leftCol =
            [CornerRightDown] <>
                replicate n TeeJunctionDown <>
                [CornerUpRight]
        centralCol =
            [TeeJunctionDown] <>
                replicate n Intersection <>
                [TeeJunctionUp]
        rightCol =
            [CornerDownLeft] <>
                replicate n TeeJunctionUp <>
                [CornerLeftUp]
    in [leftCol] <> replicate n centralCol <> [rightCol]

fromString :: String -> Either String (Array (Array Tile))
fromString str = parseLevelMapString str >>= constructLevelMap

-- 'Wall' or 'Empty'
data BasicTile = W | E

instance eqBasicTile :: Eq BasicTile where
  (==) W W = true
  (==) E E = true
  (==) _ _ = false
  (/=) x y = not (x == y)

fail = Left

parseLevelMapString :: String -> Either String (Array (Array BasicTile))
parseLevelMapString str = do
  let toLines =
    split "\n" >>>
      map (trim >>> toCharArray) >>>
      filter (not <<< null) >>>
      transpose

  let lines = toLines str
  unless (rightLength lines && all rightLength lines) $
    fail $ "expected a square levelmap; dimensions should be " <>
            show tilesAlongSide <> " by " <> show tilesAlongSide

  for lines $ \line ->
    for line $ \char ->
      let c = charString char
      in case c of
        "#" -> return W
        "_" -> return E
        _   -> fail $ "unexpected char '" <> c <> "'; expected '#' or '_'"

  where
  rightLength :: forall a. Array a -> Boolean
  rightLength x = length x == tilesAlongSide

constructLevelMap :: Array (Array BasicTile) -> Either String (Array (Array Tile))
constructLevelMap basicTiles = do
  let tileIndices = range 0 (tilesAlongSide - 1)
  let b i j = basicTiles !! i >>= (\r -> r !! j)

  for tileIndices $ \i ->
    for tileIndices $ \j ->
      let centre = fromJust $ b i j
          above = fromMaybe W $ b i (j-1)
          below = fromMaybe W $ b i (j+1)
          right = fromMaybe W $ b (i+1) j
          left  = fromMaybe W $ b (i-1) j
      in  toTile centre above right below left

-- order is: centre, above, right, below, left
toTile :: BasicTile -> BasicTile -> BasicTile -> BasicTile -> BasicTile ->
          Either String Tile
toTile W _ _ _ _ = return Inaccessible
toTile E E E E E = return Intersection
toTile E W E E E = return TeeJunctionDown
toTile E E W E E = return TeeJunctionLeft
toTile E E E W E = return TeeJunctionUp
toTile E E E E W = return TeeJunctionRight
toTile E W W E E = return CornerDownLeft
toTile E W E W E = return StraightHorizontal
toTile E W E E W = return CornerRightDown
toTile E E W W E = return CornerLeftUp
toTile E E W E W = return StraightVertical
toTile E E E W W = return CornerUpRight
toTile E _ _ _ _ = fail "dead ends are not supported"

basicMap2 :: LevelMap
basicMap2 =
  fromJust $ mkLevelMap $ either error id $ fromString $
  """
  #################
  #_______________#
  #_##_##_####_##_#
  #_____#___#_____#
  #_###_###_#_###_#
  #_______________#
  ##_##_##_###_####
  ##_##_#___##___##
  #_____#___####_##
  #_#_#_##_##____##
  #_#_#_______##_##
  #_____###_#_____#
  ##_##___#_#_###_#
  #_____#___#_____#
  #_######_###_##_#
  #_______________#
  #################
  """

mkLevelMap :: Array (Array Tile) -> Maybe LevelMap
mkLevelMap ts = fmap (\bs -> { blocks: bs, tiles: ts }) (concatTiles ts)

basicMap :: LevelMap
basicMap = fromJust (mkLevelMap basicTileMap)

getBlockAt :: Position -> LevelMap -> Maybe Block
getBlockAt (Position pos) levelmap =
  levelmap.blocks !! pos.x >>= (\r -> r !! pos.y)
