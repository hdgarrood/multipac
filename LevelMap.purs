module LevelMap where

-- The number of blocks along one side of a tile in the level map. This allows
-- a simpler model, since any given object may exist in only one block.
-- Should be an odd number, since any tile should have one central block.
tileSize :: Number
tileSize = 15

-- the number of tiles along one side of a level map.
tilesAlongSide :: Number
tilesAlongSide = 20

-- the height or width in a level, in blocks.
mapSize :: Number
mapSize = tileSize * tilesAlongSide

topRight :: [[Block]]

standardGridRow :: [Block]
standardGridRow =
    concat (replicate tilesAlongSide standardTileRow)

standardTileRow :: [Block]
standardTileRow =
    let walls = replicate (round (tileSize / 2)) Wall
    in walls ++ [Empty] ++ walls

mostlyEmptyGridRow :: [Block]
mostlyEmptyGridRow =
    let wallsSize = floor (tileSize / 2)
        walls = replicate wallsSize Wall
        emptiesSize = mapSize - (2 * wallsSize)
        empties = replicate emptiesSize Empty
    in walls ++ empties ++ walls

intersectionRow :: [Block]
intersectionRow =
    let n = floor (tileSize / 2)
       leftEdge = replicate n Wall ++ replicate (n+1) Empty
       rightEdge = reverse leftEdge
       middle = 
