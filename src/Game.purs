module Game where

import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((!!), range)
import qualified Data.Map as M
import Data.Foldable
import Control.Alt
import Control.Monad
import Control.Monad.Reader.Class
import Control.Lens (LensP(), TraversalP(), lens, at, _Just,
                     (.~), (..), (^.), (%~))
import Math (ceil, floor)

import Types
import LevelMap
import Utils

-- update signalling

player :: PlayerId -> TraversalP Game Player
player pId = players .. at pId .. _Just

applyGameUpdate :: GameUpdate -> Game -> Game
applyGameUpdate u =
  case u of
    GUPU pId x ->
      player pId %~ applyPlayerUpdate x

applyPlayerUpdate :: PlayerUpdate -> Player -> Player
applyPlayerUpdate u =
  case u of
    (ChangedPosition p) -> pPosition .~ p
    (ChangedDirection d) -> pDirection .~ d
    (ChangedIntendedDirection d) -> pIntendedDirection .~ d

applyGameUpdateM :: GameUpdate -> GameUpdateM Unit
applyGameUpdateM update = do
  tellGameUpdate update
  modifyGame (applyGameUpdate update)

changePosition :: PlayerId -> Position -> GameUpdateM Unit
changePosition pId p =
  applyGameUpdateM (GUPU pId (ChangedPosition p))

changeDirection :: PlayerId -> Maybe Direction -> GameUpdateM Unit
changeDirection pId d =
  applyGameUpdateM (GUPU pId (ChangedDirection d))

changeIntendedDirection :: PlayerId -> Maybe Direction -> GameUpdateM Unit
changeIntendedDirection pId d =
  applyGameUpdateM (GUPU pId (ChangedIntendedDirection d))

initialGame :: Game
initialGame =
  { map: levelmap
  , players: M.fromList $ f <$> [1,2,3,4]
  , items: makeItems levelmap
  }
  where
  levelmap = basicMap2
  f n = Tuple (fromJust (intToPlayerId n))
          (Player { position: tilePositionToBlock (Position {x: n, y: 1})
                  , direction: Nothing
                  , intendedDirection: Nothing
                  })

stepGame :: Input -> Game -> Tuple Game [GameUpdate]
stepGame input game =
  let actions = [handleInput input, doLogic]
      action = foldl (>>) (return unit) actions
  in  execGameUpdateM game action

handleInput :: Input -> GameUpdateM Unit
handleInput input =
  for_ (M.toList input) $ \(Tuple pId maybeDir) ->
    whenJust maybeDir $ \newDirection ->
      changeIntendedDirection pId (Just newDirection)

doLogic :: GameUpdateM Unit
doLogic = do
  eachPlayer updateDirection
  eachPlayer movePlayer

updateDirection :: PlayerId -> Player -> GameUpdateM Unit
updateDirection pId p =
  whenJust (p ^.pIntendedDirection) $ tryChangeDirection pId p

movePlayer :: PlayerId -> Player -> GameUpdateM Unit
movePlayer pId p =
  whenJust (p ^. pDirection) $ \dir -> do
    ok <- canMoveInDirection p dir
    when ok $
      changePosition pId $ moveInDirection dir (p ^. pPosition)

tryChangeDirection :: PlayerId -> Player -> Direction -> GameUpdateM Unit
tryChangeDirection pId p d = do
  ok <- canMoveInDirection p d
  when ok $ do
    changeDirection pId (Just d)
    changeIntendedDirection pId Nothing

canMoveInDirection :: Player -> Direction -> GameUpdateM Boolean
canMoveInDirection (Player p) d =
  let newPosition = moveInDirection d p.position
      canMove game = isFree game.map newPosition
  in  canMove <$> getGame

moveInDirection :: Direction -> Position -> Position
moveInDirection d p = add p (dirToPos d)

isFree :: LevelMap -> Position -> Boolean
isFree levelmap pos =
  let block = getBlockAt pos levelmap
  in  maybe false (not <<< isWall) block

-- Prepare items on a map
-- * put a little dot in every free space
makeItems :: LevelMap -> [Item]
makeItems levelmap =
  Tuple <$> r <*> r >>= \(Tuple x y) ->
    let pos = tilePositionToBlock (Position {x: x, y: y})
    in if isFree levelmap pos
         then [Item { itemType: LittleDot, position: pos }]
         else []
  where
  r = range 0 (tilesAlongSide - 1)
