module Game where

import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((!!))
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
    (ChangedPosition p) -> position .~ p
    (ChangedDirection d) -> direction .~ d
    (ChangedIntendedDirection d) -> intendedDirection .~ d

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
  { map: basicMap2
  , players: M.fromList $ f <$> [1,2,3,4]
  }
  where
  f n = Tuple (fromJust (intToPlayerId n))
          (Player { position: Position {x: z' n, y: z' 1}
                  , direction: Nothing
                  , intendedDirection: Nothing
                  })
  z = floor (tileSize / 2)
  z' n = z + (n * tileSize)

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
  whenJust (p ^.intendedDirection) $ tryChangeDirection pId p

movePlayer :: PlayerId -> Player -> GameUpdateM Unit
movePlayer pId p =
  whenJust (p ^. direction) $ \dir -> do
    ok <- canMoveInDirection p dir
    when ok $
      changePosition pId $ moveInDirection dir (p ^. position)

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
