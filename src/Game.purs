module Game where

import Data.Maybe
import Data.Array
import Data.Foldable
import Control.Alt
import Control.Monad
import Control.Monad.Reader.Class

import Types
import LevelMap
import Utils

initialGame :: Game
initialGame =
  { map: basicMap
  , objects:
    [ GOPlayer $ Player
      { position: Position {x: 7, y: 7}
      , direction: Nothing
      , intendedDirection: Nothing
      }
    ]
  }

stepGame :: Input -> Game -> Game
stepGame input game =
  let actions = [handleInput input, doLogic]
      go game action = applyUpdates game $ execGameUpdateM game action
  in foldl go game actions

handleInput :: Input -> GameUpdateM Unit
handleInput (Input i) =
  whenJust i $ \newDirection -> do
    tell $ ChangedIntendedDirection (Just newDirection)

doLogic :: GameUpdateM Unit
doLogic =
  withPlayer $ \p -> do
    updateDirection p
    movePlayer p

withPlayer :: (Player -> GameUpdateM Unit) -> GameUpdateM Unit
withPlayer action = do
  game <- askGame
  case getPlayer game of
    Just p -> action p
    _ -> return unit

getPlayer :: Game -> Maybe Player
getPlayer g =
  case g.objects !! 0 of
    Just (GOPlayer p) -> Just p
    _ -> Nothing

updateDirection :: Player -> GameUpdateM Unit
updateDirection (Player p) =
  whenJust p.intendedDirection $ tryChangeDirection (Player p)

movePlayer :: Player -> GameUpdateM Unit
movePlayer (Player p) =
  whenJust p.direction $ \dir ->
    tell $ ChangedPosition (moveInDirection dir p.position)

tryChangeDirection :: Player -> Direction -> GameUpdateM Unit
tryChangeDirection p d = do
  ok <- canMoveInDirection p d
  when ok $ do
    tell $ ChangedDirection (Just d)
    tell $ ChangedIntendedDirection Nothing

canMoveInDirection :: Player -> Direction -> GameUpdateM Boolean
canMoveInDirection (Player p) d =
  let newPosition = moveInDirection d p.position
      canMove game = isFree game.map newPosition
  in  canMove <$> askGame

moveInDirection :: Direction -> Position -> Position
moveInDirection d p = add p (dirToPos d)

isFree :: LevelMap -> Position -> Boolean
isFree levelmap pos =
  let block = getBlockAt pos levelmap
  in  maybe false isWall block

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false

applyUpdates :: Game -> [GameUpdate] -> Game
applyUpdates g us = foldl applyUpdate g us

applyUpdate :: Game -> GameUpdate -> Game
applyUpdate game update =
  game { objects = go <$> game.objects }
  where
  go (GOPlayer (Player p)) =
    GOPlayer $ Player $ case update of
      ChangedDirection newDir         -> p { direction = newDir }
      ChangedPosition newPos          -> p { position = newPos }
      ChangedIntendedDirection newDir -> p { intendedDirection = newDir }
  go x = x
