module Game where

import Data.Maybe
import Data.Array
import Control.Alt
import Control.Monad

import Types
import LevelMap
import Utils

handleInput :: Input -> Game -> GameUpdate Unit
handleInput (Input i) g =
  whenJust i $ \newDirection -> do
    case g.objects !! 0 of
      Just (GOPlayer p) ->
        changedIntendedDirection (Just newDirection)
      Just _ -> return unit
      Nothing -> return unit

stepPlayer :: Player -> Game -> GameUpdate Unit
stepPlayer p g = do
  whenJust p.intendedDirection $ \newDirection -> do
    tryChangeDirection p newDirection g

tryChangeDirection :: Player -> Direction -> Game -> GameUpdate Unit
tryChangeDirection p d g =
  when (canMoveInDirection p d g) $ do
    changedDirection (Just d)
    changedIntendedDirection Nothing

canMoveInDirection :: Player -> Direction -> Game -> Boolean
canMoveInDirection p d g =
  let newPosition = moveInDirection d p.position
  in isFree g.map newPosition

moveInDirection :: Direction -> Position -> Position
moveInDirection d p = add p (dirToPos d)

isFree :: LevelMap -> Position -> Boolean
isFree levelmap pos =
  let block = getBlockAt pos levelmap
  in  maybe false isWall block

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false
