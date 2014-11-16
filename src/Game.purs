module Game where

import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((!!))
import Data.Foldable
import Control.Alt
import Control.Monad
import Control.Monad.Reader.Class
import Control.Lens (Lens(), LensP(), lens, (.~), at, (..), (^.))

import Types
import LevelMap
import Utils

-- lenses --

player :: forall r a. LensP { player :: a | r } a
player = lens (\o -> o.player) (\o p -> o { player = p })

position :: LensP Player Position
position = lens
  (\(Player p) -> p.position)
  (\(Player p) pos -> Player $ p { position = pos })

direction :: LensP Player (Maybe Direction)
direction = lens
  (\(Player p) -> p.direction)
  (\(Player p) dir -> Player $ p { direction = dir })

intendedDirection :: LensP Player (Maybe Direction)
intendedDirection = lens
  (\(Player p) -> p.intendedDirection)
  (\(Player p) dir -> Player $ p { intendedDirection = dir })

-- update signalling

changePosition :: Position -> GameUpdateM Unit
changePosition p = do
  tellGameUpdate (ChangedPosition p)
  modifyGame (player .. position .~ p)

changeDirection :: Maybe Direction -> GameUpdateM Unit
changeDirection d = do
  tellGameUpdate (ChangedDirection d)
  modifyGame (player .. direction .~ d)

changeIntendedDirection :: Maybe Direction -> GameUpdateM Unit
changeIntendedDirection d = do
  tellGameUpdate (ChangedIntendedDirection d)
  modifyGame (player .. intendedDirection .~ d)

initialGame :: Game
initialGame =
  { map: basicMap
  , player: Player
              { position: Position {x: 7, y: 7}
              , direction: Nothing
              , intendedDirection: Nothing
              }
  }

stepGame :: Input -> Game -> Tuple Game [GameUpdate]
stepGame input game =
  let actions = [handleInput input, doLogic]
      action = foldl (>>) (return unit) actions
  in  execGameUpdateM game action

handleInput :: Input -> GameUpdateM Unit
handleInput (Input i) =
  whenJust i $ \newDirection -> do
    changeIntendedDirection (Just newDirection)

doLogic :: GameUpdateM Unit
doLogic = do
  withPlayer updateDirection
  withPlayer movePlayer

withPlayer :: (Player -> GameUpdateM Unit) -> GameUpdateM Unit
withPlayer action = do
  game <- getGame
  action $ game ^. player

updateDirection :: Player -> GameUpdateM Unit
updateDirection (Player p) =
  whenJust p.intendedDirection $ tryChangeDirection (Player p)

movePlayer :: Player -> GameUpdateM Unit
movePlayer (Player p) =
  whenJust p.direction $ \dir ->
    changePosition (moveInDirection dir p.position)

tryChangeDirection :: Player -> Direction -> GameUpdateM Unit
tryChangeDirection p d = do
  ok <- canMoveInDirection p d
  when ok $ do
    changeDirection (Just d)
    changeIntendedDirection Nothing

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

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false
