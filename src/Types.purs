module Types where

import Data.Maybe
import Data.Tuple
import Data.String
import Data.Array (map)
import Control.Monad.Free
import Control.Monad.Reader.Trans
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class

import Utils

-- newtype wrapper is just so that the ReaderT instance works
type Game = {map :: LevelMap, objects :: [GameObject]}
newtype WrappedGame = WrappedGame Game

unwrapGame :: WrappedGame -> Game
unwrapGame (WrappedGame g) = g

type LevelMap = {blocks :: [[Block]]}
data Block = Wall | Empty

newtype Position = Position {x :: Number, y :: Number}

showRecord :: String -> [String] -> String
showRecord name props =
    "(" <> name <> " {" <> joinWith ", " props <> "})"

(.:) :: forall a. (Show a) => String -> a -> String
(.:) name value = name <> ": " <> show value

instance showPosition :: Show Position where
  show (Position p) =
    showRecord "Position" ["x" .: p.x, "y" .: p.y]

data GameObject = GOPlayer Player | GOItem Item

-- instance showGameObject :: Show GameObject where
--   show (GOPlayer p) = "(GOPlayer " <> show p <> ")"
--   show (GOItem i)   = "(GOItem " <> show i <> ")"

newtype Player
  = Player
      { position :: Position
      , direction :: Maybe Direction
      , intendedDirection :: Maybe Direction
      }

instance showPlayer :: Show Player where
  show (Player p) =
    "(Player {" <> joinWith ", " props <> "})"
    where
    props = showPair <$>
              [ Tuple "position" $ show p.position
              , Tuple "direction" $ show p.direction
              , Tuple "intendedDirection" $ show p.intendedDirection
              ]
    showPair (Tuple a b) = a <> ": " <> b

type Item
  = { position :: Position
    , itemType :: ItemType
    }

data Direction = Up | Down | Left | Right
data ItemType = LittleDot | BigDot | Cherry

instance showDirection :: Show Direction where
  show Up = "Up"
  show Down = "Down"
  show Left = "Left"
  show Right = "Right"

dirToPos :: Direction -> Position
dirToPos Up    = Position {x:  0, y: -1}
dirToPos Left  = Position {x: -1, y:  0}
dirToPos Right = Position {x:  1, y:  0}
dirToPos Down  = Position {x:  0, y:  1}

add :: Position -> Position -> Position
add (Position p) (Position q) = Position {x: p.x + q.x, y: p.y + q.y}

data Input = Input (Maybe Direction)

data GameUpdate
  = ChangedDirection (Maybe Direction)
  | ChangedIntendedDirection (Maybe Direction)
  | ChangedPosition Position

type GameUpdateM a = ReaderT WrappedGame (State [GameUpdate]) a

tell :: GameUpdate -> GameUpdateM Unit
tell gu = modify (unshift gu)

askGame :: GameUpdateM Game
askGame = reader unwrapGame

runGameUpdateM :: forall a. Game -> GameUpdateM a -> Tuple a [GameUpdate]
runGameUpdateM game action =
  runState (runReaderT action (WrappedGame game)) []

execGameUpdateM :: forall a. Game -> GameUpdateM a -> [GameUpdate]
execGameUpdateM game action = snd $ runGameUpdateM game action
