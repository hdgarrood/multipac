module Types where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Either as E
import Data.JSON hiding ((.:))
import Data.Tuple
import Data.String
import Data.Array (map, singleton)
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Lens hiding ((.=))

import Utils

-- newtype wrapper is just so that the ReaderT instance works
type Game = {map :: LevelMap, player :: Player}
newtype WrappedGame = WrappedGame Game

unwrapGame :: WrappedGame -> Game
unwrapGame (WrappedGame g) = g

type LevelMap = {blocks :: [[Block]]}
data Block = Wall | Empty

instance showBlock :: Show Block where
  show Wall = "Wall"
  show Empty = "Empty"

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false

showRecord :: String -> [String] -> String
showRecord name props =
    "(" <> name <> " {" <> joinWith ", " props <> "})"

(.:) :: forall a. (Show a) => String -> a -> String
(.:) name value = name <> ": " <> show value

newtype Position = Position {x :: Number, y :: Number}

instance showPosition :: Show Position where
  show (Position p) =
    showRecord "Position" ["x" .: p.x, "y" .: p.y]

instance fromJsonPosition :: FromJSON Position where
  parseJSON (JObject obj) =
    case M.toList obj of
      [Tuple "x" x, Tuple "y" y] -> do
        x' <- parseJSON x
        y' <- parseJSON y
        return $ Position { x: x', y: y' }
      _ -> fail $ "failed to parse " <> show obj <> " as Position."

  parseJSON v = fail $ "failed to parse " <> show v <> " as Position."

instance toJsonPosition :: ToJSON Position where
  toJSON (Position p) = object ["x" .= toJSON p.x, "y" .= toJSON p.y]

add :: Position -> Position -> Position
add (Position p) (Position q) = Position {x: p.x + q.x, y: p.y + q.y}

data GameObject = GOPlayer Player | GOItem Item

instance showGameObject :: Show GameObject where
  show (GOPlayer p) = "GOPlayer (" <> show p <> ")"
  show (GOItem i)   = "GOItem (" <> show i <> ")"

newtype Player
  = Player
      { position :: Position
      , direction :: Maybe Direction
      , intendedDirection :: Maybe Direction
      }

instance showPlayer :: Show Player where
  show (Player p) =
    showRecord "Player"
      [ "position" .: p.position
      , "direction" .: p.direction
      , "intendedDirection" .: p.intendedDirection
      ]

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

newtype Item
  = Item
     { position :: Position
     , itemType :: ItemType
     }

instance showItem :: Show Item where
  show (Item i) =
    showRecord "Item"
      [ "position" .: i.position
      , "itemType" .: i.itemType
      ]

data Direction = Up | Down | Left | Right

instance showDirection :: Show Direction where
  show Up = "Up"
  show Down = "Down"
  show Left = "Left"
  show Right = "Right"

instance fromJsonDirection :: FromJSON Direction where
  parseJSON (JString str) =
    let mapping = M.fromList [ "up" ~ Up
                             , "down" ~ Down
                             , "left" ~ Left
                             , "right" ~ Right
                             ]
    in  case M.lookup str mapping of
          Just v -> E.Right v
          Nothing -> fail $ "unable to parse " <> show str <> " as Direction."
  parseJSON i = fail $ "unable to parse " <> show i <> " as Direction."

instance toJsonDirection :: ToJSON Direction where
  toJSON Up = JString "up"
  toJSON Down = JString "down"
  toJSON Left = JString "left"
  toJSON Right = JString "right"

data ItemType = LittleDot | BigDot | Cherry

instance showItemType :: Show ItemType where
  show LittleDot = "LittleDot"
  show BigDot = "BigDot"
  show Cherry = "Cherry"

dirToPos :: Direction -> Position
dirToPos Up    = Position {x:  0, y: -1}
dirToPos Left  = Position {x: -1, y:  0}
dirToPos Right = Position {x:  1, y:  0}
dirToPos Down  = Position {x:  0, y:  1}

data Input = Input (Maybe Direction)

instance showInput :: Show Input where
  show (Input x) = "Input (" <> show x <> ")"

data GameUpdate
  = ChangedDirection (Maybe Direction)
  | ChangedIntendedDirection (Maybe Direction)
  | ChangedPosition Position

instance showGameUpdate :: Show GameUpdate where
  show (ChangedDirection x) =
    "ChangedDirection (" <> show x <> ")"
  show (ChangedIntendedDirection x) =
    "ChangedIntendedDirection (" <> show x <> ")"
  show (ChangedPosition x) =
    "ChangedPosition (" <> show x <> ")"

toMap :: GameUpdate -> M.Map String String
toMap update =
  M.fromList $ singleton $ case update of
    (ChangedPosition p)          -> "changedPosition" ~ encode p
    (ChangedDirection d)         -> "changedDirection" ~ encode d
    (ChangedIntendedDirection d) -> "changedIntendedDirection" ~ encode d

fromMap :: M.Map String String -> Maybe GameUpdate
fromMap map =
  case M.toList map of
    [Tuple "changedPosition" p] ->
      ChangedPosition <$> decode p
    [Tuple "changedDirection" d] ->
      ChangedDirection <$> decode d
    [Tuple "changedIntendedDirection" d] ->
      ChangedIntendedDirection <$> decode d
    _ -> Nothing

instance fromJSONGameUpdate :: FromJSON GameUpdate where
  parseJSON val = do
    valAsMap <- parseJSON val
    case fromMap valAsMap of
      Just v -> return v
      Nothing -> fail $ "failed to parse " <> show val <> " as GameUpdate."

instance toJSONGameUpdate :: ToJSON GameUpdate where
  toJSON update =
    object $ singleton $ case update of
      (ChangedPosition p)          -> "changedPosition" .= p
      (ChangedDirection d)         -> "changedDirection" .= d
      (ChangedIntendedDirection d) -> "changedIntendedDirection" .= d

type GameUpdateM a = WriterT [GameUpdate] (State WrappedGame) a

-- these are just here to shorten declarations that are required because of
-- types that psc is not able to infer
type GameUpdateState = WrappedGame
type GameUpdateModifier = GameUpdateState -> GameUpdateState

innerGame :: LensP WrappedGame Game
innerGame = lens (\(WrappedGame g) -> g) (const WrappedGame)

tellGameUpdate :: GameUpdate -> GameUpdateM Unit
tellGameUpdate = tell <<< singleton

modifyGame :: (Game -> Game) -> GameUpdateM Unit
modifyGame = modify <<< f
  where
  f :: (Game -> Game) -> GameUpdateModifier
  f = over innerGame

getGame :: GameUpdateM Game
getGame = gets f
  where
  f :: GameUpdateState -> Game
  f x = x ^. innerGame

runGameUpdateM :: forall a.
  Game -> GameUpdateM a -> Tuple a (Tuple Game [GameUpdate])
runGameUpdateM game action =
  let a0 = runWriterT action
      a1 = runState a0 (WrappedGame game)
      rearrange (Tuple (Tuple x gus) (WrappedGame g))
        = Tuple x (Tuple g gus)

  in  rearrange a1

execGameUpdateM :: forall a.
  Game -> GameUpdateM a -> Tuple Game [GameUpdate]
execGameUpdateM game action = snd $ runGameUpdateM game action
