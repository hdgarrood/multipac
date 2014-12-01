module Types where

import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Function
import qualified Data.Map as M
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Data.String
import Data.Array (map, singleton)
import Graphics.Canvas
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Lens hiding ((.=))

import Utils
import qualified NodeWebSocket as WS

-- newtype wrapper is just so that the ReaderT instance works
type Game = { map :: LevelMap
            , players :: M.Map PlayerId Player
            , items :: [Item]
            }
newtype WrappedGame = WrappedGame Game

unwrapGame :: WrappedGame -> Game
unwrapGame (WrappedGame g) = g

data PlayerId = P1 | P2 | P3 | P4

allPlayerIds :: [PlayerId]
allPlayerIds = [P1, P2, P3, P4]

playerIdToInt :: PlayerId -> Number
playerIdToInt p =
  case p of
    P1 -> 1
    P2 -> 2
    P3 -> 3
    P4 -> 4

intToPlayerId :: Number -> Maybe PlayerId
intToPlayerId x =
  case x of
    1 -> Just P1
    2 -> Just P2
    3 -> Just P3
    4 -> Just P4
    _ -> Nothing

instance showPlayerId :: Show PlayerId where
  show P1 = "P1"
  show P2 = "P2"
  show P3 = "P3"
  show P4 = "P4"

instance eqPlayerId :: Eq PlayerId where
  (==) = (==) `on` playerIdToInt
  (/=) = (/=) `on` playerIdToInt

instance ordPlayerId :: Ord PlayerId where
  compare = compare `on` playerIdToInt

instance fromJSONPlayerId :: FromJSON PlayerId where
  parseJSON (JNumber n) =
    case intToPlayerId n of
      Just x -> return x
      Nothing -> failJsonParse n "PlayerId"
  parseJSON val = failJsonParse val "PlayerId"

type LevelMap = {blocks :: [[Block]], tiles :: [[Tile]]}
data Block = Wall | Empty

-- A fixed size two-dimensional array of blocks.
type BlockTile = [[Block]]

data Tile
  = Intersection
  | TeeJunctionUp
  | TeeJunctionRight
  | TeeJunctionDown
  | TeeJunctionLeft
  | CornerUpRight
  | CornerRightDown
  | CornerDownLeft
  | CornerLeftUp
  | StraightHorizontal
  | StraightVertical
  | Inaccessible

instance showTile :: Show Tile where
  show Intersection       = "Intersection"
  show TeeJunctionUp      = "TeeJunctionUp"
  show TeeJunctionRight   = "TeeJunctionRight"
  show TeeJunctionLeft    = "TeeJunctionLeft"
  show TeeJunctionDown    = "TeeJunctionDown"
  show CornerUpRight      = "CornerUpRight"
  show CornerRightDown    = "CornerRightDown"
  show CornerDownLeft     = "CornerDownLeft"
  show CornerLeftUp       = "CornerLeftUp"
  show StraightHorizontal = "StraightHorizontal"
  show StraightVertical   = "StraightVertical"
  show Inaccessible       = "Inaccessible"

instance showBlock :: Show Block where
  show Wall = "Wall"
  show Empty = "Empty"

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false

showRecord :: String -> [String] -> String
showRecord name props =
    "(" <> name <> " {" <> joinWith ", " props <> "})"

(.::) :: forall a. (Show a) => String -> a -> String
(.::) name value = name <> ": " <> show value

failJsonParse :: forall a b. (Show a) => a -> String -> JParser b
failJsonParse value typ =
  fail $ "failed to parse " <> show value <> " as " <> show typ <> "."

newtype Position = Position {x :: Number, y :: Number}

instance showPosition :: Show Position where
  show (Position p) =
    showRecord "Position" ["x" .:: p.x, "y" .:: p.y]

instance fromJsonPosition :: FromJSON Position where
  parseJSON (JArray [JNumber x, JNumber y]) = return $ Position { x: x, y: y}
  parseJSON v = failJsonParse v "Position"

instance toJsonPosition :: ToJSON Position where
  toJSON (Position p) = JArray [JNumber p.x, JNumber p.y]

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
      [ "position" .:: p.position
      , "direction" .:: p.direction
      , "intendedDirection" .:: p.intendedDirection
      ]

players :: forall r a. LensP { players :: a | r } a
players = lens (\o -> o.players) (\o x -> o { players = x })

items :: forall r a. LensP { items :: a | r } a
items = lens (\o -> o.items) (\o x -> o { items = x })

pPosition :: LensP Player Position
pPosition = lens
  (\(Player p) -> p.position)
  (\(Player p) pos -> Player $ p { position = pos })

pDirection :: LensP Player (Maybe Direction)
pDirection = lens
  (\(Player p) -> p.direction)
  (\(Player p) dir -> Player $ p { direction = dir })

pIntendedDirection :: LensP Player (Maybe Direction)
pIntendedDirection = lens
  (\(Player p) -> p.intendedDirection)
  (\(Player p) dir -> Player $ p { intendedDirection = dir })

eachPlayer' :: forall f. (Applicative f) =>
  Game -> (PlayerId -> Player -> f Unit) -> f Unit
eachPlayer' game action =
  for_ (M.toList $ game ^. players) $ uncurry action

eachPlayer :: (PlayerId -> Player -> GameUpdateM Unit) -> GameUpdateM Unit
eachPlayer action = do
  game <- getGame
  eachPlayer' game action

newtype Item
  = Item
     { position :: Position
     , itemType :: ItemType
     }

instance showItem :: Show Item where
  show (Item i) =
    showRecord "Item"
      [ "position" .:: i.position
      , "itemType" .:: i.itemType
      ]

iType :: LensP Item ItemType
iType = lens
  (\(Item x) -> x.itemType)
  (\(Item x) typ -> Item $ x { itemType = typ })

iPosition :: LensP Item Position
iPosition = lens
  (\(Item x) -> x.position)
  (\(Item x) pos -> Item $ x { position = pos })
  

eachItem' :: forall f. (Applicative f) =>
  Game -> (Item -> f Unit) -> f Unit
eachItem' game action =
  for_ (game ^. items) action

data Direction = Up | Down | Left | Right

instance showDirection :: Show Direction where
  show Up = "Up"
  show Down = "Down"
  show Left = "Left"
  show Right = "Right"

instance fromJsonDirection :: FromJSON Direction where
  parseJSON (JString str) =
    case str of
      "up"    -> return Up
      "down"  -> return Down
      "left"  -> return Left
      "right" -> return Right
      _       -> failJsonParse str "Direction"
  parseJSON i = failJsonParse i "Direction"

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

type Input = M.Map PlayerId (Maybe Direction)

{-- instance showInput :: Show Input where --}
{--   show (Input x) = "Input (" <> show x <> ")" --}

data PlayerUpdate
  = ChangedDirection (Maybe Direction)
  | ChangedIntendedDirection (Maybe Direction)
  | ChangedPosition Position

data GameUpdate
  = GUPU PlayerId PlayerUpdate

instance showPlayerUpdate :: Show PlayerUpdate where
  show (ChangedDirection x) =
    "ChangedDirection (" <> show x <> ")"
  show (ChangedIntendedDirection x) =
    "ChangedIntendedDirection (" <> show x <> ")"
  show (ChangedPosition x) =
    "ChangedPosition (" <> show x <> ")"

instance fromJSONPlayerUpdate :: FromJSON PlayerUpdate where
  parseJSON (JObject obj) = do
    case M.toList obj of
      [Tuple "cp" p] ->
        ChangedPosition <$> parseJSON p
      [Tuple "cd" d] ->
        ChangedDirection <$> parseJSON d
      [Tuple "cid" d] ->
        ChangedIntendedDirection <$> parseJSON d
      _ -> failJsonParse obj "PlayerUpdate"
  parseJSON val = failJsonParse val "PlayerUpdate"

instance toJSONPlayerUpdate :: ToJSON PlayerUpdate where
  toJSON update =
    object $ singleton $ case update of
      (ChangedPosition p)          -> "cp" .= p
      (ChangedDirection d)         -> "cd" .= d
      (ChangedIntendedDirection d) -> "cid" .= d

instance showGameUpdate :: Show GameUpdate where
  show (GUPU pId u) = "GUPU (" <> show pId <> ") (" <> show u <> ")"

instance fromJSONGameUpdate :: FromJSON GameUpdate where
  parseJSON (JObject obj) =
    case M.toList obj of
      [Tuple "pId" pId, Tuple "u" val] ->
        GUPU <$> parseJSON pId <*> parseJSON val
      _ -> failJsonParse obj "GameUpdate"
  parseJSON val = failJsonParse val "GameUpdate"

instance toJSONGameUodate :: ToJSON GameUpdate where
  toJSON (GUPU pId pUpd) =
    object ["pId" .= playerIdToInt pId, "u" .= pUpd]

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

type Connection =
  { wsConn :: WS.Connection
  , pId :: PlayerId
  , name :: String
  }

type ServerState =
  { game :: Game
  , input :: Input
  , connections :: [Connection]
  }

type ClientState =
  { game :: Game
  , prevGame :: Game
  , redrawMap :: Boolean
  }

type RenderingContext =
  { foreground :: Context2D
  , background :: Context2D
  }
