module Types where

import Debug.Trace
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Function
import qualified Data.Map as M
import qualified Data.Either as E
import Data.JSON
import Data.Tuple
import Data.String hiding (singleton, uncons)
import Data.Array (map, singleton)
import Graphics.Canvas
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Eff
import Control.Lens hiding ((.=))
import Data.DOM.Simple.Types (DOM(), DOMEvent())

import Utils
import qualified NodeWebSocket as WS
import qualified BrowserWebSocket as BWS

instance toJSONMap :: (ToJSON k, ToJSON v) => ToJSON (M.Map k v) where
  toJSON m = JArray $ map toJSON (M.toList m)

instance fromJSONMap :: (Ord k, FromJSON k, FromJSON v) => FromJSON (M.Map k v) where
  parseJSON (JArray arr) =
    M.fromList <$> traverse parseJSON arr

-- newtype wrapper is just so that the ReaderT instance works
type Game = { map :: LevelMap
            , players :: M.Map PlayerId Player
            , items   :: M.Map ItemId Item
            , countdown :: Maybe Number
            }
newtype WrappedGame = WrappedGame Game

unwrapGame :: WrappedGame -> Game
unwrapGame (WrappedGame g) = g

instance toJSONWrappedGame :: ToJSON WrappedGame where
  toJSON (WrappedGame game) =
    object [ "map" .= toJSON (WrappedLevelMap game.map)
           , "players" .= toJSON game.players
           , "items" .= toJSON game.items
           , "countdown" .= toJSON game.countdown
           ]

instance fromJSONWrappedGame :: FromJSON WrappedGame where
  parseJSON (JObject obj) = do
    (WrappedLevelMap m) <- obj .: "map"
    p <- obj .: "players"
    i <- obj .: "items"
    c <- obj .:? "countdown"
    return $ WrappedGame {map:m, players:p, items:i, countdown:c}

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

instance toJSONPlayerId :: ToJSON PlayerId where
  toJSON = JNumber <<< playerIdToInt

type ItemId = Number

type LevelMap = {blocks :: [[Block]], tiles :: [[Tile]]}
newtype WrappedLevelMap = WrappedLevelMap LevelMap

instance toJSONWrappedLevelMap :: ToJSON WrappedLevelMap where
  toJSON (WrappedLevelMap m) =
    object [ "blocks" .= toJSON m.blocks
           , "tiles" .= toJSON m.tiles
           ]

instance fromJSONWrappedLevelMap :: FromJSON WrappedLevelMap where
  parseJSON (JObject obj) = do
    b <- obj .: "blocks"
    t <- obj .: "tiles"
    return $ WrappedLevelMap {blocks: b, tiles: t}

data Block = Wall | Empty

instance showBlock :: Show Block where
  show Wall = "Wall"
  show Empty = "Empty"

instance toJSONBlock :: ToJSON Block where
  toJSON b = JString (show b)

instance fromJSONBlock :: FromJSON Block where
  parseJSON (JString "Wall") = return Wall
  parseJSON (JString "Empty") = return Empty
  parseJSON v = failJsonParse v "Block"

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

instance toJSONTile :: ToJSON Tile where
  toJSON t = JString (show t)

instance fromJSONTile :: FromJSON Tile where
  parseJSON (JString "Intersection") = return Intersection
  parseJSON (JString "TeeJunctionUp") = return TeeJunctionUp
  parseJSON (JString "TeeJunctionRight") = return TeeJunctionRight
  parseJSON (JString "TeeJunctionDown") = return TeeJunctionDown
  parseJSON (JString "TeeJunctionLeft") = return TeeJunctionLeft
  parseJSON (JString "CornerUpRight") = return CornerUpRight
  parseJSON (JString "CornerRightDown") = return CornerRightDown
  parseJSON (JString "CornerDownLeft") = return CornerDownLeft
  parseJSON (JString "CornerLeftUp") = return CornerLeftUp
  parseJSON (JString "StraightHorizontal") = return StraightHorizontal
  parseJSON (JString "StraightVertical") = return StraightVertical
  parseJSON (JString "Inaccessible") = return Inaccessible
  parseJSON v = failJsonParse v "Tile"

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
  fail $ "failed to parse " <> show value <> " as " <> typ <> "."

newtype Position = Position {x :: Number, y :: Number}

instance showPosition :: Show Position where
  show (Position p) =
    showRecord "Position" ["x" .:: p.x, "y" .:: p.y]

instance eqPosition :: Eq Position where
  (==) (Position p) (Position q) = p.x == q.x && p.y == q.y
  (/=) p q = not (p == q)

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
      , score :: Number
      }

instance toJSONPlayer :: ToJSON Player where
  toJSON (Player p) =
    JArray [ JString "Player"
           , toJSON p.position
           , toJSON p.direction
           , toJSON p.intendedDirection
           , toJSON p.score
           ]

instance fromJSONPlayer :: FromJSON Player where
  parseJSON
    (JArray [ JString "Player"
            , position
            , direction
            , intendedDirection
            , score
            ]) = do
                p <- parseJSON position
                d <- parseJSON direction
                i <- parseJSON intendedDirection
                s <- parseJSON score
                return $ Player
                          { position: p
                          , direction: d
                          , intendedDirection: i
                          , score: s
                          }



mkPlayer :: Position -> Player
mkPlayer pos =
  Player { position: pos
         , direction: Nothing
         , intendedDirection: Nothing
         , score: 0
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

pScore :: LensP Player Number
pScore = lens
  (\(Player p) -> p.score)
  (\(Player p) s -> Player $ p { score = s })

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

instance fromJSONItem :: FromJSON Item where
  parseJSON (JArray [JString "Item", position, itemType]) = do
    p <- parseJSON position
    i <- parseJSON itemType
    return $ Item {position: p, itemType: i}

instance toJSONItem :: ToJSON Item where
  toJSON (Item i) = JArray [ JString "Item"
                           , toJSON i.position
                           , toJSON i.itemType
                           ]

iType :: LensP Item ItemType
iType = lens
  (\(Item x) -> x.itemType)
  (\(Item x) typ -> Item $ x { itemType = typ })

iPosition :: LensP Item Position
iPosition = lens
  (\(Item x) -> x.position)
  (\(Item x) pos -> Item $ x { position = pos })

pX :: LensP Position Number
pX = lens
  (\(Position p) -> p.x)
  (\(Position p) x -> Position $ p { x = x })

pY :: LensP Position Number
pY = lens
  (\(Position p) -> p.y)
  (\(Position p) y -> Position $ p { y = y })

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

instance toJSONItemType :: ToJSON ItemType where
  toJSON = JString <<< show

instance fromJSONItemType :: FromJSON ItemType where
  parseJSON (JString "LittleDot") = return LittleDot
  parseJSON (JString "BigDot") = return BigDot
  parseJSON (JString "Cherry") = return Cherry
  parseJSON v = failJsonParse v "ItemType"

dirToPos :: Direction -> Position
dirToPos Up    = Position {x:  0, y: -1}
dirToPos Left  = Position {x: -1, y:  0}
dirToPos Right = Position {x:  1, y:  0}
dirToPos Down  = Position {x:  0, y:  1}

type Input = M.Map PlayerId (Maybe Direction)

data PlayerUpdate
  = ChangedDirection (Maybe Direction)
  | ChangedIntendedDirection (Maybe Direction)
  | ChangedPosition Position
  | ChangedScore Number

instance showPlayerUpdate :: Show PlayerUpdate where
  show (ChangedDirection x) =
    "ChangedDirection (" <> show x <> ")"
  show (ChangedIntendedDirection x) =
    "ChangedIntendedDirection (" <> show x <> ")"
  show (ChangedPosition x) =
    "ChangedPosition (" <> show x <> ")"

instance fromJSONPlayerUpdate :: FromJSON PlayerUpdate where
  parseJSON (JArray arr) =
    case arr of
       [JString "cp", x] -> ChangedPosition <$> parseJSON x
       [JString "cd", x] -> ChangedDirection <$> parseJSON x
       [JString "cid", x] -> ChangedIntendedDirection <$> parseJSON x
       [JString "cs", x] -> ChangedScore <$> parseJSON x

  parseJSON val = failJsonParse val "PlayerUpdate"

instance toJSONPlayerUpdate :: ToJSON PlayerUpdate where
  toJSON update =
    JArray $ case update of
      ChangedPosition p          -> [JString "cp", toJSON p]
      ChangedDirection d         -> [JString "cd", toJSON d]
      ChangedIntendedDirection d -> [JString "cid", toJSON d]
      ChangedScore x             -> [JString "cs", toJSON x]


data ItemUpdate
  = Eaten

instance showItemUpdate :: Show ItemUpdate where
  show Eaten = "Eaten"

instance fromJSONItemUpdate :: FromJSON ItemUpdate where
  parseJSON (JArray [JString "iu"]) = return Eaten
  parseJSON v = failJsonParse v "ItemUpdate"

instance toJSONItemUpdate :: ToJSON ItemUpdate where
  toJSON u = JArray [JString "iu"]

data GameUpdate
  = GUPU PlayerId PlayerUpdate
  | GUIU ItemId ItemUpdate
  | ChangedCountdown (Maybe Number)
  | GameEnded

instance showGameUpdate :: Show GameUpdate where
  show (GUPU pId u) = "GUPU (" <> show pId <> ") (" <> show u <> ")"
  show (GUIU iId u) = "GUIU (" <> show iId <> ") (" <> show u <> ")"
  show (ChangedCountdown x) = "ChangedCountdown " <> show x
  show GameEnded = "GameEnded"

instance fromJSONGameUpdate :: FromJSON GameUpdate where
  parseJSON (JArray arr) =
    case arr of
      [JString "gupu", pId, pUpd] ->
        GUPU <$> parseJSON pId <*> parseJSON pUpd
      [JString "guiu", iId, iUpd] ->
        GUIU <$> parseJSON iId <*> parseJSON iUpd
      [JString "ccd", x] ->
        ChangedCountdown <$> parseJSON x
      [JString "ged"] -> return GameEnded
      _ -> failJsonParse arr "GameUpdate"
  parseJSON val = failJsonParse val "GameUpdate"

instance toJSONGameUpdate :: ToJSON GameUpdate where
  toJSON (GUPU pId pUpd) =
    JArray [JString "gupu", toJSON pId, toJSON pUpd]
  toJSON (GUIU iId iUpd) =
    JArray [JString "guiu", toJSON iId, toJSON iUpd]
  toJSON (ChangedCountdown x) =
    JArray [JString "ccd", toJSON x]
  toJSON GameEnded =
    JArray [JString "ged"]

{-
a ConnectingResponse is sent to the client just after establishing a
connection, so that the client knows what its PlayerId is.
-}

data ConnectingResponse
  = YourPlayerIdIs PlayerId

instance toJSONConnectingResponse :: ToJSON ConnectingResponse where
  toJSON (YourPlayerIdIs pId) =
    JArray [JString "yourpId", toJSON pId]

instance fromJSONConnectingResponse :: FromJSON ConnectingResponse where
  parseJSON (JArray [JString "yourpId", pId]) =
    YourPlayerIdIs <$> parseJSON pId
  parseJSON v = failJsonParse v "ConnectingResponse"

-- Sent by the server during the waiting stage, ie, after initial connection
-- but before the game starts
data WaitingUpdate
  = GameStarting Game

instance toJSONWaitingUpdate :: ToJSON WaitingUpdate where
  toJSON (GameStarting game) =
    JArray [JString "starting", toJSON (WrappedGame game)]

instance fromJSONWaitingUpdate :: FromJSON WaitingUpdate where
  parseJSON (JArray [JString "starting", game]) =
    (GameStarting <<< unwrapGame) <$> parseJSON game
  parseJSON v = failJsonParse v "WaitingUpdate"

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
  { gameState :: GameState
  , connections :: [Connection]
  , callbacks :: ServerCallbacks
  }


type ServerCallback a = forall e.
  a -> ServerState
  -> Eff (trace :: Trace, ws :: WS.WebSocket | e) ServerState

newtype ServerCallbacks
  = ServerCallbacks
    { step :: ServerCallback {}
    , onMessage :: ServerCallback {msg::String, pId::PlayerId}
    , onNewPlayer :: ServerCallback {pId::PlayerId}
    , onClose :: ServerCallback {pId::PlayerId}
    }

unwrapServerCallbacks (ServerCallbacks sc) = sc

data GameState
  = WaitingForPlayers GameStateWaitingForPlayers
  | InProgress        GameStateInProgress

type GameStateWaitingForPlayers = M.Map PlayerId Boolean
type GameStateInProgress = { game :: Game, input :: Input }

type ClientCallback a = forall e.
  a -> ClientState
  -> Eff
    (trace :: Trace, ws :: BWS.WebSocket, canvas :: Canvas, dom :: DOM | e)
    ClientState

newtype ClientCallbacks
  = ClientCallbacks
    { onMessage :: ClientCallback {msg::String}
    , onKeyDown :: ClientCallback {event::DOMEvent}
    , render :: ClientCallback {ctx::RenderingContext}
    }

unwrapClientCallbacks (ClientCallbacks cc) = cc

type ClientStateInProgress
  = { game :: Game
    , prevGame :: Game
    , redrawMap :: Boolean
    }

type ClientState
  = { socket :: BWS.Socket
    , gameState :: ClientGameState
    , callbacks :: ClientCallbacks
    , playerId :: PlayerId
    }

data ClientGameState
  = CWaitingForPlayers Boolean
  | CInProgress ClientStateInProgress

type RenderingContext =
  { foreground :: Context2D
  , background :: Context2D
  }
