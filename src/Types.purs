module Types where

import Prelude
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Function
import Data.Map (Map())
import Data.Map as Map
import Data.Either (Either())
import Data.Either as E
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators ((:=), (.?))
import Data.Tuple
import Data.String hiding (singleton, uncons)
import Data.Array (singleton)
import Data.Sequence as S
import Data.Generic
import Graphics.Canvas
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import DOM.Timer
import Data.Lens
import Data.Lens.Getter ((^.))
import Data.Lens.Setter (over)
import Data.Lens.Types (LensP())
import DOM (DOM())
import Math (floor, pi, pow)

import GenericMap
import Utils
import NodeWebSocket as WS
import WebSocket as BWS

-- newtype wrapper is just for instances. We have to duplicate the type synonym
-- because of psc bug #1443
type Game =
  { map :: WrappedLevelMap
  , players :: GenericMap PlayerId Player
  , items   :: GenericMap ItemId Item
  , countdown :: Maybe Number
  , rampage :: Maybe Rampage
  , safeZone :: Array Position
  }

newtype WrappedGame = WrappedGame
  { map :: WrappedLevelMap
  , players :: GenericMap PlayerId Player
  , items   :: GenericMap ItemId Item
  , countdown :: Maybe Number
  , rampage :: Maybe Rampage
  , safeZone :: Array Position
  }

derive instance genericWrappedGame :: Generic WrappedGame

unwrapGame :: WrappedGame -> Game
unwrapGame (WrappedGame g) = g

instance encodeJsonWrappedGame :: EncodeJson WrappedGame where
  encodeJson = gEncodeJson

instance decodeJsonWrappedGame :: DecodeJson WrappedGame where
  decodeJson = gDecodeJson

data PlayerId = P1 | P2 | P3 | P4

derive instance genericPlayerId :: Generic PlayerId

allPlayerIds :: Array PlayerId
allPlayerIds = [P1, P2, P3, P4]

playerIdToInt :: PlayerId -> Int
playerIdToInt p =
  case p of
    P1 -> 1
    P2 -> 2
    P3 -> 3
    P4 -> 4

intToPlayerId :: Int -> Maybe PlayerId
intToPlayerId x =
  case x of
    1 -> Just P1
    2 -> Just P2
    3 -> Just P3
    4 -> Just P4
    _ -> Nothing

instance showPlayerId :: Show PlayerId where
  show = gShow

instance eqPlayerId :: Eq PlayerId where
  eq = gEq

instance ordPlayerId :: Ord PlayerId where
  compare = gCompare

instance decodeJsonPlayerId :: DecodeJson PlayerId where
  decodeJson = gDecodeJson

instance encodeJsonPlayerId :: EncodeJson PlayerId where
  encodeJson = gEncodeJson

type ItemId = Number

-- psc bug #1443
type LevelMap =
  { blocks   :: Array (Array Block)
  , tiles    :: Array (Array Tile)
  }

newtype WrappedLevelMap = WrappedLevelMap
  { blocks   :: Array (Array Block)
  , tiles    :: Array (Array Tile)
  }

derive instance genericLevelMap :: Generic WrappedLevelMap

instance encodeJsonLevelMap :: EncodeJson WrappedLevelMap where
  encodeJson = gEncodeJson

instance decodeJsonLevelMap :: DecodeJson WrappedLevelMap where
  decodeJson = gDecodeJson

data Block = Wall | Empty

derive instance genericBlock :: Generic Block

instance showBlock :: Show Block where
  show = gShow

instance encodeJsonBlock :: EncodeJson Block where
  encodeJson = gEncodeJson

instance decodeJSONBlock :: DecodeJson Block where
  decodeJson = gDecodeJson

-- A fixed size two-dimensional array of blocks.
type BlockTile = Array (Array Block)

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

derive instance genericTile :: Generic Tile

instance showTile :: Show Tile where
  show = gShow

instance encodeJsonTile :: EncodeJson Tile where
  encodeJson = gEncodeJson

instance decodeJsonTile :: DecodeJson Tile where
  decodeJson = gDecodeJson

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false

showRecord :: String -> Array String -> String
showRecord name props =
    "(" <> name <> " {" <> joinWith ", " props <> "})"

(.::) :: forall a. (Show a) => String -> a -> String
(.::) name value = name <> ": " <> show value

failJsonParse :: forall a b. (Show a) => a -> String -> Either String b
failJsonParse value typ =
  E.Left $ "failed to parse " <> show value <> " as " <> typ <> "."

newtype Position = Position {x :: Number, y :: Number}

derive instance genericPosition :: Generic Position

instance showPosition :: Show Position where
  show = gShow

instance eqPosition :: Eq Position where
  eq = gEq

instance decodeJsonPosition :: DecodeJson Position where
  decodeJson = gDecodeJson

instance encodeJsonPosition :: EncodeJson Position where
  encodeJson = gEncodeJson

addPos :: Position -> Position -> Position
addPos (Position p) (Position q) = Position {x: p.x + q.x, y: p.y + q.y}

scalePos :: Number -> Position -> Position
scalePos s (Position p) = Position {x: s * p.x, y: s * p.y}

quadrance :: Position -> Position -> Number
quadrance (Position p) (Position q) =
  ((p.x - q.x) `pow` 2.0) + ((p.y - q.y) `pow` 2.0)

data GameObject = GOPlayer Player | GOItem Item

derive instance genericGameObject :: Generic GameObject

newtype Player
  = Player
      { position :: Position
      , direction :: Maybe Direction
      , intendedDirection :: Maybe Direction
      , score :: Int
      , nomIndex :: Int
      , respawnCounter :: Maybe Int
      }

derive instance genericPlayer :: Generic Player

instance encodeJsonPlayer :: EncodeJson Player where
  encodeJson = gEncodeJson

instance decodeJsonPlayer :: DecodeJson Player where
  decodeJson = gDecodeJson

nomIndexMax = 10

mkPlayer :: Position -> Player
mkPlayer pos =
  Player { position: pos
         , direction: Nothing
         , intendedDirection: Nothing
         , score: 0
         , nomIndex: nomIndexMax / 2
         , respawnCounter: Nothing
         }

players :: LensP Game (Map PlayerId Player)
players = lens (\o -> runGenericMap o.players) (\o x -> o { players = GenericMap x })

items :: LensP Game (Map ItemId Item)
items = lens (\o -> runGenericMap o.items) (\o x -> o { items = GenericMap x })

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

pScore :: LensP Player Int
pScore = lens
  (\(Player p) -> p.score)
  (\(Player p) s -> Player $ p { score = s })

pNomIndex :: LensP Player Int
pNomIndex = lens
  (\(Player p) -> p.nomIndex)
  (\(Player p) s -> Player $ p { nomIndex = s })

pRespawnCounter :: LensP Player (Maybe Int)
pRespawnCounter = lens
  (\(Player p) -> p.respawnCounter)
  (\(Player p) s -> Player $ p { respawnCounter = s })

eachPlayer' :: forall f. (Applicative f) =>
  Game -> (PlayerId -> Player -> f Unit) -> f Unit
eachPlayer' game action =
  for_ (Map.toList $ game ^. players) $ uncurry action

eachPlayer :: (PlayerId -> Player -> GameUpdateM Unit) -> GameUpdateM Unit
eachPlayer action = do
  game <- getGame
  eachPlayer' game action

newtype Item
  = Item
     { position :: Position
     , itemType :: ItemType
     }

derive instance genericItem :: Generic Item

instance showItem :: Show Item where
  show = gShow

instance decodeJsonItem :: DecodeJson Item where
  decodeJson = gDecodeJson

instance encodeJsonItem :: EncodeJson Item where
  encodeJson = gEncodeJson

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

derive instance genericDirection :: Generic Direction

instance showDirection :: Show Direction where
  show = gShow

instance decodeJsonDirection :: DecodeJson Direction where
  decodeJson = gDecodeJson

instance encodeJsonDirection :: EncodeJson Direction where
  encodeJson = gEncodeJson

data ItemType = LittleDot | BigDot | Cherry

derive instance genericItemType :: Generic ItemType

instance showItemType :: Show ItemType where
  show = gShow

instance eqItemType :: Eq ItemType where
  eq = gEq

instance encodeJsonItemType :: EncodeJson ItemType where
  encodeJson = gEncodeJson

instance decodeJsonItemType :: DecodeJson ItemType where
  decodeJson = gDecodeJson

dirToPos :: Direction -> Position
dirToPos Up    = Position {x:  0.0, y: -1.0}
dirToPos Left  = Position {x: -1.0, y:  0.0}
dirToPos Right = Position {x:  1.0, y:  0.0}
dirToPos Down  = Position {x:  0.0, y:  1.0}

directionToRadians :: Direction -> Number
directionToRadians d =
  case d of
    Right -> 0.0
    Down -> pi / 2.0
    Left -> pi
    Up -> 3.0 * pi / 2.0

opposite d =
  case d of
    Up -> Down
    Down -> Up
    Right -> Left
    Left -> Right

type Input = Map PlayerId (Maybe Direction)

data PlayerUpdate
  = ChangedDirection (Maybe Direction)
  | ChangedIntendedDirection (Maybe Direction)
  | ChangedPosition Position
  | ChangedScore Number
  | ChangedNomIndex Number
  | ChangedRespawnCounter (Maybe Number)
  | PlayerLeft

derive instance genericPlayerUpdate :: Generic PlayerUpdate

instance showPlayerUpdate :: Show PlayerUpdate where
  show = gShow

instance decodeJsonPlayerUpdate :: DecodeJson PlayerUpdate where
  decodeJson = gDecodeJson

instance encodeJsonPlayerUpdate :: EncodeJson PlayerUpdate where
  encodeJson = gEncodeJson

data ItemUpdate
  = Eaten

derive instance genericItemUpdate :: Generic ItemUpdate

instance showItemUpdate :: Show ItemUpdate where
  show = gShow

instance decodeJsonItemUpdate :: DecodeJson ItemUpdate where
  decodeJson = gDecodeJson

instance encodeJsonItemUpdate :: EncodeJson ItemUpdate where
  encodeJson = gEncodeJson

data GameEndReason
  = Completed
  | TooManyPlayersDisconnected

derive instance genericGameEndReason :: Generic GameEndReason

instance showGameEndReason :: Show GameEndReason where
  show = gShow

instance encodeJsonGameEndReason :: EncodeJson GameEndReason where
  encodeJson = gEncodeJson

instance decodeJsonGameEndReason :: DecodeJson GameEndReason where
  decodeJson = gDecodeJson

data Rampage
  = Rampaging PlayerId Number
  | Cooldown Number

derive instance genericRampage :: Generic Rampage

instance encodeJsonRampage :: EncodeJson Rampage where
  encodeJson = gEncodeJson

instance decodeJsonRampage :: DecodeJson Rampage where
  decodeJson = gDecodeJson

data GameUpdate
  = GUPU PlayerId PlayerUpdate
  | GUIU ItemId ItemUpdate
  | ChangedCountdown (Maybe Number)
  | GameEnded GameEndReason
  | ChangedRampage (Maybe Rampage)

derive instance genericGameUpdate :: Generic GameUpdate

instance decodeJsonGameUpdate :: DecodeJson GameUpdate where
  decodeJson = gDecodeJson

instance encodeJsonGameUpdate :: EncodeJson GameUpdate where
  encodeJson = gEncodeJson

-- Sent by the server during the waiting stage, ie, after initial connection
-- but before the game starts
data WaitingUpdate
  = GameStarting WrappedGame
  | NewReadyStates (GenericMap PlayerId Boolean)

derive instance genericWaitingUpdate :: Generic WaitingUpdate

instance encodeJsonWaitingUpdate :: EncodeJson WaitingUpdate where
  encodeJson = gEncodeJson

instance decodeJsonWaitingUpdate :: DecodeJson WaitingUpdate where
  decodeJson = gDecodeJson

type GameUpdateM a = WriterT (S.Seq GameUpdate) (State WrappedGame) a

-- these are just here to shorten declarations that are required because of
-- types that psc is not able to infer
type GameUpdateState = WrappedGame
type GameUpdateModifier = GameUpdateState -> GameUpdateState

innerGame :: LensP WrappedGame Game
innerGame = lens (\(WrappedGame g) -> g) (const WrappedGame)

tellGameUpdate :: GameUpdate -> GameUpdateM Unit
tellGameUpdate = tell <<< S.singleton

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
  Game -> GameUpdateM a -> Tuple a (Tuple Game (S.Seq GameUpdate))
runGameUpdateM game action =
  let a0 = runWriterT action
      a1 = runState a0 (WrappedGame game)
      rearrange (Tuple (Tuple x gus) (WrappedGame g))
        = Tuple x (Tuple g gus)

  in  rearrange a1

execGameUpdateM :: forall a.
  Game -> GameUpdateM a -> Tuple Game (S.Seq GameUpdate)
execGameUpdateM game action = snd $ runGameUpdateM game action

input_ = lens (\s -> s.input) (\s x -> s { input = x })
game_ = lens (\s -> s.game) (\s x -> s { game = x })

data GameState
  = WaitingForPlayers GameStateWaitingForPlayers
  | InProgress        GameStateInProgress

type GameStateWaitingForPlayers = Map PlayerId Boolean
type GameStateInProgress = { game :: Game, input :: Input }

type ReadyState = Boolean

data ServerOutgoingMessage
  = SOWaiting WaitingUpdate
  | SOInProgress (Array GameUpdate)

derive instance genericServerOutgoingMessage :: Generic ServerOutgoingMessage

instance encodeJsonServerOutgoingMessage :: EncodeJson ServerOutgoingMessage where
  encodeJson = gEncodeJson

instance decodeJsonServerOutgoingMessage :: DecodeJson ServerOutgoingMessage where
  decodeJson = gDecodeJson

asWaitingMessageO :: ServerOutgoingMessage -> Maybe WaitingUpdate
asWaitingMessageO (SOWaiting x) = Just x
asWaitingMessageO _ = Nothing

asInProgressMessageO :: ServerOutgoingMessage -> Maybe (Array GameUpdate)
asInProgressMessageO (SOInProgress x) = Just x
asInProgressMessageO _ = Nothing

data ServerIncomingMessage
  = SIToggleReadyState
  | SIInProgress Direction

derive instance genericServerIncomingMessage :: Generic ServerIncomingMessage

instance encodeJsonServerIncomingMessage :: EncodeJson ServerIncomingMessage where
  encodeJson = gEncodeJson

instance decodeJsonServerIncomingMessage :: DecodeJson ServerIncomingMessage where
  decodeJson = gDecodeJson

asWaitingMessage :: ServerIncomingMessage -> Maybe Unit
asWaitingMessage (SIToggleReadyState) = Just unit
asWaitingMessage _ = Nothing

asInProgressMessage :: ServerIncomingMessage -> Maybe Direction
asInProgressMessage (SIInProgress d) = Just d
asInProgressMessage _ = Nothing

matchMessage :: forall m a b. (Monad m, EncodeJson a) =>
  (a -> Maybe b) -> a -> (b -> m Unit) -> m Unit
matchMessage f msg action =
  maybe (return unit) action (f msg)

data ClientState
  = CWaitingForPlayers ClientStateWaiting
  | CInProgress        ClientStateInProgress

type ClientStateWaiting
  = { prevGame          :: Maybe Game
    , backgroundCleared :: Boolean
    , cachedHtml        :: String
    , readyStates       :: Map PlayerId Boolean
    }

type ClientStateInProgress
  = { game :: Game
    , prevGame :: Game
    , redrawMap :: Boolean
    , cachedHtml :: String
    }

type RenderingContext =
  { foreground :: Context2D
  , background :: Context2D
  }

redrawMap = lens
  (\s -> s.redrawMap)
  (\s x -> s { redrawMap = x })

backgroundCleared = lens
  (\s -> s.backgroundCleared)
  (\s x -> s { backgroundCleared = x })

readyStates = lens
  (\s -> s.readyStates)
  (\s x -> s { readyStates = x })

cachedHtml = lens
  (\s -> s.cachedHtml)
  (\s x -> s { cachedHtml = x })
