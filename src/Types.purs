module Types where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable
import Data.Traversable
import Data.Function 
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either)
import Data.Either as E
import Data.Tuple
import Data.String hiding (singleton, uncons)
import Data.Array (singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Graphics.Canvas
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class
import Control.Monad.State
import Control.Monad.State.Class
import Effect
import Effect.Ref (Ref)
import Effect.Timer
import Data.Lens
import Data.Lens.Getter ((^.))
import Data.Lens.Setter (over)
import Data.Lens.Types (Lens')
import Math (floor, pi, pow)

-- newtype wrapper is just for instances. We have to duplicate the type synonym
-- because of psc bug #1443
type Game =
  { map :: WrappedLevelMap
  , players :: Map PlayerId Player
  , items   :: Map ItemId Item
  , countdown :: Maybe Int
  , rampage :: Maybe Rampage
  , safeZone :: Array Position
  }

newtype WrappedGame = WrappedGame
  { map :: WrappedLevelMap
  , players :: Map PlayerId Player
  , items   :: Map ItemId Item
  , countdown :: Maybe Int
  , rampage :: Maybe Rampage
  , safeZone :: Array Position
  }

derive instance eqWrappedGame :: Eq WrappedGame
derive instance genericWrappedGame :: Generic WrappedGame _

unwrapGame :: WrappedGame -> Game
unwrapGame (WrappedGame g) = g

data PlayerId = P1 | P2 | P3 | P4

-- | For use in UIs, instead of the Show instance.
displayPlayerId :: PlayerId -> String
displayPlayerId x =
  case x of
    P1 -> "P1"
    P2 -> "P2"
    P3 -> "P3"
    P4 -> "P4"

derive instance eqPlayerId :: Eq PlayerId
derive instance ordPlayerId :: Ord PlayerId
derive instance genericPlayerId :: Generic PlayerId _

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
  show = genericShow

type ItemId = Int

-- psc bug #1443
type LevelMap =
  { blocks   :: Array (Array Block)
  , tiles    :: Array (Array Tile)
  }

newtype WrappedLevelMap = WrappedLevelMap
  { blocks   :: Array (Array Block)
  , tiles    :: Array (Array Tile)
  }

unwrapLevelMap :: WrappedLevelMap -> LevelMap
unwrapLevelMap (WrappedLevelMap m) = m

derive instance eqLevelMap :: Eq WrappedLevelMap
derive instance genericLevelMap :: Generic WrappedLevelMap _

data Block = Wall | Empty

derive instance eqBlock :: Eq Block
derive instance genericBlock :: Generic Block _

instance showBlock :: Show Block where
  show = genericShow

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

derive instance eqTime :: Eq Tile
derive instance genericTile :: Generic Tile _

instance showTile :: Show Tile where
  show = genericShow

isWall :: Block -> Boolean
isWall Wall = true
isWall _ = false

showRecord :: String -> Array String -> String
showRecord name props =
    "(" <> name <> " {" <> joinWith ", " props <> "})"

failJsonParse :: forall a b. (Show a) => a -> String -> Either String b
failJsonParse value typ =
  E.Left $ "failed to parse " <> show value <> " as " <> typ <> "."

newtype Position = Position {x :: Number, y :: Number}

derive instance eqPosition :: Eq Position
derive instance genericPosition :: Generic Position _

instance showPosition :: Show Position where
  show = genericShow

addPos :: Position -> Position -> Position
addPos (Position p) (Position q) = Position {x: p.x + q.x, y: p.y + q.y}

scalePos :: Number -> Position -> Position
scalePos s (Position p) = Position {x: s * p.x, y: s * p.y}

quadrance :: Position -> Position -> Number
quadrance (Position p) (Position q) =
  ((p.x - q.x) `pow` 2.0) + ((p.y - q.y) `pow` 2.0)

data GameObject = GOPlayer Player | GOItem Item

derive instance eqGameObject :: Eq GameObject
derive instance genericGameObject :: Generic GameObject _

newtype Player
  = Player
      { position :: Position
      , direction :: Maybe Direction
      , intendedDirection :: Maybe Direction
      , score :: Int
      , nomIndex :: Int
      , respawnCounter :: Maybe Int
      }

derive instance eqPlayer :: Eq Player
derive instance genericPlayer :: Generic Player _

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

players :: Lens' Game (Map PlayerId Player)
players = lens _.players (\o x -> o { players = x })

items :: Lens' Game (Map ItemId Item)
items = lens _.items (\o x -> o { items = x })

pPosition :: Lens' Player Position
pPosition = lens
  (\(Player p) -> p.position)
  (\(Player p) pos -> Player $ p { position = pos })

pDirection :: Lens' Player (Maybe Direction)
pDirection = lens
  (\(Player p) -> p.direction)
  (\(Player p) dir -> Player $ p { direction = dir })

pIntendedDirection :: Lens' Player (Maybe Direction)
pIntendedDirection = lens
  (\(Player p) -> p.intendedDirection)
  (\(Player p) dir -> Player $ p { intendedDirection = dir })

pScore :: Lens' Player Int
pScore = lens
  (\(Player p) -> p.score)
  (\(Player p) s -> Player $ p { score = s })

pNomIndex :: Lens' Player Int
pNomIndex = lens
  (\(Player p) -> p.nomIndex)
  (\(Player p) s -> Player $ p { nomIndex = s })

pRespawnCounter :: Lens' Player (Maybe Int)
pRespawnCounter = lens
  (\(Player p) -> p.respawnCounter)
  (\(Player p) s -> Player $ p { respawnCounter = s })

eachPlayer' :: forall f. (Applicative f) =>
  Game -> (PlayerId -> Player -> f Unit) -> f Unit
eachPlayer' game action =
  let
    ps :: Array (Tuple PlayerId Player)
    ps = Map.toUnfoldable $ game ^. players
  in
    for_ ps $ uncurry action


eachPlayer :: (PlayerId -> Player -> GameUpdateM Unit) -> GameUpdateM Unit
eachPlayer action = do
  game <- getGame
  eachPlayer' game action

newtype Item
  = Item
     { position :: Position
     , itemType :: ItemType
     }

derive instance eqItem :: Eq Item
derive instance genericItem :: Generic Item _

instance showItem :: Show Item where
  show = genericShow

iType :: Lens' Item ItemType
iType = lens
  (\(Item x) -> x.itemType)
  (\(Item x) typ -> Item $ x { itemType = typ })

iPosition :: Lens' Item Position
iPosition = lens
  (\(Item x) -> x.position)
  (\(Item x) pos -> Item $ x { position = pos })

pX :: Lens' Position Number
pX = lens
  (\(Position p) -> p.x)
  (\(Position p) x -> Position $ p { x = x })

pY :: Lens' Position Number
pY = lens
  (\(Position p) -> p.y)
  (\(Position p) y -> Position $ p { y = y })

eachItem' :: forall f. (Applicative f) =>
  Game -> (Item -> f Unit) -> f Unit
eachItem' game action =
  for_ (game ^. items) action

data Direction = Up | Down | Left | Right

derive instance eqDirection :: Eq Direction
derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show = genericShow

data ItemType = LittleDot | BigDot

derive instance eqItemType :: Eq ItemType
derive instance genericItemType :: Generic ItemType _

instance showItemType :: Show ItemType where
  show = genericShow

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
  | ChangedScore Int
  | ChangedNomIndex Int
  | ChangedRespawnCounter (Maybe Int)
  | PlayerLeft

derive instance eqPlayerUpdate :: Eq PlayerUpdate
derive instance genericPlayerUpdate :: Generic PlayerUpdate _

instance showPlayerUpdate :: Show PlayerUpdate where
  show = genericShow

data ItemUpdate
  = Eaten

derive instance eqItemUpdate :: Eq ItemUpdate
derive instance genericItemUpdate :: Generic ItemUpdate _

instance showItemUpdate :: Show ItemUpdate where
  show = genericShow

data GameEndReason
  = Completed
  | TooManyPlayersDisconnected

derive instance genericGameEndReason :: Generic GameEndReason _

instance showGameEndReason :: Show GameEndReason where
  show = genericShow

data Rampage
  = Rampaging PlayerId Int
  | Cooldown Int

derive instance eqRampage :: Eq Rampage
derive instance genericRampage :: Generic Rampage _

data GameUpdate
  = GUPU PlayerId PlayerUpdate
  | GUIU ItemId ItemUpdate
  | ChangedCountdown (Maybe Int)
  | GameEnded GameEndReason
  | ChangedRampage (Maybe Rampage)

derive instance genericGameUpdate :: Generic GameUpdate _

data ReadyState
  = Ready
  | NotReady

invertReadyState :: ReadyState -> ReadyState
invertReadyState Ready = NotReady
invertReadyState NotReady = Ready

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState
derive instance genericReadyState :: Generic ReadyState _

instance showReadyState :: Show ReadyState where
  show = genericShow

-- Sent by the server during the waiting stage, ie, after initial connection
-- but before the game starts
data WaitingUpdate
  = GameStarting WrappedGame
  | NewReadyStates (Map PlayerId ReadyState)

derive instance genericWaitingUpdate :: Generic WaitingUpdate _

type GameUpdateM a = WriterT (Array GameUpdate) (State WrappedGame) a

innerGame :: Lens' WrappedGame Game
innerGame = lens (\(WrappedGame g) -> g) (const WrappedGame)

tellGameUpdate :: GameUpdate -> GameUpdateM Unit
tellGameUpdate = tell <<< singleton

modifyGame :: (Game -> Game) -> GameUpdateM Unit
modifyGame = void <<< modify <<< over innerGame

getGame :: GameUpdateM Game
getGame = gets (\x -> x ^. innerGame)

runGameUpdateM :: forall a.
  Game -> GameUpdateM a -> Tuple a (Tuple Game (Array GameUpdate))
runGameUpdateM game action =
  let a0 = runWriterT action
      a1 = runState a0 (WrappedGame game)
      rearrange (Tuple (Tuple x gus) (WrappedGame g))
        = Tuple x (Tuple g gus)

  in  rearrange a1

execGameUpdateM :: forall a.
  Game -> GameUpdateM a -> Tuple Game (Array GameUpdate)
execGameUpdateM game action = snd $ runGameUpdateM game action

input_ = lens (\s -> s.input) (\s x -> s { input = x })
game_ = lens (\s -> s.game) (\s x -> s { game = x })

data GameState
  = WaitingForPlayers GameStateWaitingForPlayers
  | InProgress        GameStateInProgress

type GameStateWaitingForPlayers = Map PlayerId ReadyState
type GameStateInProgress = { game :: Game, input :: Input }

data ServerOutgoingMessage
  = SOWaiting WaitingUpdate
  | SOInProgress (Array GameUpdate)

derive instance genericServerOutgoingMessage :: Generic ServerOutgoingMessage _

asWaitingMessageO :: ServerOutgoingMessage -> Maybe WaitingUpdate
asWaitingMessageO (SOWaiting x) = Just x
asWaitingMessageO _ = Nothing

asInProgressMessageO :: ServerOutgoingMessage -> Maybe (Array GameUpdate)
asInProgressMessageO (SOInProgress x) = Just x
asInProgressMessageO _ = Nothing

data ServerIncomingMessage
  = SIToggleReadyState
  | SIInProgress Direction

derive instance genericServerIncomingMessage :: Generic ServerIncomingMessage _

asWaitingMessage :: ServerIncomingMessage -> Maybe Unit
asWaitingMessage (SIToggleReadyState) = Just unit
asWaitingMessage _ = Nothing

asInProgressMessage :: ServerIncomingMessage -> Maybe Direction
asInProgressMessage (SIInProgress d) = Just d
asInProgressMessage _ = Nothing

matchMessage :: forall m a b. (Monad m) =>
  (a -> Maybe b) -> a -> (b -> m Unit) -> m Unit
matchMessage f msg action =
  maybe (pure unit) action (f msg)

data ClientState
  = CWaitingForPlayers ClientStateWaiting
  | CInProgress        ClientStateInProgress

type ClientStateWaiting
  = { prevGame          :: Maybe Game
    , backgroundCleared :: Boolean
    , cachedHtml        :: String
    , readyStates       :: Map PlayerId ReadyState
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
