module Game where

import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((!!), range, filter, length, take)
import qualified Data.Map as M
import Data.Foldable
import Control.Alt
import Control.Monad
import Control.Monad.Reader.Class
import Control.Lens (LensP(), TraversalP(), lens, at, _Just, _1, _2,
                     (.~), (..), (^.), (%~), (~))
import Math (ceil, floor, pi)

import Types
import LevelMap
import Utils


minPlayers = 2


-- update signalling

player :: PlayerId -> TraversalP Game Player
player pId = players .. at pId .. _Just

item :: ItemId -> TraversalP Game Item
item iId = items .. at iId .. _Just

applyGameUpdate :: GameUpdate -> Game -> Game
applyGameUpdate u =
  case u of
    GUPU pId PlayerLeft ->
      removePlayer pId
    GUPU pId x ->
      player pId %~ applyPlayerUpdate x
    GUIU iId x ->
      items .. at iId %~ applyItemUpdate x
    ChangedCountdown x ->
      setCountdown x
    GameEnded _ ->
      id
  where
  setCountdown x game = game {countdown = x}

applyGameUpdates :: [GameUpdate] -> Game -> Game
applyGameUpdates updates game = foldr applyGameUpdate game updates

removePlayer :: PlayerId -> Game -> Game
removePlayer pId = players .. at pId .~ Nothing

applyPlayerUpdate :: PlayerUpdate -> Player -> Player
applyPlayerUpdate u =
  case u of
    (ChangedPosition p) -> pPosition .~ p
    (ChangedDirection d) -> pDirection .~ d
    (ChangedIntendedDirection d) -> pIntendedDirection .~ d
    (ChangedScore s) -> pScore .~ s
    (ChangedNomIndex a) -> pNomIndex .~ a

applyItemUpdate :: ItemUpdate -> Maybe Item -> Maybe Item
applyItemUpdate u =
  case u of
    Eaten -> const Nothing

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

changeScore :: PlayerId -> Number -> GameUpdateM Unit
changeScore pId s =
  applyGameUpdateM (GUPU pId (ChangedScore s))

changeCountdown :: Maybe Number -> GameUpdateM Unit
changeCountdown =
  applyGameUpdateM <<< ChangedCountdown

changeNomIndex :: PlayerId -> Number -> GameUpdateM Unit
changeNomIndex pId i =
  applyGameUpdateM (GUPU pId (ChangedNomIndex i))

eat :: ItemId -> GameUpdateM Unit
eat iId =
  applyGameUpdateM (GUIU iId Eaten)

endGame :: GameEndReason -> GameUpdateM Unit
endGame =
  applyGameUpdateM <<< GameEnded

initialGame :: Game
initialGame =
  { map: levelmap
  , players: M.fromList $ f <$> starts
  , items:   M.fromList $ take 1 $ zipNumbers $ makeItems levelmap (snd <$> starts)
  , countdown: Just 90
  }
  where
  levelmap = basicMap2
  f (Tuple pId position) = Tuple pId (mkPlayer position)
  starts =
    [ P1 ~ tilePositionToBlock (Position {x:7, y:7})
    , P2 ~ tilePositionToBlock (Position {x:9, y:7})
    , P3 ~ tilePositionToBlock (Position {x:7, y:8})
    , P4 ~ tilePositionToBlock (Position {x:9, y:8})
    ]

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
  g <- getGame
  case g.countdown of
    Just x -> changeCountdown (decrementOrNothing x)
    Nothing -> do
      eachPlayer updateDirection
      eachPlayer movePlayer
      eachPlayer eatItems
      checkForGameEnd

decrementOrNothing x =
  let x' = x - 1
  in if x' <= 0
        then Nothing
        else Just x'

updateDirection :: PlayerId -> Player -> GameUpdateM Unit
updateDirection pId p =
  whenJust (p ^.pIntendedDirection) $ tryChangeDirection pId p

movePlayer :: PlayerId -> Player -> GameUpdateM Unit
movePlayer pId p =
  whenJust (p ^. pDirection) $ \dir -> do
    ok <- canMoveInDirection p dir
    when ok $ do
      changePosition pId $ moveInDirection dir (p ^. pPosition)
      changeNomIndex pId $ ((p ^. pNomIndex) + 1) % nomIndexMax

eatItems :: PlayerId -> Player -> GameUpdateM Unit
eatItems pId p = do
  g <- getGame
  whenJust (lookupItemByPosition (p ^. pPosition) g) $ \iId -> do
    eat iId
    changeScore pId (p ^. pScore + 1)

checkForGameEnd :: GameUpdateM Unit
checkForGameEnd = do
  g <- getGame
  whenJust (checkEnded g) endGame

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

-- Prepare items on a map
-- * put a little dot in every free space
makeItems :: LevelMap -> [Position] -> [Item]
makeItems levelmap excludes =
  Tuple <$> r <*> r >>= \(Tuple x y) ->
    let pos = tilePositionToBlock (Position {x: x, y: y})
    in if shouldHaveDot pos
         then [Item { itemType: LittleDot, position: pos }]
         else []
  where
  r = range 0 (tilesAlongSide - 1)
  shouldHaveDot pos = isFree levelmap pos && not (elem pos excludes)

makeGame :: [PlayerId] -> Game
makeGame pIds =
  let players' =
    deleteWhere (\pId _ -> not (elem pId pIds)) (initialGame.players)
  in initialGame { players = players' }

inCountdown :: Game -> Boolean
inCountdown g = isJust g.countdown

checkEnded :: Game -> Maybe GameEndReason
checkEnded g
  | M.isEmpty g.items                         = Just Completed
  | length (M.values g.players) < minPlayers  = Just TooManyPlayersDisconnected
  | otherwise                                 = Nothing

isEnded = isJust <<< checkEnded

-- TODO: performance
lookupItemByPosition :: Position -> Game -> Maybe ItemId
lookupItemByPosition pos g =
  case filter (\i -> (i ^. _2 ^. iPosition) == pos) (M.toList g.items) of
      [i] -> Just (i ^. _1)
      _   -> Nothing
