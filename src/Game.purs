module Game where

import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((!!), range, filter, length, take)
import qualified Data.Map as M
import Data.Foldable
import Control.Arrow
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
rampageLength = 180
respawnLength = 60

-- minimum squared distance that a player needs to be from another player in
-- order to eat them, in blocks.
minEatingQuadrance = 4

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
    ChangedRampage r ->
      \g -> g { rampage = r }
  where
  setCountdown x game = game {countdown = x}

applyGameUpdates :: [GameUpdate] -> Game -> Game
applyGameUpdates updates game = foldr applyGameUpdate game updates

removePlayer :: PlayerId -> Game -> Game
removePlayer pId = players .. at pId .~ Nothing

applyPlayerUpdate :: PlayerUpdate -> Player -> Player
applyPlayerUpdate u =
  case u of
    (ChangedPosition p)          -> pPosition .~ p
    (ChangedDirection d)         -> pDirection .~ d
    (ChangedIntendedDirection d) -> pIntendedDirection .~ d
    (ChangedScore s)             -> pScore .~ s
    (ChangedNomIndex a)          -> pNomIndex .~ a
    (ChangedRespawnCounter x)    -> pRespawnCounter .~ x

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

changeRespawnCounter :: PlayerId -> Maybe Number -> GameUpdateM Unit
changeRespawnCounter pId x =
  applyGameUpdateM (GUPU pId (ChangedRespawnCounter x))

endGame :: GameEndReason -> GameUpdateM Unit
endGame =
  applyGameUpdateM <<< GameEnded

changeRampage :: Maybe (Tuple PlayerId Number) -> GameUpdateM Unit
changeRampage =
  applyGameUpdateM <<< ChangedRampage

initialGame :: Game
initialGame =
  { map: levelmap
  , players: M.fromList $ f <$> starts
  , items:   M.fromList $ zipNumbers $ makeItems levelmap safeZone bigDots
  , countdown: Just 90
  , rampage: Nothing
  , safeZone: safeZone
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
  safeZone = do
    x <- [7,8,9]
    y <- [6,7,8,9]
    return (tilePositionToBlock (Position {x:x, y:y}))
  bigDots = do
    x <- [3, tilesAlongSide - 4]
    y <- [3, tilesAlongSide - 4]
    return (tilePositionToBlock (Position {x:x, y:y}))


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
      eachPlayer eatOtherPlayers
      eachPlayer attemptRespawn
      decrementRampageCounter
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
    ok <- canMoveInDirection pId p dir
    when ok $ do
      changePosition pId $ moveInDirection dir (p ^. pPosition)
      changeNomIndex pId $ ((p ^. pNomIndex) + 1) % nomIndexMax

isRampage :: PlayerId -> GameUpdateM Boolean
isRampage pId =
  (\g -> maybe false (fst >>> (==) pId) g.rampage) <$> getGame

eatItems :: PlayerId -> Player -> GameUpdateM Unit
eatItems pId p = do
  g <- getGame
  let mItem = lookupItemByPosition (p ^. pPosition) g
  whenJust mItem $ \(Tuple iId item) -> do
    case item ^. iType of
      LittleDot -> do
        eat iId
        changeScore pId (p ^. pScore + 1)
      BigDot ->
        when (not (isJust g.rampage)) $ do
          eat iId
          changeScore pId (p ^. pScore + 5)
          startRampage pId

eatOtherPlayers :: PlayerId -> Player -> GameUpdateM Unit
eatOtherPlayers pId p = do
  ok <- isRampage pId
  when ok $ do
    eachPlayer $ \pId' p' ->
      when (pId' /= pId && not (isRespawning p')) $ do
        let q = quadrance (p ^. pPosition) (p' ^. pPosition)
        when (q <= minEatingQuadrance) $ do
          changeRespawnCounter pId' $ Just respawnLength
          takeHalfPoints pId p pId' p'
  where
  takeHalfPoints pId p pId' p' = do
    let delta = floor (p' ^. pScore / 2)
    changeScore pId $ p ^. pScore + delta
    changeScore pId' $ p' ^. pScore - delta

decrementRampageCounter :: GameUpdateM Unit
decrementRampageCounter = do
  g <- getGame
  whenJust g.rampage $ \(Tuple pId counter) ->
    changeRampage (Tuple pId <$> decrementOrNothing counter)

startRampage :: PlayerId -> GameUpdateM Unit
startRampage pId =
  changeRampage $ Just $ pId ~ rampageLength

checkForGameEnd :: GameUpdateM Unit
checkForGameEnd = do
  g <- getGame
  whenJust (checkEnded g) endGame

tryChangeDirection :: PlayerId -> Player -> Direction -> GameUpdateM Unit
tryChangeDirection pId p d = do
  ok <- canMoveInDirection pId p d
  when ok $ do
    changeDirection pId (Just d)
    changeIntendedDirection pId Nothing

canMoveInDirection :: PlayerId -> Player -> Direction -> GameUpdateM Boolean
canMoveInDirection pId p d =
  let newPosition = moveInDirection d (p ^. pPosition)
      destIsFree game  = isFree game.map newPosition
      immobilised game = isRespawning p || isFleeing game
      isFleeing game   = maybe false (\(Tuple pId' count) ->
                                pId' /= pId && count % 3 == 0) game.rampage
      both (Tuple x y) = x && y
  in  both <<< (destIsFree &&& (not <<< immobilised)) <$> getGame

isRespawning p =
  isJust $ p ^. pRespawnCounter

attemptRespawn :: PlayerId -> Player -> GameUpdateM Unit
attemptRespawn pId p = do
  g <- getGame
  whenJust (p ^. pRespawnCounter) $ \ctr -> do
    let ctr' = decrementOrNothing ctr
    changeRespawnCounter pId ctr'
    when (ctr' == Nothing) $ do
      let mPos = find (\pos -> isFree g.map pos) g.safeZone
      whenJust mPos $ changePosition pId

inSafeZone :: Game -> Position -> Boolean
inSafeZone g pos = pos `elem` g.safeZone

moveInDirection :: Direction -> Position -> Position
moveInDirection d p = add p (dirToPos d)

isFree :: LevelMap -> Position -> Boolean
isFree levelmap pos =
  let block = getBlockAt pos levelmap
  in  maybe false (not <<< isWall) block

makeItems :: LevelMap -> [Position] -> [Position] -> [Item]
makeItems levelmap safeZone bigDotPositions =
  littleDots <> bigDots
  where
  littleDots = do
    x <- r
    y <- r
    let pos = tilePositionToBlock (Position {x: x, y: y})
    if shouldHaveLittleDot pos
      then [Item { itemType: LittleDot, position: pos }]
      else []

  r = range 0 (tilesAlongSide - 1)

  shouldHaveLittleDot pos =
    isFree levelmap pos &&
      not (elem pos safeZone) &&
      not (elem pos bigDotPositions)

  bigDots =
    (\pos -> Item { itemType: BigDot, position: pos }) <$> bigDotPositions

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
lookupItemByPosition :: Position -> Game -> Maybe (Tuple ItemId Item)
lookupItemByPosition pos g =
  case filter (\i -> (i ^. _2 ^. iPosition) == pos) (M.toList g.items) of
      [i] -> Just i
      []  -> Nothing
      x   -> error $ "lookupItemByPosition: items stacked: " <> show x
