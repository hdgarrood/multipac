module Game where

import Prelude
import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array ((!!), range, filter, length, take)
import Data.List as List
import Data.Int (toNumber, fromNumber)
import Data.Map as M
import Data.Foldable
import Data.Profunctor.Strong ((&&&))
import Control.Arrow
import Control.Alt
import Control.Apply
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Lens (LensP(), Lens(), TraversalP(), lens, (.~), _1, _2)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~))
import Data.Lens.At (at)
import Data.Lens.Prism.Maybe (_Just)
import Math (ceil, floor, pi, (%))

import Types
import GenericMap
import LevelMap
import Utils


minPlayers = 2
rampageLength = 180
cooldownLength = 120
respawnLength = 60

-- minimum squared distance that a player needs to be from another player in
-- order to eat them, in blocks.
minEatingQuadrance = 4.0

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
      items <<< at iId %~ applyItemUpdate x
    ChangedCountdown x ->
      setCountdown x
    GameEnded _ ->
      id
    ChangedRampage r ->
      \g -> g { rampage = r }
  where
  setCountdown x game = game {countdown = x}

applyGameUpdates :: forall f. (Foldable f) => f GameUpdate -> Game -> Game
applyGameUpdates updates game = foldr applyGameUpdate game updates

removePlayer :: PlayerId -> Game -> Game
removePlayer pId = players <<< at pId .~ (Nothing :: Maybe Player)

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

changeScore :: PlayerId -> Int -> GameUpdateM Unit
changeScore pId s =
  applyGameUpdateM (GUPU pId (ChangedScore s))

changeCountdown :: Maybe Int -> GameUpdateM Unit
changeCountdown =
  applyGameUpdateM <<< ChangedCountdown

changeNomIndex :: PlayerId -> Int -> GameUpdateM Unit
changeNomIndex pId i =
  applyGameUpdateM (GUPU pId (ChangedNomIndex i))

eat :: ItemId -> GameUpdateM Unit
eat iId =
  applyGameUpdateM (GUIU iId Eaten)

changeRespawnCounter :: PlayerId -> Maybe Int -> GameUpdateM Unit
changeRespawnCounter pId x =
  applyGameUpdateM (GUPU pId (ChangedRespawnCounter x))

endGame :: GameEndReason -> GameUpdateM Unit
endGame =
  applyGameUpdateM <<< GameEnded

changeRampage :: Maybe Rampage -> GameUpdateM Unit
changeRampage =
  applyGameUpdateM <<< ChangedRampage

initialGame :: Game
initialGame =
  { map: levelmap
  , players: GenericMap $ f <$> starts
  , items:   GenericMap $ zipIndices $ makeItems levelmap safeZone bigDots
  , countdown: Just 90
  , rampage: Nothing
  , safeZone: safeZone
  }
  where
  levelmap = WrappedLevelMap basicMap2
  f (Tuple pId position) = Tuple pId (mkPlayer position)
  starts =
    [ P1 ~ tilePositionToBlock (Position {x:7.0, y:7.0})
    , P2 ~ tilePositionToBlock (Position {x:9.0, y:7.0})
    , P3 ~ tilePositionToBlock (Position {x:7.0, y:8.0})
    , P4 ~ tilePositionToBlock (Position {x:9.0, y:8.0})
    ]
  safeZone = do
    x <- map toNumber [7,8,9]
    y <- map toNumber [6,7,8,9]
    return (tilePositionToBlock (Position {x:x, y:y}))
  bigDots = do
    x <- map toNumber [3, tilesAlongSide - 4]
    y <- map toNumber [3, tilesAlongSide - 4]
    return (tilePositionToBlock (Position {x:x, y:y}))



stepGame :: Input -> Game -> Tuple Game (Array GameUpdate)
stepGame input game =
  execGameUpdateM game (handleInput input *> doLogic)

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
      changeNomIndex pId $ ((p ^. pNomIndex) + 1) `mod` nomIndexMax

isRampage :: PlayerId -> GameUpdateM Boolean
isRampage pId = do
  g <- getGame
  maybe (return false) isRampaging g.rampage
  where
  isRampaging rampage =
    return $ case rampage of
      Rampaging pId' _ -> pId' == pId
      Cooldown _       -> false

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
    let delta = p' ^. pScore / 2
    changeScore pId $ p ^. pScore + delta
    changeScore pId' $ p' ^. pScore - delta

decrementRampageCounter :: GameUpdateM Unit
decrementRampageCounter = do
  g <- getGame
  whenJust g.rampage $ \rampage ->
    changeRampage $
      case rampage of
        Rampaging pId ctr ->
          Just $ fromMaybe (Cooldown cooldownLength)
                           (Rampaging pId <$> decrementOrNothing ctr)
        Cooldown ctr ->
          Cooldown <$> decrementOrNothing ctr

startRampage :: PlayerId -> GameUpdateM Unit
startRampage pId =
  changeRampage $ Just $ Rampaging pId rampageLength

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
      isFleeing game   =
        case game.rampage of
          Just (Rampaging pId' count) -> pId' /= pId && count `mod` 3 == 0
          _ -> false
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
      changeDirection pId Nothing

inSafeZone :: Game -> Position -> Boolean
inSafeZone g pos = pos `elem` g.safeZone

moveInDirection :: Direction -> Position -> Position
moveInDirection d p = addPos p (dirToPos d)

isFree :: WrappedLevelMap -> Position -> Boolean
isFree (WrappedLevelMap levelmap) pos =
  let block = getBlockAt pos levelmap
  in  maybe false (not <<< isWall) block

makeItems :: WrappedLevelMap -> Array Position -> Array Position -> Array Item
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

  r = map toNumber $ range 0 (tilesAlongSide - 1)

  shouldHaveLittleDot pos =
    isFree levelmap pos &&
      not (elem pos safeZone) &&
      not (elem pos bigDotPositions)

  bigDots =
    (\pos -> Item { itemType: BigDot, position: pos }) <$> bigDotPositions

makeGame :: forall f. (Foldable f) => f PlayerId -> Game
makeGame pIds =
  let
    f = deleteWhere (\pId _ -> not (elem pId pIds))
  in
    initialGame # players %~ f

inCountdown :: Game -> Boolean
inCountdown g = isJust g.countdown

checkEnded :: Game -> Maybe GameEndReason
checkEnded g
  | M.isEmpty (g ^. items) && isNothing g.rampage       = Just Completed
  | List.length (M.values (g ^. players)) < minPlayers  = Just TooManyPlayersDisconnected
  | otherwise                                           = Nothing

isEnded = isJust <<< checkEnded

lookupItemByPosition :: Position -> Game -> Maybe (Tuple ItemId Item)
lookupItemByPosition pos g =
  case List.filter (\i -> (i ^. _2 ^. iPosition) == pos) (M.toList (g ^. items)) of
      List.Cons i List.Nil ->
        Just i
      List.Nil ->
        Nothing
      other ->
        unsafeThrow $ "lookupItemByPosition: items stacked: " <> show other
