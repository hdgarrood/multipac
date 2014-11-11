module Types where

import Control.Monad.Free
import Data.Maybe

type Game = {map :: LevelMap, objects :: [GameObject]}

type LevelMap = {blocks :: [[Block]]}
data Block = Wall | Empty

type Position = {x :: Number, y :: Number}
data GameObject = GOPlayer Player | GOItem Item

type Player
  = { position :: Position
    , currentDirection :: Maybe Direction
    , intendedDirection :: Maybe Direction
    }

type Item
  = { position :: Position
    , itemType :: ItemType
    }

data Direction = Up | Down | Left | Right
data ItemType = LittleDot | BigDot | Cherry

dirToPos :: Direction -> Position
dirToPos Up    = {x:  0, y: -1}
dirToPos Left  = {x: -1, y:  0}
dirToPos Right = {x:  1, y:  0}
dirToPos Down  = {x:  0, y:  1}

add :: Position -> Position -> Position
add p q = {x: p.x + q.x, y: p.y + q.y}

data Input = Input (Maybe Direction)

data GameUpdateF next
  = ChangedDirection (Maybe Direction) next
  | ChangedIntendedDirection (Maybe Direction) next
  | ChangedPosition Position next

instance functorGameUpdateF :: Functor GameUpdateF where
  (<$>) f (ChangedDirection d n) = ChangedDirection d (f n)
  (<$>) f (ChangedIntendedDirection d n) = ChangedIntendedDirection d (f n)
  (<$>) f (ChangedPosition p n)  = ChangedPosition p (f n)

type GameUpdate = Free GameUpdateF

changedDirection :: Maybe Direction -> GameUpdate Unit
changedDirection d = Free (ChangedDirection d (Pure unit))

changedIntendedDirection :: Maybe Direction -> GameUpdate Unit
changedIntendedDirection d = Free (ChangedIntendedDirection d (Pure unit))

changedPosition :: Position -> GameUpdate Unit
changedPosition p = Free (ChangedPosition p (Pure unit))
