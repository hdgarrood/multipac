module Types where

import Data.Tuple

data Game = Game LevelMap [Item] [Ghost] Player 
data LevelMap = LevelMap [[Block]]

data Block = Wall | Empty
data ItemType = LittleDot | BigDot | Cherry
data Direction = Up | Down | Left | Right
data Colour = Red | Pink | Cyan | Orange
data GhostState = Normal | Vulnerable | Recovering

type Position = Tuple Int Int
data Item = Item ItemType Position 
data Ghost = Ghost GhostState Colour Position Direction
data Player = Player Position Direction
