module TestGame where

import Data.Maybe
import Data.Array

import Types
import Game

gameAfterOneStep :: Game
gameAfterOneStep = stepGame (Input (Just Up)) initialGame

playerPosition :: Game -> Maybe Position
playerPosition g =
  case g.objects !! 0 of
    Just (GOPlayer (Player p)) -> Just p.position
    _ -> Nothing
