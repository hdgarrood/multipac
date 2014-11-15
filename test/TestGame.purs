module TestGame where

import Data.Maybe
import Data.Array

import Types
import Game

gameAfterOneStep :: Game
gameAfterOneStep = stepGame (Input (Just Down)) initialGame
