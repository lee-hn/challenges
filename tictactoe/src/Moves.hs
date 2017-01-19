module Moves
  (
    MovesData(GameState)
  , allMoves
  , lastMove
  , addMove
  , lastPlayer
  , playerMoves
  ) where

import Data.List

data MovesData = GameState {
                   allMoves :: [Int]
                 } deriving (Show, Eq)

lastMove :: MovesData -> Maybe Int
lastMove (GameState []) = Nothing
lastMove (GameState allMoves) = Just $ last allMoves

addMove :: Int -> MovesData -> MovesData
addMove move (GameState allMoves) = (GameState $ move : allMoves)

lastPlayer :: MovesData -> Maybe Int
lastPlayer (GameState allMoves)
    | length allMoves == 0 = Nothing
    | even $ length allMoves = Just 1
    | odd $ length allMoves = Just 0

playerMoves :: Int -> MovesData -> [Int]
playerMoves player (GameState allMoves)
    | player == 0 = [allMoves !! n | n <- moveIndices, even n]
    | player == 1 = [allMoves !! n | n <- moveIndices, odd n]
  where moveIndices = [0..length allMoves - 1]

