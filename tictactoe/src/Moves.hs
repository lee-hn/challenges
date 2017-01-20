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
lastMove (GameState moves) = Just $ last moves

addMove :: Int -> MovesData -> MovesData
addMove move (GameState moves) = (GameState $ moves ++ [move])

lastPlayer :: MovesData -> Maybe Int
lastPlayer (GameState moves)
    | length moves == 0 = Nothing
    | even $ length moves = Just 1
    | odd $ length moves = Just 0

playerMoves :: Int -> MovesData -> [Int]
playerMoves player (GameState moves)
    | player == 0 = [moves !! n | n <- moveIndices, even n]
    | player == 1 = [moves !! n | n <- moveIndices, odd n]
  where moveIndices = [0..length moves - 1]

