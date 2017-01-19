module Rules
    (
      GameData(Game)
    , boardData
    , movesData
    , OutcomeData(PlayerOneWin, PlayerTwoWin, Draw, Continue)
    , isSpace
    , isUnoccupied
    , isWin
    , isDraw
    , outcome
    , nextPlayer
    ) where

import Board
    (
      BoardData(Rows)
    , boardRows
    , boardSpaces
    )
import Moves
    (
      MovesData(GameState)
    , allMoves
    , playerMoves
    , lastPlayer
    )
import Data.List

data GameData = Game {
                  boardData :: BoardData
                , movesData :: MovesData
                } deriving (Show)

isSpace :: Int -> BoardData -> Bool
isSpace move board = elem move spaces
                   where spaces = boardSpaces board

isUnoccupied :: Int -> MovesData -> Bool
isUnoccupied move moves = notElem move gameState
                        where gameState = allMoves moves

isWin :: GameData -> Bool
isWin game = or [isPlayerOneWin, isPlayerTwoWin]
  where rows = boardRows $ boardData game
        playerOneMoves = playerMoves 0 (movesData game)
        playerTwoMoves = playerMoves 1 (movesData game)
        isPlayerOneWin = isPlayerWin 0 rows playerOneMoves
        isPlayerTwoWin = isPlayerWin 1 rows playerTwoMoves

isPlayerWin :: Int -> [[Int]] -> [Int] -> Bool
isPlayerWin player rows moves = do
    or $ map (\row -> intersect row moves == row) rows

isDraw :: GameData -> Bool
isDraw game = and [isFullBoard, not $ isWin game]
  where spaces = boardSpaces $ boardData game
        gameState = allMoves $ movesData game
        isFullBoard = spaces \\ gameState == []

data OutcomeData = PlayerOneWin
                 | PlayerTwoWin
                 | Draw
                 | Continue deriving (Show, Eq)

outcome :: GameData -> OutcomeData
outcome game
    | isWin game && (winner == Just 0) = PlayerOneWin
    | isWin game && (winner == Just 1) = PlayerTwoWin
    | isDraw game = Draw
    | otherwise = Continue
  where winner = lastPlayer $ movesData game

nextPlayer :: MovesData -> Int
nextPlayer moves
    | lastPlayer moves == Just 0 = 1
    | otherwise = 0
