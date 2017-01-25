module Components.Minimax
    (
      scoreMove
    , possibleMoves
    , scorePossibleMoves
    , chooseBestMove
    ) where

import Components.Rules
    (
      GameData(Components)
    , boardData
    , movesData
    , isWin
    , isDraw
    , outcome
    , nextPlayer
    )
import Components.Moves
    (
      MovesData
    , allMoves
    , addMove
    , lastPlayer
    )
import Components.Board
    (
      BoardData
    , boardSpaces
    )
import Data.List
import System.Random

scoreMove :: Int -> GameData -> Int
scoreMove move game
    | isWin updatedGame = 1
    | isDraw updatedGame = 0
    | otherwise = do
        let moveScores = scorePossibleMoves updatedGame
        let bestMove = chooseBestMove moveScores
        negate $ snd bestMove
  where board = boardData game
        moves = movesData game
        updatedMoves = addMove move moves
        updatedGame = Components board updatedMoves

possibleMoves :: GameData -> [Int]
possibleMoves game
    | isWin game = []
    | isDraw game = []
    | otherwise = sort $ spaces \\ moves
  where spaces = boardSpaces $ boardData game
        moves = allMoves $ movesData game

scorePossibleMoves :: GameData -> [(Int, Int)]
scorePossibleMoves game
    | length spaces == 9 = zip spaces (replicate 9 0)
    | otherwise = do
        let scores = map (\move -> scoreMove move game) spaces
        zip spaces scores
  where spaces = possibleMoves game

chooseBestMove :: [(Int, Int)] -> (Int, Int)
chooseBestMove moveScores
    | numBestMoves == 1 = head bestMoves
    | otherwise = pickRandomMove bestMoves
  where bestScore = maximum $ snd $ unzip moveScores
        bestMoves = [moveScore | moveScore <- moveScores,
                                 snd moveScore == bestScore]
        numBestMoves = length bestMoves

pickRandomMove :: [(Int, Int)] -> (Int, Int)
pickRandomMove moveScores = do
    let gen = mkStdGen 9
    let randomIndex = fst $ randomR (0, length moveScores - 1) gen
    moveScores !! randomIndex
