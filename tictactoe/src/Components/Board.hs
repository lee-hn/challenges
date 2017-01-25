module Components.Board
    (
      BoardData(Rows)
    , boardDimension
    , boardSpaces
    , boardRows
    ) where

import Data.List

data BoardData = Rows {
                   boardDimension :: Int
                 } deriving (Show)

boardSpaces :: BoardData -> [Int]
boardSpaces (Rows dim) = take spaces [0..]
                       where spaces = dim ^ 2

boardRows :: BoardData -> [[Int]]
boardRows (Rows dim)
    | dim == 0 = [[]]
    | otherwise = horizontals ++ verticals ++ diagonals
  where horizontals = take dim [take dim [n..] | n <- take dim [0, dim..]]
        verticals = transpose horizontals
        diagonals = [take dim [0, dim + 1..],
                     take dim [dim - 1, 2 * (dim - 1)..]]
