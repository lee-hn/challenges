module Errors
  (
    ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError, PlayerOrderError)
  ) where

data ErrorData = InvalidSpaceError
               | OccupiedSpaceError
               | NotANumberError
               | PlayerOrderError
               deriving (Show, Eq, Enum)
