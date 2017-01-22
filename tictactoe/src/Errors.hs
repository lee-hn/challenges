module Errors
  (
    ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError)
  ) where

data ErrorData = InvalidSpaceError
               | OccupiedSpaceError
               | NotANumberError
               deriving (Show, Eq, Enum)
