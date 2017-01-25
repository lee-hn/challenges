module UI.Errors
  (
    ErrorData(InvalidSpaceError, OccupiedSpaceError, NotANumberError, InvalidResponseError)
  ) where

data ErrorData = InvalidSpaceError
               | OccupiedSpaceError
               | NotANumberError
               | InvalidResponseError
               deriving (Show, Eq, Enum)
