module UI.UI
    (
      MonadUI
    , writeLine
    , readLine
    ) where

class Monad monad => MonadUI monad where
    writeLine :: String -> monad ()
    readLine :: monad String

