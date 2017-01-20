module UI
    (
      MonadUI
    , writeLine
    , readLine
    , displayMessage
    ) where

class Monad monad => MonadUI monad where
    writeLine :: String -> monad ()
    readLine :: monad String

instance MonadUI IO where
    writeLine = putStrLn
    readLine = getLine

displayMessage :: MonadUI monad => String -> monad () 
displayMessage message = writeLine message 
