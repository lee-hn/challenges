{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI
    (
      spyMock
    , displayMessage
    ) where

import Control.Monad.Writer

newtype Mock a = Mock (Writer [String] a)
               deriving (Functor, Applicative, Monad, MonadWriter [String])

spyMock :: Mock a -> [String]
spyMock (Mock w) = execWriter w

class Monad m => MonadUI m where
    writeLine :: String -> m ()
    readLine :: m String

instance MonadUI IO where
    writeLine = putStrLn
    readLine = getLine

instance MonadUI Mock where
    writeLine output = tell [output]
    readLine = return "input"

displayMessage :: MonadUI m => String -> m () 
displayMessage message = writeLine message 
