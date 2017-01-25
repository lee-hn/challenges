module UI.Console.Base
    (
      displayMessage
    , getUserInput
    , displayPrompt
    ) where

import UI.UI
    (
      MonadUI
    , writeLine
    , readLine
    )

instance MonadUI IO where
    writeLine = putStrLn
    readLine = getLine

displayMessage :: MonadUI monad => String -> monad ()
displayMessage message = writeLine $ message ++ "\n"

getUserInput :: MonadUI monad => monad String
getUserInput = readLine

displayPrompt :: MonadUI monad => String -> monad String
displayPrompt prompt = do
    displayMessage prompt
    getUserInput

