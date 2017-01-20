{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mocks 
    (
      MockPlayerData(MockPlayer)
    , spyMock
    ) where

import Player
    (
      PlayerClass
    , makeMove
    )
import UI 
    ( 
      MonadUI
    , writeLine
    , readLine
    )
import Control.Monad.Writer

data MockPlayerData = MockPlayer
                    deriving (Show, Eq)

instance PlayerClass MockPlayerData where
    makeMove mock game = 0

newtype MockUI a = MockUI (Writer [String] a)
                 deriving (
                            Functor
                          , Applicative
                          , Monad
                          , MonadWriter [String]
                          )

spyMock :: MockUI a -> [String]
spyMock (MockUI a) = execWriter a

instance MonadUI MockUI where
    writeLine output = tell [output]
    readLine = return "input"
