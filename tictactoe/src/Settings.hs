{-# LANGUAGE GADTs #-}

module Settings
    (
      PlayerData(Players)
    , playerOne
    , playerTwo
    ) where

import Player ( PlayerClass )

data PlayerData one two where
    Players :: (PlayerClass one, PlayerClass two) => {
        playerOne :: one,
        playerTwo :: two 
    } -> PlayerData one two 
