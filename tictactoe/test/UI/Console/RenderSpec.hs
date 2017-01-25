{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module UI.Console.RenderSpec where

import Test.Hspec
import UI.Console.Render
import UI.UI ( MonadUI )
import Components.Board ( BoardData(Rows) )
import Components.Moves ( MovesData(GameState) )
import Components.Rules ( GameData(Components) )
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

mkFixture "Fixture" [''MonadUI]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let midGame = GameState [1, 3, 5, 7]

  describe "labelSpace" $ do
    it "represents unplayed space as its index" $ do
      let label = " 0 "
      let game = Components board midGame
      shouldBe (labelSpace 0 game) label

    it "represents move by player one with marker X" $ do
      let label = " X "
      let game = Components board midGame
      shouldBe (labelSpace 1 game) label

    it "represents move by player two with marker O" $ do
      let label = " O "
      let game = Components board midGame
      shouldBe (labelSpace 3 game) label

  describe "convertGameToString" $ do
    it "represents a new game on 3x3 board as string" $ do
      let gameString = " 0 | 1 | 2 \n\
                       \---+---+---\n\
                       \ 3 | 4 | 5 \n\
                       \---+---+---\n\
                       \ 6 | 7 | 8 "
      let game = Components board newGame
      shouldBe (convertGameToString game) gameString

    it "represents in-progress game on 3x3 board as string" $ do
      let gameString = " 0 | X | 2 \n\
                       \---+---+---\n\
                       \ O | 4 | X \n\
                       \---+---+---\n\
                       \ 6 | O | 8 "
      let game = Components board midGame
      shouldBe (convertGameToString game) gameString

