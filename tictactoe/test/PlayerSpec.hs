module PlayerSpec where

import Test.Hspec
import Player
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import Rules ( GameData(Game) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let newGame = GameState []
  let game = Game board newGame

  describe "makeMove" $ do 
    it "always returns 0 if player type is Mock" $ do 
      shouldBe (makeMove Mock game) 0
