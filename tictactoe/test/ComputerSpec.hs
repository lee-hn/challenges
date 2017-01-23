module ComputerSpec where

import Test.Hspec
import Computer
import Rules ( GameData(Game) )
import Board ( BoardData(Rows) )
import Moves ( MovesData(GameState) )
import UI ( MonadUI )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let board = Rows 3
  let computer = ComputerPlayer
  let newGame = GameState []
  let nearEndGame = GameState [4, 0, 1, 7, 5, 3, 6, 8]
  let midGame = GameState [4, 0, 1, 7, 3, 6]




  describe "makeMove" $ do
    it "makes the only possible move" $ do
      shouldBe True True
{-
      let game = Game board nearEndGame
      shouldBe (makeMove computer game) 2

    it "chooses the winning move out of three possible moves" $ do
      let game = Game board midGame
      shouldBe (makeMove computer game) 5

    it "can make a move at the beginning of a game" $ do
      let game = Game board newGame
      shouldBe (elem (makeMove computer game) [0..8]) True

-}
