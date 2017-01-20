module SettingsSpec where

import Test.Hspec
import Settings
import Moves ( MovesData(GameState) )
import Computer ( ComputerPlayerData(ComputerPlayer) )
import Human ( HumanPlayerData(HumanPlayer) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let computer = ComputerPlayer
  let human = HumanPlayer

  describe "playerOne" $ do
    it "returns Computer if player one is computer" $ do
      let players = Players computer human 
      shouldBe (playerOne players) computer

    it "return Human if player one is human" $ do
      let players = Players human computer 
      shouldBe (playerOne players) human

  describe "playerTwo" $ do
    it "returns Computer if player two is computer" $ do
      let players = Players human computer 
      shouldBe (playerTwo players) computer

    it "return Human if player two is human" $ do
      let players = Players computer human 
      shouldBe (playerTwo players) human
