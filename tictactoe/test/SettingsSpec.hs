module SettingsSpec where

import Test.Hspec
import Settings
import Player ( PlayerTypeData(Computer, Human) )
import Moves ( MovesData(GameState) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "playerOne" $ do
    it "returns Computer if player one is computer" $ do
      let players = Players Computer Human
      shouldBe (playerOne players) Computer

    it "return Human if player one is human" $ do
      let players = Players Human Computer
      shouldBe (playerOne players) Human

  describe "playerTwo" $ do
    it "returns Computer if player two is computer" $ do
      let players = Players Human Computer
      shouldBe (playerTwo players) Computer

    it "return Human if player two is human" $ do
      let players = Players Computer Human
      shouldBe (playerTwo players) Human
