module UISpec where

import Test.Hspec
import UI 
import Mocks ( spyMock )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "displayMessage" $ do 
    it "prints to output" $ do 
      shouldBe (spyMock $ displayMessage "message") ["message"]
