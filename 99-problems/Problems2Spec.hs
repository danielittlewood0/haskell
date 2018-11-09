import Problems1
import Problems2
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
  describe "#myGCD" $ do
    it "does euclid on two ints" $ do
      (myGCD 54 72) `shouldBe` (18 :: Int)

