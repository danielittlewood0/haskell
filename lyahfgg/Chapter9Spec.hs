import Test.Hspec
import Test.QuickCheck
import Control.Monad
import System.Random

main = hspec $ do 
  describe "#putStrLn" $ do
    it "prints a line to the terminal" $ do
      io <- putStrLn "Test passed!"
      io `shouldBe` ()  
      
  describe "#getLine" $ do
    it "Demands a line of input" $ do
      name <- getLine
      let name_ = name
      name `shouldBe` name_

  describe "#putStr" $ do
    it "doesn't leave a line break" $ do
      putStr "abra"
      putStr "ca" 
      putStr "dabra"

  describe "#putChar" $ do
    it "puts a single Char" $ do
      putChar 'a'
      putChar 'b'
      putChar 'c'

  describe "#print" $ do
    it "prints any showable object to terminal" $ do
      print True

  describe "getChar" $ do
    it "grabs a character from input" $ do
      pending

  describe "when" $ do 
    it "execute IO if boolean is true" $ do
      value <- (when (1 > 0) (return ()))   
      value `shouldBe` ()

  describe "sequence" $ do
    it "performs a list of IO actions in sequence and holds their outputs" $ do
      strings <- sequence [getLine,getLine,getLine]
      let a = head strings 
          b = head (tail strings) 
          c = head $ tail $ tail strings
      strings `shouldBe` [a,b,c]

  describe "mapM, mapM_" $ do
    it "maps an io over a list and keeps all results" $ do
      pending

  describe "#forever" $ do
    it "loops an I/O action forever" $ do
      pending

  describe "#forM" $ do
    it "flips arguments of mapM for chaining purposes" $ do
      pending

  describe "#getContents" $ do
    it "reads the contents of a file" $ do
      pending

  describe "Random" $ do
    describe "#random" $ do
      it "takes a generator and return a random, generator pair" $ do
        let g = mkStdGen 100
        ((fst (random g)) :: Int) `shouldBe` -3633736515773289454

    describe "#randomR" $ do
      it "generates a random value between two bounds" $ do
        let g = mkStdGen 100
            (xs,g') = (randomR (1,6) g) :: (Int,StdGen)
        (all (<= (6 :: Int)) xs) `shouldBe` True
--      xs `shouldSatisfy` (\xs -> all (<=6) xs )

