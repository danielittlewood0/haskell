import Test.Hspec
import ReversePolishCalc

main = hspec $ do 
  describe "#isStringNumber" $ do
    it "true iff all chars are digits" $ do
      "11234" `shouldSatisfy` isStringNumber
      "r1234" `shouldSatisfy` (not .isStringNumber)

  describe "#readUntilSpace" $ do
    it "returns (head,tail) until next space or end" $ do
      (readUntilSpace "Hello") `shouldBe` ("Hello","")
      (readUntilSpace "Hello World") `shouldBe` ("Hello","World")
      (readUntilSpace "") `shouldBe` ("","")

  describe "#splitSpaces" $ do
    it "splits string on spaces" $ do
      (splitSpaces "x") `shouldBe` ["x"]
      (splitSpaces "10 4 3 + 2 * -") `shouldBe` ["10","4","3","+","2","*","-"]
    
  describe "#parsePolishStep" $ do
    it "takes an instruction and alters the stack" $ do
      (parsePolishStep "+" [1,2] :: [Int]) `shouldBe` [3]
      (parsePolishStep "*" [3,1,2] :: [Int]) `shouldBe` [3,2]
      (parsePolishStep "100" [1,2] :: [Int]) `shouldBe` [100,1,2]
    
  describe "#parsePolish" $ do
    it "Gives you the final stack" $ do
      let string = "10 4 3 + 2 * -"
      (parsePolish "10 4 3 2") `shouldBe` [2,3,4,10]
      (parsePolishStep "10" []) `shouldBe` [10]
      (parsePolish "10") `shouldBe` [10]
      (parsePolish "10 2") `shouldBe` [2,10]
      (parsePolishStep "+" [10,2]) `shouldBe` [12]
      (parsePolish "10 2 +") `shouldBe` [12]
      (parsePolish string) `shouldBe` [-4]


  it "tail junk" $ do
    let (str,_) = readUntilSpace "Hello world"
    putStrLn $ str
