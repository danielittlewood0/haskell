import Test.Hspec
import Test.QuickCheck
import Chapter8


main = hspec $ do 
  describe "Point" $ do
    describe "#show" $ do
      it "default" $ do
        (show (Point 0 1)) `shouldBe` "Point 0.0 1.0"

  describe "Shape" $ do
    describe "area" $ do 
      it "Computes the area of a circle" $ do
        (area $ Circle (Point 0 0) 1) `shouldBe` pi 
      it "Computes the area of a rectangle" $ do
        (area $ Rectangle (Point 0 1) (Point 1 0)) `shouldBe` 1
 
    describe "#show" $ do
      it "does the default thing" $ do 
        (show $ Circle (Point 0 0) 1) `shouldBe` "Circle (Point 0.0 0.0) 1.0"

  describe "Person" $ do
    describe "records" $ do
      it "has a first name" $ do
        let person = Person{firstname = "Daniel",
                            lastname = "Littlewood",
                            age = 23,
                            height = 170.0,
                            phoneNumber = "REDACTED",
                            flavour = "Mint"}
        (firstname person) `shouldBe` "Daniel"
        (lastname person) `shouldBe` "Littlewood"
        (age person) `shouldBe` 23
        (height person) `shouldBe` 170.0
        (phoneNumber person) `shouldBe` "REDACTED"
        (flavour person) `shouldBe` "Mint"

  describe "Vector" $ do
    describe "#vplus" $ do
      it "Adds two vectors" $ do
        let v = Vector 1 2 3
            u = Vector 6 5 4
        (u `vplus` v) `shouldBe` (Vector 7 7 7)

    describe "#vectDot" $ do
      it "Dot product of two vectors" $ do
        let v = Vector 1 2 3
            u = Vector 6 5 4
        (u `vectDot` v) `shouldBe` 28

    describe "#scalarMult" $ do
      it "Multiply a vector by a scalar" $ do
        let v = (Vector 1 2 3) :: Vector Int
        (scalarMult 2 v) `shouldBe` (Vector 2 4 6)

  describe "Day" $ do
    describe "deriving (Eq)" $ do
      it "checks whether the days are identical" $ do
        (Monday == Monday) `shouldBe` True
        (Monday == Tuesday) `shouldBe` False
        (Monday /= Tuesday) `shouldBe` True
    
      
    describe "deriving (Ord)" $ do
      it "Orders the days in the order defined" $ do
        (Monday < Tuesday) `shouldBe` True
        (Saturday > Sunday) `shouldBe` False

    describe "deriving (Show)" $ do
      it "Lets you print a day" $ do
        (show Monday) `shouldBe` "Monday"

    describe "deriving (Read)" $ do
      it "Reads the day from a string (provide type)" $ do
        (read "Monday" :: Day) `shouldBe` Monday

    describe "deriving (Bounded)" $ do
      it "First defined is min, last is max" $ do
        (minBound :: Day) `shouldBe` Monday
        (maxBound :: Day) `shouldBe` Sunday

    describe "deriving (Enum)" $ do
      it "Defines successor and predecessor of a day" $ do
        (succ Monday) `shouldBe` Tuesday
        (pred Sunday) `shouldBe` Saturday
        [Monday .. Sunday] `shouldBe` [
             Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

  describe "Locker thing" $ do
    it "Tells you if locker isn't there" $ do
      (lockerLookup 99 lockers) `shouldBe` Left "Locker 99 doesn't exist!"
    it "Tells you if locker is taken" $ do
      (lockerLookup 100 lockers) `shouldBe` Left "Locker 100 is taken!"
    it "Gives you the code otherwise" $ do
      (lockerLookup 101 lockers) `shouldBe` Right "JAH3I"

  describe "#treeSearch" $ do
    it "checks whether a binary search tree contains an element" $ do
      let nums = [8,6,4,1,7,3,5]  
      let numsTree = foldr treeInsert EmptyTree nums  
      (8 `treeSearch` numsTree) `shouldBe` True
      (9 `treeSearch` numsTree) `shouldBe` False

  describe "YesNo" $ do
    it "0 is falsy and 1 is truthy" $ do
      (yesno (0::Int)) `shouldBe` False
      (yesno (1::Int)) `shouldBe` True

    it "[] is falsy and ['a'] is truthy" $ do
      (yesno []) `shouldBe` False
      (yesno ['a']) `shouldBe` True

    it "Nothing is falsy and Just x depends on x" $ do
      (yesno (Nothing :: Maybe Int)) `shouldBe` False
      (yesno (Just [])) `shouldBe` False
      (yesno (Just (2 :: Int))) `shouldBe` True

    describe "#yesnoIf" $ do 
      it "evaluates a YesNo condition for if else" $ do
        let f x = yesnoIf (x :: Int) 1 0
        (yesno (1::Int)) `shouldBe` True
        (f 1) `shouldBe` 1
        (f 0) `shouldBe` 0

  describe "Functors" $ do
    describe "#fmap" $ do
      it "matches map for Lists" $ do
        (fmap (+2) [1,2,3]) `shouldBe` (map (+2) [1,2,3]) 

      it "maps over Maybe" $ do
        (fmap (+2) Nothing) `shouldBe` Nothing
        (fmap (+2) (Just 3)) `shouldBe` Just 5


