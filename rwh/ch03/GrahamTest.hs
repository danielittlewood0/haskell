import Test.Hspec
import Test.QuickCheck
import Graham

main = hspec $ do 
  describe "#crossProduct" $ do 
    it "Given two points, computes their (scalar) cross product" $ do
      crossProduct (Vector (1, 0)) (Vector (0, 1)) `shouldBe` 1

    it "Anticommutative example" $ do
      crossProduct (Vector (0, 1)) (Vector (1, 0)) `shouldBe` -1

    it "Rotation invariant" $ do
      crossProduct (Vector (1, 1)) (Vector (-1, 1)) `shouldBe` 2

    it "Computes twice the signed triangle area" $ do
      crossProduct (Vector (1, 1)) (Vector (0, 3)) `shouldBe` 2 * 1.5

  describe "#dotProduct" $ do
    it "you know what the dot product does" $ do
      dotProduct (Vector (1,1)) (Vector (0,3)) `shouldBe` 3

  describe "#directionVector" $ do 
    it "gives the vector from first point to second" $ do
      directionVector (Point (1, 0)) (Point (0, 1)) `shouldBe` Vector (-1, 1)

    it "does nothing if the first point is (0,0)" $ do
      directionVector (Point (0, 0)) (Point (2, 3)) `shouldBe` Vector (2, 3)
      
  describe "#getDirection a b c" $ do
    it "returns Left if C is on the left of the line AB" $ do
      let [a,b,c] = map Point [(0,0), (1,0), (1,1)]
      getDirection a b c `shouldBe` Graham.Left

    it "returns Right if C is on the left of the line AB" $ do
      let [a,b,c] = map Point [(0,0), (1,1), (1,-1)]
      getDirection a b c `shouldBe` Graham.Right

    it "returns Colinear if ABC is a straight line" $ do
      let [a,b,c] = map Point [(1,1), (-1,2), (-3,3)]
      getDirection a b c `shouldBe` Graham.Colinear

  describe "#cyclicDirections" $ do
    it "Gives directions of points in threes" $ do
      let points = map Point [(0, 0), (1, 0), (1, 1), (-1, 1), (-1, 0), (0, 0)]
      cyclicDirections points `shouldBe` take 4 (repeat Graham.Left)

  describe "#compareYthenX" $ do
    it "Always compares by y components if different" $ do
      let p1 = Point (0,0)
      let p2 = Point (0,1)
      (p1 `compareYthenX` p2) `shouldBe` LT

    it "case of different x coords" $ do
      let p1 = Point (2,0)
      let p2 = Point (0,1)
      (p1 `compareYthenX` p2) `shouldBe` LT

    it "compares x coords if ys are eq" $ do
      let p1 = Point (2,1)
      let p2 = Point (0,1)
      (p1 `compareYthenX` p2) `shouldBe` GT

    it "eq iff same point" $ do
      let p1 = Point (2,1)
      let p2 = Point (2,1)
      (p1 `compareYthenX` p2) `shouldBe` EQ
  
  describe "#initialGrahamPoint" $ do
    it "finds the bottom-left point in a list" $ do
      let points = map Point [(0, 0), (1, 0), (1, 1), (-1, 1), (-1, 0), (0, 0)]
      initialGrahamPoint points `shouldBe` Point (-1,0)

    it "prefers bottom to left" $ do
      let points = map Point [(-1,0), (0,1), (1,0), (0,-1)]
      initialGrahamPoint points `shouldBe` Point (0,-1)

  describe "#compareByAngleFrom" $ do
    it "A less than B iff B on left of OA" $ do
      let o = Point (1,1)
      let a = Point (2,1)
      let b = Point (2,2)
      compareByAngleFrom o a b `shouldBe` LT

    it "A greater than B iff B on right of OA" $ do
      let o = Point (1,1)
      let a = Point (2,2)
      let b = Point (2,0)
      compareByAngleFrom o a b `shouldBe` GT

    it "equal iff colinear" $ do
      let o = Point (1,1)
      let a = Point (2,2)
      let b = Point (3,3)
      compareByAngleFrom o a b `shouldBe` EQ
