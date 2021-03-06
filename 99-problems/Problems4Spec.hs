import Problems4
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO()
main = hspec $ do
  describe "#myGCD" $ do
    it "does euclid on two ints" $ do
      (myGCD 54 72) `shouldBe` (18 :: Int)

  describe "#primes" $ do
    it "produces a list of primes up to n" $ do
      (primes 25) `shouldBe` [2,3,5,7,11,13,17,19,23]
  
  describe "#isPrime" $ do
    it "Checks whether a given number is prime" $ do
      3   `shouldSatisfy` isPrime
      101 `shouldSatisfy` isPrime
      6   `shouldSatisfy` not . isPrime
      100 `shouldSatisfy` not . isPrime

  describe "#coPrime" $ do
    it "Checks whether gcd(a,b) == 1" $ do
      (coPrime 15 8) `shouldBe` True
      (coPrime 15 9) `shouldBe` False

  describe "#eulerPhi" $ do
    it "Counts the number of numbers <=n coprime to n" $ do
      (eulerPhi 10) `shouldBe` 4
      (eulerPhi 20) `shouldBe` 8

  describe "#smallestFactor" $ do
    it "Finds the smallest prime factor of n" $ do
      (smallestFactor 15 ) `shouldBe` 3
      (smallestFactor 125) `shouldBe` 5
      (smallestFactor 25 ) `shouldBe` 5
      (smallestFactor 2  ) `shouldBe` 2
      (smallestFactor 3  ) `shouldBe` 3
      (smallestFactor 4  ) `shouldBe` 2
      (smallestFactor 5  ) `shouldBe` 5

  describe "#primeFactors" $ do
    it "Finds the prime factorisation of n" $ do
      (primeFactors (2)) `shouldBe` [2]
      (primeFactors (3)) `shouldBe` [3]
      (primeFactors (4)) `shouldBe` [2,2]
      (primeFactors (5)) `shouldBe` [5]
      (primeFactors (100)) `shouldBe` [2,2,5,5]
      (primeFactors (63) ) `shouldBe` [3,3,7]

  describe "#primeFactorisation" $ do
    it "Finds the prime factorisation of n (standard form)" $ do
      (primeFactorisation (2)) `shouldBe` [(2,1)]
      (primeFactorisation (3)) `shouldBe` [(3,1)]
      (primeFactorisation (4)) `shouldBe` [(2,2)]
      (primeFactorisation (5)) `shouldBe` [(5,1)]
      (primeFactorisation (100)) `shouldBe` [(2,2),(5,2)]
      (primeFactorisation (63) ) `shouldBe` [(3,2),(7,1)]

  describe "#eulerPhiPrimePower" $ do
    it "p^(k-1) (p-1)" $ do
      (eulerPhiPrimePower (2,1))  `shouldBe` 1
      (eulerPhiPrimePower (3,1))  `shouldBe` 2
      (eulerPhiPrimePower (2,2))  `shouldBe` 2
      (eulerPhiPrimePower (3,2))  `shouldBe` 6
      (eulerPhiPrimePower (3,3)) `shouldBe` 18
      (eulerPhiPrimePower (2,6)) `shouldBe` 32

  describe "#eulerPhiOther" $ do
    it "alternative formula" $ do
      (eulerPhiOther 10) `shouldBe` (eulerPhi 10)
      (eulerPhiOther 56) `shouldBe` (eulerPhi 56)
      (eulerPhiOther 73) `shouldBe` (eulerPhi 73)
      (eulerPhiOther 42) `shouldBe` (eulerPhi 42)
      (eulerPhiOther 7 ) `shouldBe` (eulerPhi 7 )
      (eulerPhiOther 62) `shouldBe` (eulerPhi 62)
      (eulerPhiOther 81) `shouldBe` (eulerPhi 81)

  describe "#primeRange" $ do 
    it "primes between two bounds" $ do
      (primeRange 20 10) `shouldBe` [11,13,17,19]

  describe "#goldbach" $ do
    it "returns the smallest p,q such that n=p+q" $ do
      goldbach 4  `shouldBe` (2,2)
      goldbach 10 `shouldBe` (3,7)
      goldbach 16 `shouldBe` (3,13)

  describe "#goldbachList" $ do
    it "all goldbach decompositions between two bounds" $ do
      goldbachList 11 20 `shouldBe` [(5,7),(3,11),(3,13),(5,13),(3,17)]
