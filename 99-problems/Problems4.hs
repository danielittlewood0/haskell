module Problems4 where
import Problems1
import Data.List

myGCD :: Int -> Int -> Int
myGCD a b
    | b == 0  = a
    | a <  b  = myGCD b a
    | a == b  = a
    | a >  b  = myGCD (a-b) b

div_eq :: Int -> Int -> Bool
div_eq a b 
    | a `mod` b == 0 = True
    | b `mod` a == 0 = True
    | otherwise      = False

primes :: Int -> [Int]
primes n = nubBy div_eq [2..n] 

--primes :: Int -> [Int]
--primes n = filter (\i -> any (divides i) (primes k)) [2..n]
--    where k = (floor . sqrt . fromIntegral) n
--          divides i = (\j -> i `mod` j == 0)

isPrime :: Int -> Bool
isPrime n = n `elem` (primes n)
    where k = (floor . sqrt . fromIntegral) n
--
coPrime :: Int -> Int -> Bool
coPrime x y = (gcd x y) == 1
--
eulerPhi :: Int -> Int
eulerPhi n = length $ filter (coPrime n) [1..n]
--
smallestFactor n 
    |factors == [] = n
    |otherwise     = head factors
    where factors = filter (\m -> n `mod` m == 0) [2..k]
          k = (floor . sqrt . fromIntegral) n

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors 2 = [2]
primeFactors n = k:primeFactors (n `div` k)
    where k = smallestFactor n

primeFactorisation :: Int -> [(Int,Int)]
primeFactorisation n = map swap $ (count . pack . primeFactors) n
    where swap = (\(x,y) -> (y,x))

eulerPhiPrimePower :: (Int,Int) -> Int
eulerPhiPrimePower (p,k) = p^(k-1) * (p-1)

eulerPhiOther :: Int -> Int
eulerPhiOther n = foldr multiply 1 (primeFactorisation n)
    where multiply (p,k) x = x * eulerPhiPrimePower (p,k)

primeRange :: Int -> Int -> [Int]
primeRange upper lower = (primes upper) \\ (primes lower)

goldbach :: Int -> (Int,Int)
goldbach n = head [(p,n-p) | p <- ps, (n-p) `elem` ps]
    where ps = primes n

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList m n = map goldbach evens
    where evens = filter even [m..n]

