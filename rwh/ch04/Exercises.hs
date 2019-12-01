toInt :: String -> Int
toInt = foldl (\acc -> \x -> 10*acc + (toDigit x)) 0

toDigit :: Char -> Int
toDigit x = case x of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9

-- myConcat = foldr (++) []

-- Data.List.groupBy f appears to group consecutive f-equal elements
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f [] = []
myGroupBy f ( x:xs )  = (x:( takeWhile (f x) (xs) )):(myGroupBy f (dropWhile (f x) xs ))

myGroupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy' f [] = []
myGroupBy' f ( x:xs )  = (x:head):(myGroupBy f tail)
    where (head,tail) = break (not . (f x)) xs

myGroupBy'' :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy'' f = foldr step []
    where step x [] = [[x]]
          step x (xs:acc) = if (f x (last xs)) then ((x:xs):acc) else ([x]:(xs:acc))

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr step False
    where step x = (||) (f x)

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr step True
    where step x = (&&) (f x)

words :: String -> [String]
words = foldr step [[]]
  where step x (a:acc) = if (x == ' ') then ([]:(a:acc)) else ((x:a):acc)

unlines :: [String] -> String
unlines xs = tail $ foldr step "" xs
  where step x acc = ('\n':x) ++ acc
