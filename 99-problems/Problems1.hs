module Problems1 where 

data NestedList a = Elem a | List [NestedList a]

newLast :: [a] -> a
newLast (x:[]) = x
newLast (x:xs) = newLast xs

lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:y:xs) = lastButOne (y:xs)

eleAt :: Int -> [a] -> a
eleAt 1 (x:xs) = x
eleAt n (x:xs) = eleAt (n-1) xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == y) && (isPalindrome ys)
    where y:ys = myReverse xs

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ flatten (List xs)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress [x,y]
    |(x == y) = [x]
    |otherwise = [x,y]
compress (x:y:ys)
    |(x == y) = (compress (y:ys))
    |otherwise = x:(compress (y:ys))

packExpanded :: (Eq a) => [a] -> [[a]] -> [[a]]
packExpanded [] xs = xs
packExpanded (a:as) [[]] = packExpanded as [[a]]
packExpanded (a:as) ((x:xs):xss)
    |(a == x)  = packExpanded as ((a:(x:xs)):xss)
    |otherwise = packExpanded as ([a]:((x:xs):xss))

pack :: (Eq a) => [a] -> [[a]]
pack xs = reverse $ packExpanded xs [[]]

count :: (Eq a) => [[a]] -> [(Int,a)]
count xss = [(n,x) | (x:xs) <- xss, let n = 1 + length xs]

encode xs = count $ pack xs
