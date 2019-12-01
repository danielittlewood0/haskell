myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (x:xs)
    | f x       = x:(takeWhile f xs)
    | otherwise = []

myTakeWhile' f = foldr (takeIf f) []
    where takeIf f x acc = if ( f  x ) then ( x:acc ) else ( [] )
