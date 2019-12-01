module Graham where
import Data.List

data Direction = Left | Right | Colinear deriving (Show, Eq)

data Vector a = Vector (a, a) deriving (Show, Eq)
data Point a = Point (a, a) deriving (Show, Eq)

crossProduct :: (Real a) => Vector a -> Vector a -> a
crossProduct (Vector (a,b)) (Vector (c,d)) = a * d - b * c

dotProduct :: (Real a) => Vector a -> Vector a -> a
dotProduct (Vector (a,b)) (Vector (x,y)) = a * x + b * y

directionVector :: (Num a) => Point a -> Point a -> Vector a
directionVector (Point (x1, y1)) (Point (x2, y2)) = Vector (x2 - x1, y2 - y1)

getDirection :: (Real a) => Point a -> Point a -> Point a -> Direction
getDirection a b c = case compare (crossProduct ab bc) 0 of
                             LT -> Graham.Right
                             EQ -> Colinear
                             GT -> Graham.Left
      where ab = directionVector a b
            bc = directionVector b c

cyclicDirections :: (Real a) => [Point a] -> [Direction]
cyclicDirections (a:b:c:points) = (getDirection a b c):(cyclicDirections (b:c:points))
cyclicDirections _ = []

compareYthenX :: (Real a) => Point a -> Point a -> Ordering
compareYthenX (Point (x,y)) (Point (a,b)) = case compare y b of
                                                  LT -> LT 
                                                  GT -> GT
                                                  EQ -> compare x a

initialGrahamPoint :: (Real a) => [Point a] -> Point a
initialGrahamPoint = minimumBy compareYthenX

compareByAngleFrom :: (Real a) => Point a -> Point a -> Point a -> Ordering
compareByAngleFrom origin a b = compare (crossProduct ob oa) 0
      where oa = directionVector origin a
            ob = directionVector origin b

sortByAngleFrom :: (Real a) => Point a -> [Point a] -> [Point a]
sortByAngleFrom x = sortBy (compareByAngleFrom x)

nextGrahamPoint :: (Real a) => Point a -> [Point a] -> Point a
nextGrahamPoint x xs = minimumBy (compareByAngleFrom x) (filter ((/=) x) xs)

grahamScanStep :: (Real a) => ([Point a], [Point a]) -> ([Point a], [Point a])
grahamScanStep (xs, []) = (xs, [])
grahamScanStep ([], ys) = grahamScanStep ([initialGrahamPoint ys], ys)
grahamScanStep (x:xs, ys) = 
    if (z `elem` xs) 
      then (x:xs, []) 
    else 
      grahamScanStep (z:x:xs, ys)
    where z = nextGrahamPoint x ys

grahamScan :: (Real a) => [Point a] -> [Point a]
grahamScan xs = (fst . grahamScanStep) ([], xs)
