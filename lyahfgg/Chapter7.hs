module Chapter7 where
import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int 
numUniques = length . nub

isInitOf :: (Eq a) => [a] -> [a] -> Bool
isInitOf xs ys = xs `elem` (inits ys)

subList :: (Eq a) => [a] -> [a] -> Bool
subList xs ys = any (xs `isInitOf`) (tails ys)

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = (Data.List.foldr) (check) Nothing
    where check (k,v) acc
              |key == k  = Just v
              |otherwise = acc

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v

fromList' = foldr mergeTuple Map.empty
    where mergeTuple (x,y) = Map.insert x y
