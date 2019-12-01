module Problems2 where
import Problems1
import Data.Maybe
data Compression = Multiple Int Char | Single Char deriving Show

encodeModified :: [Char] -> [Compression]
encodeModified [] = []
encodeModified xs = nextCompression:(encodeModified untakens)
    where (nextCompression,untakens) = fromJust (getNextCompression xs)

getNextCompression :: [Char] -> Maybe (Compression,[Char])
getNextCompression [] = Nothing
getNextCompression (x:xs)
    | (x_count == 1) = Just (Single x, xs)
    | otherwise = Just (Multiple x_count x, untakens)
      where x_count = length takens
            (takens,untakens) = takeWhileSplit (== x) (x:xs)

takeWhileSplit :: (a -> Bool) -> [a] -> ([a],[a])
takeWhileSplit f [] = ([],[])
takeWhileSplit f (x:xs) 
    | f x = (x:takens,untakens)
    | otherwise = ([],(x:xs))
      where (takens,untakens) = takeWhileSplit f xs

-- TODO: encodeModified requires fromJust, which breaks the purity. Is this avoidable?

decodeModified :: [Compression] -> [Char]
decodeModified [] = []
decodeModified (x:xs) = (decodeSingle x) ++ (decodeModified xs)

decodeSingle :: Compression -> [Char]
decodeSingle (Single x) = [x]
decodeSingle (Multiple n x) = map (\y -> x) [1..n]

-- TODO: quickcheck that decodeSingle . encodeSingle = id

-- dup every element
repli :: [a] -> [a]
repli [] = []
repli (x:xs) = (x:x:(repli xs))

--

dropEveryN :: Int -> [a] -> [a]
dropEveryN = dropEveryNcounter 1

dropEveryNcounter :: Int -> Int -> [a] -> [a]
dropEveryNcounter _ _ [] = []
dropEveryNcounter i n (x:xs) 
    | (n == i) = dropEveryNcounter 1 n xs
    | otherwise = x:(dropEveryNcounter (i+1) n xs)

--

splitListAt :: Int -> [a] -> ([a],[a])
splitListAt = splitListAtCounter 0 

splitListAtCounter :: Int -> Int -> [a] -> ([a],[a])
splitListAtCounter _ _ [] = ([],[])
splitListAtCounter i n (x:xs)
    | (i < n)   = (x:takens,untakens)
    | otherwise = ([], (x:xs))
      where (takens,untakens) = splitListAtCounter (i+1) n xs

-- includes ith and jth element
slice :: Int -> Int -> [a] -> [a]
slice = sliceWithCounter 0

sliceWithCounter :: Int -> Int -> Int -> [a] -> [a]
sliceWithCounter _ _ _ [] = []
sliceWithCounter i start end (x:xs)
    | i < start = rest
    | i > end   = []
    | otherwise = x:rest
      where rest = sliceWithCounter (i+1) start end xs
