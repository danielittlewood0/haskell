module ReversePolishCalc where
import Data.List.Split
import Data.Char
data PolishStack = Polish {stack :: [Int]}

isStringNumber :: String -> Bool
isStringNumber = all isDigit 
 
readUntilSpace :: String -> (String,String)
readUntilSpace "" = ("","")
readUntilSpace (x:xs) 
    |x == ' ' = ("",xs)
    |otherwise = (x:ys,zs)
        where (ys,zs) = readUntilSpace xs

splitSpaces str 
    | xs == []  = [x]
    | x /= ""   = x:(splitSpaces xs)
    | otherwise = (splitSpaces xs)
    where (x,xs) = readUntilSpace str

parsePolishStep :: (Integral a,Read a) => String -> [a] -> [a]
parsePolishStep head stack
    |head == ""   = stack
    |head == "+"  = (a+b):cs
    |head == "-"  = (b-a):cs
    |head == "*"  = (a*b):cs
    |isStringNumber head = (read head):stack
        where (a:b:cs) = stack

parsePolish :: (Integral a,Read a) => String -> [a]
parsePolish string = foldl (flip parsePolishStep) [] (splitSpaces string)
