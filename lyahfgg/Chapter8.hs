module Chapter8 where
import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * (r^2)
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

nudgePoint :: Point -> Float -> Float -> Point
nudgePoint (Point x y) a b = Point (x+a) (y+b)

nudge :: Shape -> Float -> Float -> Shape 
nudge (Circle pt r) a b = Circle qt r
    where qt = nudgePoint pt a b
nudge (Rectangle p1 p2) a b = Rectangle q1 q2
    where q1 = nudgePoint p1 a b
          q2 = nudgePoint p2 a b


data Person = Person { firstname :: String,
                       lastname :: String,
                       age :: Int,
                       height :: Float, 
                       phoneNumber :: String,
                       flavour :: String
                       } deriving (Show)
                      
data Vector a = Vector a a a deriving (Show,Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t 
(Vector a b c) `vplus` (Vector x y z) = Vector (x+a) (y+b) (z+c)

vectDot :: (Num t) => Vector t -> Vector t -> t
vectDot (Vector a b c) (Vector x y z) = (a*x) + (b*y) + (c*z)

scalarMult :: (Num t) => t -> Vector t -> Vector t 
scalarMult x (Vector a b c) = Vector (x*a) (x*b) (x*c)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 

data LockerState = Taken | Free deriving (Show,Eq)
type Code = String 
type LockerMap = Map.Map Int (LockerState,Code)

lockerLookup :: Int -> LockerMap -> Either String Code 
lockerLookup n map = 
    case (Map.lookup n map) of
        Nothing  -> Left $ "Locker " ++ (show n) ++ " doesn't exist!"
        Just (Taken,_) -> Left $ "Locker " ++ (show n) ++ " is taken!"
        Just (Free,code) -> Right code

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a bs cs)
    | x == a = Node x bs cs
    | x <  a = Node a (treeInsert x bs) cs
    | x >  a = Node a bs (treeInsert x cs) 
