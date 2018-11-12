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

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show,Read,Eq,Ord)



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a bs cs)
    | x == a = Node x bs cs
    | x <  a = Node a (treeInsert x bs) cs
    | x >  a = Node a bs (treeInsert x cs) 

treeSearch :: (Ord a) => a -> Tree a -> Bool
treeSearch x EmptyTree = False
treeSearch x (Node y left right)
    | x == y = True
    | x <  y = treeSearch x left
    | x >  y = treeSearch x right

data TrafficLight = Red | Yellow | Green 

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where 
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where 
    yesno :: a -> Bool 

instance YesNo Int where 
    yesno 0 = False
    yesno _ = True

instance YesNo (Tree a) where 
    yesno EmptyTree = False
    yesno _ = True

instance YesNo [a] where 
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance (YesNo a) => YesNo (Maybe a) where
    yesno Nothing = False 
    yesno (Just x) = yesno x 

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf condition if_ else_ = if (yesno condition) then if_ else else_

--class Functor f where 
--fmap :: (a -> b) -> f a -> f b

--instance Functor [] where
--fmap = map

--instance Functor Maybe where
--fmap Nothing = Nothing
--fmap Just x = Just (f x)
--
--instance Functor (Map k) where
--fmap = Map.map
--
--Kinds: The kind of a type is like a type of a type. Concrete types are given type *, and a type constructor F turning As into Bs has :k F = (:k A) -> (:k B). So :k Maybe = * -> * and :k Either = * -> * -> *
--
--Weird example: 

class Tofu t where 
  tofu :: j a -> t a j

--We deduce that a is a *, so j is a * -> *, so t is a * -> (* -> *) -> *
--
--Let's make a type with this kind: 
data Frank a b = Frank {frankField :: b a} deriving (Show) 
-- To see this has the required kind, note that Frank takes two type parameters. Also not that a and b a are concrete types, so that b must have kind * -> *
-- for instance:
-- :t Frank {frankField = Just "HAHA"} = Frank [Char] Maybe
-- :t Frank {frankField = Node 'a' EmptyTree EmptyTree} = Frank Char Tree
-- :t Frank {frankField = "YES"} = Frank Char []
-- So we can make Frank an instance of Tofu
instance Tofu Frank where 
  tofu = Frank

data Barry t k p = Barry {yabba :: p, dabba :: t k}
-- The kind of Barry is (* -> *) -> * -> * -> *
instance Functor (Barry a b) where
    fmap f (Barry {yabba=x,dabba=y}) = Barry{yabba=f x, dabba=y}


