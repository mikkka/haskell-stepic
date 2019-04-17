module Main where
      
main :: IO ()
main = putStrLn "hello world"

data Color = Red | Green | Blue  
instance Show Color where
  show Red    = "Red"
  show Green  = "Green"
  show Blue   = "Blue"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
  
stringToColor :: String -> Color
stringToColor "Red"   = Red
stringToColor "Green" = Green
stringToColor "Blue"  = Blue

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error     = EQ
cmp Error _         = GT
cmp _ Error         = LT
cmp Warning Warning = EQ
cmp Warning Info    = GT
cmp Info Warning    = LT
cmp Info Info       = EQ

data SomeData = Kek

data Result = Fail | Success
doSomeWork :: SomeData -> (Result,Int)
doSomeWork _ = (Fail, undefined)

processData :: SomeData -> String
processData x =  case doSomeWork x of
  (Fail,  code) -> "Fail: " ++ (show code)
  (Success,  _) -> "Success"

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Rectangle x y) = x * y

data Result' = Fail' Int | Success'

instance Show Result' where
    show (Fail' x) = "Fail: " ++ (show x)
    show Success'  = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case doSomeWork x of
  (Fail,  code) -> Fail' code
  (Success,  _) -> Success'

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b 
isSquare _ = False


data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

zToInt :: Z -> Int
zToInt (Z sign bits) = sign' * val
  where
    sign' = case sign of 
      Minus -> -1
      Plus  ->  1
    val = foldr (\x s -> s * 2 + case x of 
                                    Zero -> 0
                                    One -> 1) 0 bits

intToZ :: Int -> Z
intToZ int 
    | int == 0  = (Z Plus [])
    | int > 0   = (Z Plus $ toBits  int)
    | int < 0   = (Z Minus $ toBits $ -1 * int)
  where
    toBits 0 = []
    toBits x = let 
                    val = case x `mod` 2 of
                      0 -> Zero
                      1 -> One
                    divv = x `div` 2
                  in val : toBits divv

add :: Z -> Z -> Z
add x y = intToZ (zToInt x + zToInt y)

mul :: Z -> Z -> Z
mul x y = intToZ (zToInt x * zToInt y)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = Person {firstName = firstName p2, lastName = lastName p1, age = age p2}

abbrFirstName :: Person -> Person
abbrFirstName p@(Person (x:y:s) _ _) = p {firstName = x : "."}
abbrFirstName p = p


eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

data Coord a = Coord a !a

getX :: Coord a -> a
getX (Coord x _) = x

getY :: Coord a -> a
getY (Coord _ y) = y
