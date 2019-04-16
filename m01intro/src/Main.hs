module Main where

import Data.Char

main :: IO ()
main = do
  putStrLn "hello world"

lenVec3 x y z = sqrt $ x ^ 2 + y ^ 2 + z ^ 2

sign x = if x > 1 then 1 else (if x == 0 then 0 else -1)

infixl 6 *+*
x *+* y = x ^ 2 + y ^ 2

x |-| y = 
  if bet > 0 then bet else -bet
  where bet = x - y

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist (p1x, p1y) (p2x, p2y) = sqrt $ (p1x - p2x) ^ 2 + (p1y - p2y) ^ 2

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

doubleFact2 :: Integer -> Integer
doubleFact2 n | n <= 1 = 1
              | otherwise =  n * doubleFact (n - 2)


fibonacciBad :: Integer -> Integer
fibonacciBad 0     = 0
fibonacciBad 1     = 1
fibonacciBad (-1)  = 1
fibonacciBad n  | n > 1     = fibonacciBad (n - 1)  + fibonacciBad (n - 2)
                | n < (-1)  = (fibonacciBad (n + 2) - fibonacciBad (n + 1))


fibonacci :: Integer -> Integer
fibonacci n | n == 0  = 0
            | n > 0   = fibonacciHelper 1 0 (n - 1)
            | n < 0   = fibonacciHelper 1 0 (n + 1)

fibonacciHelper :: Integer -> Integer -> Integer -> Integer
fibonacciHelper acc prev 0            = acc
fibonacciHelper acc prev n | n > 0    = fibonacciHelper (prev + acc) acc (n - 1)
                           | n < 0    = fibonacciHelper (prev - acc) acc (n + 1)

seqA :: Integer -> Integer
seqA n 
    | n >= 0 = let
      helper k0 k1 k2 0     = k0
      helper k0 k1 k2 n     = helper k1 k2 (k2 + k1 - 2 * k0) (n - 1)
    in helper 1 2 3 n
  | otherwise = error "arg must be >= 0"


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0   = (0,1)
              | x > 0    = helper (0,0) x
              | x < 0    = helper (0,0) (-x)
      where
        helper acc 0    = acc
        helper (s, c) x = helper (s + x `mod` 10, c + 1) (x `div` 10)


sum'n'count2 :: Int -> (Int, Int)
sum'n'count2 x = (sum $ digits, length $ digits)
  where
    digits = (map digitToInt . show . abs) x

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = funky $ ranges
      where
        funky = sum . map (\(x1,x2) -> ((f x1 + f x2) * (x2 - x1)) / 2)
        ranges = zip pts (tail pts)
        delta = (b - a) / 10000
        pts = map (\x -> a + x * delta) [0..10000]


integration2 :: (Double -> Double) -> Double -> Double -> Double
integration2 f a b = foldl (\s e -> (let (x1, x2) = e in ((f x1 + f x2) * (x2 - x1)) / 2) + s) 0.0 ranges
  wherecd 
    delta = (b - a) / 10000
    pts = map (\x -> a + x * delta) [0..10000]
    ranges = zip pts (tail pts)
