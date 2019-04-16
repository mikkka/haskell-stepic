module Main where

import Data.Char
import Data.List

main :: IO ()
main = putStrLn "hello world"

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y xs = x : y : xs

nTimes:: a -> Int -> [a]
nTimes x n = helper n x []
  where
    helper 0 _ xs = xs
    helper n x xs = helper (n - 1) x (x : xs)

nTimes2 = flip replicate

sndHead = snd . head

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs

oddsOnly2 :: Integral a => [a] -> [a]
oddsOnly2 = filter odd

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = reverse (helper xs ys zs [])
    where
      helper [] [] [] sum    = sum
      helper xs ys zs sum    = helper xs' ys' zs' ((x + y + z) : sum)
        where
          (x, xs') = extrude xs
          (y, ys') = extrude ys
          (z, zs') = extrude zs
          extrude []     = (0,[])
          extrude (x:xs) = (x,xs)


sum3_2 :: Num a => [a] -> [a] -> [a] -> [a]
sum3_2 (a:as) (b:bs) (c:cs) = (a+b+c) : sum3 as bs cs
sum3_2 [] [] [] = []
sum3_2 a b c = sum3 (nonEmpty a) (nonEmpty b) (nonEmpty c) 
                where 
                  nonEmpty []    = [0]
                  nonEmpty xs    = xs


groupElems :: Eq a => [a] -> [[a]]
groupElems []      = []
groupElems [x]     = [[x]]
groupElems (x:xs)  = 
                      let 
                          yss = groupElems xs
                      in 
                          if x == head (head yss) then
                            (x : head yss) : (tail yss)
                          else
                            [x] : yss


readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter $ (\x -> p1 x || p2 x)

qsort :: Ord a => [a] -> [a]
qsort []        = []
qsort xs@(x:_)  = 
              let 
                lft = filter ( <  x) xs
                mdl = filter ( == x) xs
                rgt = filter ( >=  x) xs
              in
                qsort lft ++ mdl ++ qsort rgt


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x ->  [x * x, x * x * x])

perms :: [a] -> [[a]]
perms []        = [[]]
perms (x:xs)    = concatMap (\p -> inserts x p) (perms xs)
                where
                  inserts x []          = [[x]]
                  inserts x yss@(y:ys)  = (x:yss) : map (y:) (inserts x ys)


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
--max3 = zipWith3 (\a b c -> max a $ max b c)
max3 = zipWith3 ((max .) . max)

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

data Odd = Odd Integer deriving (Eq, Show)
instance Enum Odd where
  succ (Odd x)                            = Odd (x + 2)
  pred (Odd x)                            = Odd (x - 2)
  enumFrom (Odd x)                        = map Odd (filter odd [x..])
  enumFromTo (Odd x) (Odd m)              = map Odd (filter odd [x..m])
  enumFromThen (Odd x) (Odd n')           = map Odd [x,n'..]
  enumFromThenTo (Odd x) (Odd n') (Odd m) = map Odd [x,n'..m]
  toEnum x                                = Odd $ toInteger (x)
  fromEnum (Odd x)                        = fromInteger x

baseVal = 9900000000000000000
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
          test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests

coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change 0   = [[]]
change sum = [fst:rest | 
                  fst <- map fromInteger coins,
                  sum - fst >= 0,
                  rest <- change (sum - fst)
              ] 


concatList :: [[a]] -> [a]
concatList = foldr (++) []

              
lengthList :: [a] -> Int
lengthList = foldr f 0
      where f x s = s + 1

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then s + x else s) 0

meanList :: [Double] -> Double
meanList xs = s / l
  where (s,l) = foldr (\x (s, l) -> (x + s, l+ 1)) (0, 0) xs

-- wtf??
evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\x ~(xs, ys) -> (x : ys, xs)) ([], [])
-- evenOnlyInf :: [a] -> [a]

-- dat works
evenOnlyRec :: [a] -> [a]
evenOnlyRec = oddStep
  where
    oddStep [] = []
    oddStep (x: xs) = evenStep xs
    evenStep [] = []
    evenStep (x: xs) = x : oddStep xs

evenOnlyComb :: [a] -> [a]    
evenOnlyComb xs = map fst $ filter (\(_,x) -> even x) (xs  `zip` [1..])

--

infCopyR xs = foldr(\x xs -> x : xs) [] xs


evenOnly2 :: [a] -> [a]
evenOnly2 = snd . foldr (\x p -> (x : (snd p), (fst p))) ([], [])

lastElem :: [a] -> a
lastElem = foldl1 (\a b -> b)


revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\(x1, x2) -> if x2 < x1 then Nothing else Just(x2, (x1, pred x2)))