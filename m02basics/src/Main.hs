module Main where
import Data.Function

main :: IO ()
main = putStrLn "hello world"

getSecondFrom :: a -> b -> c -> b
getSecondFrom _ x _ = x

multSecond = g01 `on` h01
g01 = (*)
h01 (_,x) = x

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


doItYourself = f . g . h

f = logBase 2
g = (^ 3)
h = max 42

{-
kek :: a -> (a,b) -> a -> (b,a,a)
kek a1 (a2,b) a3 = (b,a1,a2)
kek a1 (a2,b) a3 = (b,a2,a3)
kek a1 (a2,b) a3 = (b,a1,a3)
kek a1 (a2,b) a3 = (b,a2,a1)
kek a1 (a2,b) a3 = (b,a3,a2)
kek a1 (a2,b) a3 = (b,a3,a1)
kek a1 (a2,b) a3 = (b,a1,a1)
kek a1 (a2,b) a3 = (b,a2,a2)
kek a1 (a2,b) a3 = (b,a3,a3)
-}

swap :: (a,b) -> (b,a)
swap = uncurry (flip (,))


class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a | doesEnrageMork a && doesEnrageGork a = stomp (stab a)
                | doesEnrageMork a = stomp a
                | doesEnrageGork a = stab a
                | otherwise = a
                    
a = 127.2
b = 24.1
c = 20.1
d = 2
ip = show a ++ show b ++ show c ++ show d

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a | a == maxBound = minBound
          | otherwise     = succ a

  spred :: a -> a
  spred a | a == minBound = maxBound
          | otherwise     = pred a

avg :: Int -> Int -> Int -> Double
avg x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3

foo 0 x = x
foo n x = let x' = foo (n - 1) (x + 1)
          in x' `seq` x'

bar 0 f = f
bar x f = let f' = \a -> f (x + a)
              x' = x - 1
          in f' `seq` x' `seq` bar x' f'

baz 0 (x, y) = x + y
baz n (x, y) = let x' = x + 1
                   y' = y - 1
                   p  = (x', y')
                   n' = n - 1
               in p `seq` n' `seq` baz n' p

quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p