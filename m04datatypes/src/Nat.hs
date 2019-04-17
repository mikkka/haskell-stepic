module Nat where

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x    = x
add (Suc x) y = Suc (add x y)

mul :: Nat -> Nat -> Nat
mul Zero x          = Zero
mul (Suc x) y       = add y (mul x y)

fac :: Nat -> Nat
fac Zero        = Suc Zero
fac (Suc x)     = mul (Suc x) (fac x)
