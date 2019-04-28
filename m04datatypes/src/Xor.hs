module Xor where

newtype Xor = Xor { getXor :: Bool }
  deriving (Eq,Show)

instance Semigroup Xor where
  (<>) = undefined

instance Monoid Xor where
  mempty = Xor False
  mappend (Xor False) (Xor False)  = Xor False
  mappend (Xor False) (Xor True)   = Xor True
  mappend (Xor True)  (Xor False)  = Xor True
  mappend (Xor True)  (Xor True)   = Xor False
