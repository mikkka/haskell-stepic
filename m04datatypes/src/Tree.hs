module Tree where

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node lft rgt) = 1 + max (height lft) (height rgt)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node lft rgt) = 1 + (size lft) + (size rgt)


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x)       = (1, x)
    go (Node lft rgt) = 
      let
        (lc, ls) = go lft
        (rc, rs) = go rgt in (lc + rc, ls + rs)

