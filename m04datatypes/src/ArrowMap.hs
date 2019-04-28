module ArrowMap where

import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }
instance MapLike ArrowMap where
  empty = ArrowMap (\_ -> Nothing)
  
  lookup k map = (getArrowMap map) k
  
  insert k v map = 
    let fun = getArrowMap map in
      ArrowMap (\x -> if x == k then Just v else fun x)
  
  delete k map = 
    let fun = getArrowMap map in
      ArrowMap (\x -> if x == k then Nothing else fun x)
  
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)
  