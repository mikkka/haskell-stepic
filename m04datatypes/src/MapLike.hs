module MapLike where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup k list = case filter (\(x,y) -> k == x) (getListMap list) of
                    []      -> Nothing
                    (x:xs)  -> Just $ snd x
  insert k v list = ListMap ((k,v) : (getListMap $ delete k list))
  delete k list = ListMap (filter (\(x,y) -> k /= x) (getListMap list))





