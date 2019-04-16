module Maybbe where

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

listToMaybe :: [a] -> Maybe a
listToMaybe (x : xs)  = Just x
listToMaybe _         = Nothing