module ParsePerson2 where

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson xs = do 
                    kv <- toKV xs
                    firstName <- findValueStr "firstName" kv
                    lastName <- findValueStr "lastName" kv
                    age <- findAge kv
                    return (Person firstName lastName age)
  where
    toKV xs = let 
                dirtyParse = map (\(k,v) -> (reverse . dropWhile (== ' ') . reverse $ k, dropWhile (== ' ') . tail $ v)) . map (break (== '=')) . lines
                parsed = dirtyParse xs
              in if any (\s -> snd s == "") parsed then Left ParsingError else Right parsed
    findAge kv = findValueStr "age" kv >>= (\s -> 
      case reads s :: [(Int,String)] of 
        [x] -> case x of 
                (a, "") -> Right a
                _       -> Left (IncorrectDataError s)
        _   -> Left (IncorrectDataError s))
    findValueStr sample kvs = maybeToEither IncompleteDataError (lookup sample kvs)
    maybeToEither = flip maybe Right . Left


maybeToEither = ((flip maybe) Right) . Left

{-

maybe :: 
b -> (a -> b) -> Maybe a -> b

flip maybe :: 
(a -> b) -> b -> Maybe a -> b

Right :: 
b -> Either a b

(flip maybe) Right :: 
Either a1 a2 -> Maybe a2 -> Either a1 a2

a2 -> Either a1 a2
(a -> b) -> b -> Maybe a -> b

a :: a2
b :: Either a1 a2


Left :: 
a -> Either a b

a -> Either a b
     Either a1 a2 -> Maybe a2 -> Either a1 a2
-}