module ParsePerson where

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson xs = do 
                    kv <- toKV xs
                    firstName <- findFirstName kv
                    lastName <- findLastName kv
                    age <- findAge kv
                    return (Person firstName lastName age)
  where
    toKV :: String -> Either Error [(String,String)]
    toKV xs = foldr (\x acc -> case wordsWhen (== '=') x of
                                    x:y:[]                -> fmap (\s -> (x,y):s) acc 
                                    _                     -> Left ParsingError) (Right []) (wordsWhen (== '\n') xs)
    findFirstName :: [(String,String)] -> Either Error String
    findFirstName = findValueStr "firstName"
    findLastName :: [(String,String)] -> Either Error String
    findLastName = findValueStr "lastName"
    findAge :: [(String,String)] -> Either Error Int
    findAge kv = findValueStr "age" kv >>= (\s -> 
      case reads s :: [(Int,String)] of 
        [x] -> case x of 
                (a, "") -> Right a
                x       -> Left (IncorrectDataError s)
        x   -> Left (IncorrectDataError s))
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
    findValueStr ::  String -> [(String,String)] ->  Either Error String
    findValueStr sample kvs = case filter (\(k,v) -> isDatKey sample k) kvs of 
                                (x:xs) -> Right (dropWhile ( == ' ') (snd x))
                                _      -> Left IncompleteDataError
      where 
        isDatKey [] []          = True
        isDatKey (s:ss) []      = False
        isDatKey [] (k:ks)      = if ' ' == k then (isDatKey [] ks) else False
        isDatKey (s:ss) (k:ks)  = if s == k then (isDatKey ss ks) else False
