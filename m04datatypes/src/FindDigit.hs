module FindDigit where

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit xs = case filter isDigit xs of
                x : xs -> Just x
                _      -> Nothing 

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
                    Just x  -> x
                    Nothing -> 'X'
                    