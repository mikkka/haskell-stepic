module Coord where
data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

getCenter :: Double -> Coord Int -> Coord Double
getCenter r (Coord x y)  = Coord 
                              (r * (fromIntegral x) + 0.5 * r)  (r * (fromIntegral y) + 0.5 * r)

getCell :: Double -> Coord Double -> Coord Int
getCell r (Coord x y) = Coord 
                            (fromIntegral $ floor $ x / r) (fromIntegral $ floor $ y / r)