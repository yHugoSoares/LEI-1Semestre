module LI2 where

import Graphics.Gloss
    
procura :: Eq a => a -> [a] -> Int
procura x [] = -1
procura x (y:ys)    | x == y = 0
                        | procura x ys == -1 = -1
                        | otherwise = 1 + procura x ys
    
procura' x lst = case (lookup x (zip lst [0..])) of
        Nothing -> -1
        Just i -> i

subListas :: Int -> [a] -> [[a]]
subListas _ [] = []
subListas n l = [(take n l),(drop n l)]

concat' :: Eq a => [[a]] -> [a]