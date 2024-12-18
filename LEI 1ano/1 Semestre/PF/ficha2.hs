module Ficha2 where

    funA :: [Double] -> Double
    funA [] = 0
    funA (y:ys) = y^2 + (funA ys)

    funB :: [Int] -> [Int]
    funB [] = []
    funB (h:t) = if (mod h 2)==0 then h : (funB t)
    else (funB t)

    