module Ficha4 where
import Data.Char

digitAlpha :: String -> (String, String)
digitAlpha "" = ("", "")
digitAlpha (h:t)
    | isAlpha h = (h : alpha, nonAlpha)
    | otherwise = (alpha, h : nonAlpha)
  where
    (alpha, nonAlpha) = digitAlpha t

digitAlpha' :: String -> (String, String)
digitAlpha' l = (filter isAlpha l, filter isDigit l) 

digitAlpha'' :: String -> (String, String)
digitAlpha'' l = foldl (\ (a,b) x -> if (isAlpha x) then (x:a,b) 
                                     else if (isDigit x) then (a,x:b) 
                                     else (a,b)) ("","") l

nzp :: [Int] -> (Int,Int,Int)
nzp l = foldl (\ (a,b,c) x -> if (x < 0) then (a+1,b,c)
                                else if (x > 0) then (a,b,c+1)
                                else (a,b+1,c)) (0,0,0) l

type Polinomio = [Monomio]
type Monomio = (Float,Int)

grau :: Polinomio -> Int
grau l = maximum (map snd l)

grau' :: Polinomio -> Int
grau' l = foldl (\ x (a,b) -> if b > x then b else x) 0 l