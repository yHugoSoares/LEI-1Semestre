module Aula12 where

data ArvExp = Const Int
            | Op Operador ArvExp ArvExp

type Operador = Char -- "+" , "-" , "*", "/"

e :: ArvExp
e = (Op '+' (Const 2) (Op '*' (Const 4) (Const 5)))

calc :: ArvExp -> Int
calc (Const x) = x
calc (Op '+' x y) = (calc x) + (calc y)
calc (Op '-' x y) = (calc x) - (calc y)
calc (Op '*' x y) = (calc x) * (calc y)
calc (Op '/' x y) = (calc x) `div` (calc y)

showExp :: ArvExp -> String
showExp (Const x) = show x
showExp (Op '+' x y) = "(" ++ showExp x ++ " + " ++ showExp y ++ ")"
showExp (Op '-' x y) = "(" ++ showExp x ++ " - " ++ showExp y ++ ")"
showExp (Op '*' x y) = "(" ++ showExp x ++ " * " ++ showExp y ++ ")"
showExp (Op '/' x y) = "(" ++ showExp x ++ " / " ++ showExp y ++ ")"

showExp' :: ArvExp -> String
showExp' e = showExp e ++ " = " ++ show (calc e)

instance Show ArvExp where
        show = showExp'

arvores :: String -> [ArvExp]
arvores l   | not (any isOP l) = [Const (read l)]
            | otherwise = [Op op e1 e2
                            | (s1,op,s2)    <- parte s
                            , e1            <- arvores s1
                            , e2            <- arvores s2
                            ]

isOP :: Char -> Bool
isOP o = elem o ("+-*/")showExp' (read l)

parte :: String -> [(String, Char, String)]
parte [] = []
parte s =   let (s1,s2) = break isOP s
            in case s2 of
                [] -> []
                (op:t) -> (s1,op,t)
                            [ (s1 ++ [op] ++ x , o , y )
                            , (x,o,y)   <- parte t
                            ] 

haSolucao :: String -> Int -> Maybe ArvExp
haSolucao x y = maybe x