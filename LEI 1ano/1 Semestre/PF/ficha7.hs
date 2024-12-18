module Ficha7 where

data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt
    | Mult ExpInt ExpInt
    deriving (Show,Eq)

l :: ExpInt
l = (Mais (Const 3) (Menos (Const 2) (Const 5)))

l2 = Const 3 `Mais` Const 4  `Mult` Const 5

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = -(calcula a)
calcula (Mult a b) = (calcula a) * (calcula b)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)

infixa :: ExpInt -> String
infixa (Const a) = show a
infixa (Simetrico a) = "-" ++ (infixa a)
infixa (Mais a b) = "(" ++ (infixa a) ++ " + " ++ (infixa b) ++ ")"
infixa (Menos a b) = "(" ++ (infixa a) ++ " - " ++ (infixa b) ++ ")"
infixa (Mult a b) = "(" ++ (infixa a) ++ " * " ++ (infixa b) ++ ")"

posfixa :: ExpInt -> String
posfixa (Const a) = show a
posfixa (Simetrico a) = (posfixa a) ++ "-"
posfixa (Mais a b ) = (posfixa a) ++ " " ++ (posfixa b) ++ " +"
posfixa (Menos a b) = (posfixa a) ++ " " ++ (posfixa b) ++ " -"
posfixa (Mult a b ) = (posfixa a) ++ " " ++ (posfixa b) ++ " *"

data RTree a = R a [RTree a]
    deriving (Show,Eq)
rt :: RTree Int
rt = R 3 [R 5 [R 1 [R 31 []]
             ,R 2 [R 12 [R 20 []]]
             ,R 3 []
             ,R 4 []]
    , R 7 [R 8 []]
    , R 4 []
    ]

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a (h:t)) = soma h + soma (R a t)

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R a (h:t))  = max (1 + altura h) (altura (R a t))

prune :: Int -> RTree a -> RTree a
prune _ (R a [])    = (R a [])
prune 0 _ = error "Impossivel"
prune 1 (R a _)     = (R a [])
prune n (R a s) = R a (map (prune (n-1)) s)

mirror :: Num a => RTree a -> RTree a
mirror (R a []) = R (-a) []
mirror (R a l) = R a (reverse (map mirror l))

postorder :: RTree a -> [a]
postorder (R a l) = concatMap postorder l ++ [a]