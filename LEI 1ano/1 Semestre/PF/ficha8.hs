module Ficha8 where
{-
Eq : ==
Ord : <= ou compare
Show : show
Num : (+) (-) (*)
Enum : toEnumfrom
Read : readPrec
-}

data Frac = F Integer Integer

normaliza :: Frac -> Frac
normaliza (F a 0) = error "Impossivel"
normaliza (F a b) = F (a `div` mdc a b) (b `div` mdc a b)

mdc :: Integer -> Integer -> Integer
mdc x 0 = abs x
mdc x y = mdc y (x `mod` y)

instance Eq Frac where
    (==) :: Frac -> Frac -> Bool
    f1 == f2 = normaliza f1 == normaliza f2

instance Ord Frac where
    (<=) :: Frac -> Frac -> Bool
    f1 <= f2 = normaliza f1 <= normaliza f2

instance Show Frac where
    show (F x y) = show x ++ "/" ++ show y

e = (Mais (Const 2) (Mult (Const 4) (Const 5)))

data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "-" ++ show a
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"