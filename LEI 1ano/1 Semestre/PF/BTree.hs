module Trees where

data Let = Let [Def] ExpInt
        | 

data Def = Def Char ExpInt
          deriving Show

data ExpInt = Const Int
            | Var   Char
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
            deriving Show

letEx :: Let
letEx = Let [Def 'a' (Const 4)
            ,Def 'b' (Mais (Const 5) (Var 'a'))
            ]
            ((Var 'a') 'Mult' (Var 'b'))

showLet :: Let -> String 
showLet (Let ds e) = "let " ++ showDefs ds ++ " in " ++ infixa e
showLet (Exp e)    = infixa e

showDefs :: [Def] -> String
showDefs ds = concat (map showDef ds)

showDef :: Def -> String
showDef (Def c e) = [c] ++ " = " ++ infixa e

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Var c)   = [c]

