module Ficha6 where

-- preorder  -> raiz ++ esquerda ++ direita
-- inorder   -> esquerda ++ raiz ++ direita
-- postorder -> esquerda ++ direita ++ raiz

data BTree a = Empty | Node a (BTree a) (BTree a)
            deriving Show

arv1 = Node 3 (Node 5 Empty Empty)
             (Node 9 Empty
                    (Node 1 Empty Empty)
             )
arv2 = Node 2 (Node 1 (Node 5 Empty Empty)
                               (Node 3 Empty Empty))
                   (Node 10 Empty
                               (Node 4 (Node 7 Empty Empty)
                                           Empty))

arv3 = Node 5 (Node 7 (Node 4 Empty Empty) Empty)
                (Node 2 (Node 6 Empty Empty) Empty)

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ left right) = 1 + max (altura left) (altura right)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ left right) = 1 + contaNodos left + contaNodos right

folhas :: BTree a -> Int
folhas (Node _ left right) = (folhas left) + (folhas right)
folhas Empty = 1

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 (Node _ left right) = Empty
prune n (Node x left right) = Node x (prune (n-1) left) (prune (n-1) right)

path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (h:t) (Node x left right)
    | h == True = x : path t right
    | h == False = x : path t left

morrer :: BTree a -> BTree a
morrer Empty = Empty
morrer (Node x left right) = (Node x (morrer right) (morrer left))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node x left1 right1) (Node y left2 right2) =
    Node (f x y) (zipWithBT f left1 left2) (zipWithBT f right1 right2)

--unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
--unzipBT (Node (a,b,c) left right) = (Node a left right, Node b left right, Node c left right)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving (Eq, Show)
data Classificacao = Aprov Int
                | Rep
                | Faltou
                deriving (Eq, Show)
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por n´umero)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (no,n,r,c) left right) 
    | r == TE = [(no,n)] ++ trabEst left ++ trabEst right
    | r /= TE = [] ++ trabEst left ++ trabEst right