module Ficha6 where
import Control.Arrow (ArrowChoice(right))
import Data.Sequence (ViewR(EmptyR))

data BTree a = Empty
    | Node a (BTree a) (BTree a)
        deriving Show
bTree1 :: BTree Int
bTree1 = Node 1
    (Node 2
      (Node 4 Empty Empty)
      (Node 5 (Node 5 Empty Empty) Empty))
    (Node 3
      (Node 2 Empty Empty)
      (Node 6 Empty Empty))

bTree2 :: BTree Int
bTree2 =
  Node 50
    (Node 25
      (Node 10
        (Node 5 Empty Empty)
        (Node 15 Empty Empty))
      (Node 40
        (Node 30 Empty Empty)
        (Node 45 Empty Empty)))
    (Node 75
      (Node 60
        (Node 55 Empty Empty)
        (Node 70 Empty Empty))
      (Node 90
        (Node 80 Empty Empty)
        (Node 100 Empty Empty)))
-- a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ left right) = 1 + max (altura left) (altura right)

-- b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ left right) = 1 + contaNodos left + contaNodos right

-- c)
folhas :: BTree a -> Int
folhas Empty = 1
folhas (Node _ left right) = folhas left + folhas right

-- d)
prune :: Int -> BTree a -> BTree a
prune _ (Node a Empty Empty)= Node a Empty Empty
prune _ Empty = Empty
prune 1 (Node a _ _)        = Node a Empty Empty
prune n (Node a left right) = Node a (prune (n-1) left) (prune (n-1) right)

-- e)
path :: [Bool] -> BTree a -> [a]
path [] (Node a left right) = []
path _ Empty = []
path (h:t) (Node a left right) | h = a : path t right
                               | otherwise = a : path t left

-- f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a left right) = Node a (mirror right) (mirror left)

-- g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node a left1 right1) (Node b left2 right2) = Node (f a b) (zipWithBT f left1 left2) (zipWithBT f right1 right2)

-- h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) left right) = (Node a leftA rightA,Node b leftB rightB,Node c leftC rightC)
        where   (leftA, leftB, leftC) = unzipBT left
                (rightA, rightB, rightC) = unzipBT right

-- 2)
-- a)
minimo :: Ord a => BTree a -> a

minimo (Node a Empty Empty) = a
minimo (Node a left Empty) = a `min` minimo left
minimo (Node a Empty right) = a `min` minimo right
minimo (Node a left right) = a `min` (minimo left `min` minimo right)

-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node a Empty Empty) = Empty
semMinimo (Node a left right)   | mini == a = Empty
                                | otherwise = Node a (semMinimo left) (semMinimo right)
    where mini = minimo (Node a left right)

-- c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty Empty) = (a , Empty)
minSmin (Node a l Empty) = (a , Empty)
minSmin (Node a Empty r) = (a , r)
minSmin (Node a left right) = (min, Node a left' right)
  where (min,left') = minSmin left

-- d)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node a left right)
  | x < a = Node a (remove x left) right
  | x > a = Node a left (remove x right)
  | otherwise = removeNode (Node a left right)
  where
    removeNode :: Ord a => BTree a -> BTree a
    removeNode (Node _ Empty right) = right
    removeNode (Node _ left Empty) = left
    removeNode (Node _ left right) =
      let (minVal, newRight) = minSmin right
      in Node minVal left newRight

-- 3)
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
  | Rep
  | Faltou
    deriving Show

type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)
turma1 :: Turma
turma1 =
  Node (1, "Alice", ORD, Aprov 18)
    (Node (2, "Bob", TE, Rep)
      (Node (3, "Charlie", MEL, Aprov 20) Empty Empty)
      (Node (4, "David", TE, Aprov 15) Empty Empty))
    (Node (5, "Eve", ORD, Faltou)
      (Node (6, "Frank", MEL, Aprov 14) Empty Empty)
      (Node (7, "Grace", TE, Aprov 19) Empty Empty))

-- a)
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,no,r,c) Empty Empty) | n == num = True
                               | otherwise = False
inscNum n (Node (num,no,r,c) Empty right) | n == num = True
                              | n > num = inscNum n right
                              | n < num = False
inscNum n (Node (num,no,r,c) left Empty) | n == num = True
                              | n > num = False
                              | n < num = inscNum n left
inscNum n (Node (num,no,r,c) left right) | n == num = True
                              | n > num = inscNum n right
                              | n < num = inscNum n left

-- b)
inscNome :: Nome -> Turma -> Bool
inscNome nome (Node (_,no,_,_) left right) | nome == no = True
                                           | otherwise = inscNome nome left || inscNome nome right 