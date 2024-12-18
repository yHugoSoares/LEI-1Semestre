module Sessao where
import Data.List (groupBy, partition, intercalate)
import GHC.CmmToAsm.AArch64.Instr (x0)
import GHC.Linker.Loader (uninitializedLoader)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

data RTree a = R a [RTree a]
    deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x xs) = map (x:) (concat a)
    where a = map paths xs

{-
unpaths :: Eq a => [[[a]]] -> RTree a
unpaths [[[x]]] = R x []
unpaths l =
  let x = head (head (head l))
      subTrees = map (\xs -> unpaths [xs]) (filter (\a -> head (head a) == x) (concat l))
  in R x subTrees
-}

porNiveis :: RTree a -> [a]
porNiveis (R x l) = x : porNiveisaux l

porNiveisaux :: [RTree a] -> [a]
porNiveisaux [] = []
porNiveisaux ((R x l):xs) = x : porNiveisaux (xs ++ l)

type Mat a = [[a]]
csv :: Show a => Mat a -> String
csv [] = ""
csv [[]] = ""
csv (x:xs) = tail(init (show x)) ++ "\n" ++ csv xs

unInOrder :: [a] -> [BTree a]
unInOrder []  = [Empty]
unInOrder [x] = [Node x Empty Empty]
unInOrder l   = (Node (head l) (unInOrder (tail l)) (unInOrderaux tail l)) : (Node (head (tail l)) (unInOrderaux (l)) (unInOrder l))

unInOrderaux :: [a] -> BTree a
unInOrderaux [x] = Node x Empty Empty
unInOrderaux l = Node (head l) (unInOrderaux tail l) (unInOrderaux tail (tail l))