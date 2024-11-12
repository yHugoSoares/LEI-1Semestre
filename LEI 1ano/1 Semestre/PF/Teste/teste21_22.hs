module Teste2122 where
import Prelude hiding (zip)
import Data.List hiding (zip)
import System.Random
import Control.Monad (replicateM)

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:xs)| x < y = x : preCrescente (y:xs)
                     | otherwise = [x]

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = maximum l - minimum l

type Mat a = [[a]]
soma :: Num a => Mat a -> Mat a -> Mat a
--soma [[1,2,3],[9,8,7]] [[4,5,6],[6,7,4]] = [[1+4,2+5,3+6],[9+6,8+7,7+4]]
soma [] [] = []
soma [[a]] [[b]] = [[a+b]]
soma (x:xs) (y:ys) = zipWith (+) x y : soma xs ys

type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

agendaExemplo :: Agenda
agendaExemplo =
  Nodo ("Charlie", [123456789])
    (Nodo ("Alice", [987654321]) Vazia
      (Nodo ("Bob", [111223344, 999999999]) Vazia Vazia)
    )
    (Nodo ("Eva", [555555555])
      (Nodo ("David", [999888777, 111223344]) Vazia Vazia)
      (Nodo ("Frank", [444556677]) Vazia Vazia)
    )

instance Show Agenda where
    show Vazia = ""
    show (Nodo (nome,telefones) left right) = show left ++ "Nome: " ++ nome ++ ", telefone(s): " ++ telefoneB telefones ++ "\n" ++ show right
        where
            telefoneB :: [Telefone] -> String
            telefoneB [] = ""
            telefoneB [a] = show a
            telefoneB (x:xs) = show x ++ "/" ++ telefoneB xs

randomSel :: Int -> [a] -> IO [a]
randomSel n _ | n <= 0 = return []
randomSel n l = do
    indices <- nub <$> replicateM n (randomRIO (0, length l - 1))
    let r = [l !! i | i <- indices]
    return r

organiza :: Eq a => [a] -> [(a,[Int])]
organiza [] = []
organiza l = organizaAux l 0
    where
        organizaAux :: Eq a => [a] -> Int -> [(a,[Int])]
        organizaAux [a] i = [(a,[i])]
        organizaAux (x:y:xs) i  | x == y = (x,[i,i+1]) : organizaAux (y:xs) (i+1)
                                | otherwise = (x,[i]) : organizaAux (y:xs) (i+1)

func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x > 10) l)

func' :: [[Int]] -> [Int]
func'  [ ]  = []
func' [[a]] = [a]
func' (x:xs) | sum x > 10 = x ++ func' xs
             | otherwise = func' xs

data RTree a = R a [RTree a]
type Dictionary = [ RTree (Char, Maybe String) ]

data LTree a = Tip a | Fork (LTree a) (LTree a)
lt :: LTree Char
lt = Fork (Tip 'a') (Fork (Tip 'b') (Tip 'c'))

dumpLT :: LTree a -> [(a, Int)]
dumpLT tree = dumpLT' tree 1

dumpLT' :: LTree a -> Int -> [(a, Int)]
dumpLT' (Tip x) level = [(x, level)]
dumpLT' (Fork left right) level = dumpLT' left (level + 1) ++ dumpLT' right (level + 1)

unDumpLT :: [(a, Int)] -> LTree a
unDumpLT [(x, _)] = Tip x
unDumpLT lst = Fork left right
  where
    level = snd (head lst)
    (left, right) = splitByLevel level lst

splitByLevel :: Int -> [(a, Int)] -> (LTree a, LTree a)
splitByLevel level lst = (unDumpLT leftList, unDumpLT rightList)
  where
    (leftList, rightList) = partition (\(_, 1) -> 1 == level) lst