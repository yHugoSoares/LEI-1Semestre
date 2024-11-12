import Data.List
{-
Apresente uma definição recursiva da função unlines :: [String] -> String} que junta todas as strings da lista numa só, separando-as pelo caracter '\n'. 
Por exemplo, unlines ["Prog", "Func"] == "Prog\nFunc"
-}
unlines' :: [String] -> String
unlines' [] = ""
unlines' [a] = a
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

{-
O formato *csv* (**c**omma **s**eparated **v**alues) serve para descrever tabelas de uma forma textual: cada linha da tabela corresponde a uma linha do texto, enquanto que os elementos de cada linha se encontram separados por vírgulas. 
Por exemplo, a string `"2,3,6,4\n12,3,12,4\n3,-4,5,7"` pode ser usada para descrever a matriz:
|  2  3  6  4 |
| 12  3 12  4 |
|  3 -4  5  7 |
-}
type Mat = [[Int]]

stringToMat :: String -> Mat
stringToMat s = map stringToVector (lines s)
    where
    stringToVector :: String -> [Int]
    stringToVector s = read ("[" ++ s ++ "]")

{-
(b) Defina a fun¸c˜ao transposta :: String -> String que recebe a tabela em formato tex-
tual e devolve a tabela transposta tamb´em em formato textual.
-}
transposta :: String -> String
transposta s = show (transpose (stringToMat s))

{-
3. Considere o seguinte tipo de dados para representar uma lista em que os elementos podem ser
acrescentados `a esquerda (Esq) ou `a direita (Dir) da lista. Nula representa a lista vazia.
data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula
(a) Defina a fun¸c˜ao semUltimo :: Lista a -> Lista a que recebe uma Lista n˜ao vazia e
devolve a Lista sem o seu elemento mais `a direita.
-}
data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula

semUltimo :: Lista a -> Lista a
semUltimo (Esq a Nula) = Nula
semUltimo (Esq a l) = Esq a (semUltimo l)
semUltimo (Dir l a) = l

{-
(b) Defina Lista como instˆancia da classe Show de forma a que a lista Esq 1 (Dir (Dir (Esq
9 Nula) 3) 4) seja apresentada como [1, 9, 3, 4].
-}
instance Show a => Show (Lista a) where
    show l = "[" ++ showAux l ++ "]"
        where
            showAux (Esq a Nula) = show a
            showAux (Dir Nula a) = show a
            showAux (Dir l a) = showAux l ++ "," ++ show a
            showAux (Esq a l) = show a ++ "," ++ showAux l
            showAux Nula = ""

{-
4. Relembre a defini¸c˜ao do tipo das ´arvores bin´arias e da fun¸c˜ao que faz uma travessia inorder de
uma ´arvore.
data BTree a = Empty | Node a (BTree a) (BTree a)
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r:inorder d)
(a) Defina uma fun¸c˜ao numera :: BTree a -> BTree (a,Int) que coloca em cada nodo da
´arvore argumento o n´umero de ordem desse nodo numa travessia inorder. A fun¸c˜ao deve
percorrer a ´arvore uma ´unica vez.
Por exemplo, numera (Node ’a’ (Node ’b’ Empty Empty) (Node ’c’ Empty Empty))
deve dar como resultado (Node (’a’,2) (Node (’b’,1) Empty Empty) (Node (’c’,3)
Empty Empty))
Sugest˜ao: Comece por definir a fun¸c˜ao numeraAux :: Int -> BTree a -> (Int,BTree
(a,Int)) que recebe um inteiro (o primeiro n´umero a ser usado) e retorna a ´arvore numerada
bem como o n´umero de elementos dessa ´arvore.
-}
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
bt :: BTree Char
bt =
    Node 'a'
        (Node 'b' Empty Empty)
        (Node 'c' Empty Empty)

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = inorder e ++ (r:inorder d)

numera :: BTree a -> BTree (a,Int)
numera = snd . numeraAux 0

numeraAux :: Int -> BTree a -> (Int,BTree (a,Int))
numeraAux _ Empty = (0, Empty)
numeraAux n (Node e l r) = (novo_n + n_dir, Node (e,novo_n) novo_l novo_r)
    where (n_esq, novo_l) = numeraAux n l
          novo_n = n_esq + n + 1
          (n_dir, novo_r) = numeraAux novo_n r