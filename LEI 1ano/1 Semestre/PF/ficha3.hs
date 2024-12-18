{- |
Module : Ficha3
Copyright : Cleber
Modulo de exemplo de Ficha3 de LI1.
Contem funções recursivas.
-}

module Ficha3 where
    import Data.Char
    
    {- | 
    Factorial de um número "a" 
    = Exemplo
    >>> fact 5 = 120
    -}
    fact :: Int -> Int
    fact 0 = 1
    fact a = a* fact(a-1) 

    {- | 
    Adicionar n a uma lista 
    = Exemplo
    >>> maisN [1,2,3] 2 = [3,4,5]
    -}
    maisN :: [Int] -> Int -> [Int]
    maisN [] n = []
    maisN (x:xs) n = x+n : (maisN xs n)

    {- | 
    Recebe uma lista de Strings e remove todas as Strings iniciadas por um dado caracter.
    = Exemplo
    >>> remove ["ola","xixi","coco"] 'o' = ["xixi","coco"]
    -}
    remove :: [String] -> Char -> [String]
    remove [] c = []
    remove (x:xs) c = if comecaPor x c then remove xs c else x : remove xs c

    {- |
    Verifica se a string começa por uma certa letra
    = Exemplo
    >>> comecaPor "Jeronimo" j = True
    -}
    comecaPor :: String -> Char -> Bool
    comecaPor "" c = False
    comecaPor s c = head s  == c

    {- | 
    Função recursiva que recebe uma lista de pares de inteiros e adiciona um valor dado à primeira componente de cada par
    = Exemplo
    >>> maisPrimeira [(1,2),(3,5),(2,4)] 1 = [(2,2),(4,5),(3,4)]
    -}
    maisPrimeira :: [(Int,Int)] -> Int -> [(Int,Int)]
    maisPrimeira [] n = []
    maisPrimeira ((x,y):xs) n = (x+n , y) : (maisPrimeira xs n)

    {- | 
    Função recursiva para encontrar o maior valor na segunda componente de pares de inteiros
    = Exemplo
    >>> maiorSegunda [(1,2),(3,5),(2,4)] = 5
    -}
    maiorSegunda :: [(Int, Int)] -> Int
    maiorSegunda [(x, y)] = y
    maiorSegunda ((x, y):resto)
        | y > maiorResto = y 
        | otherwise = maiorResto 
        where
        maiorResto = maiorSegunda resto

    {- | 
    Função que recebe um dígito e calcula o próximo dígito 
    = Exemplo
    >>> proximoD '5' = '6'
    -}
    proximoD :: Char -> Char
    proximoD '9' = '0'
    proximoD c = chr (ord c + 1)

    {- |
    Função que recebe uma letra e substitui pela letra seguinte
    = Exemplo
    >>> proximoV 'g' = 'h'
    -}
    proximoL :: Char -> Char
    proximoL 'z' = 'a'
    proximoL l = chr (ord l + 1)

    {- |
    Função que recebe uma lista de letra e substitui cada uma delas pela letra seguinte
    = Exemplo
    >>> proximoV ['a','b','d'] = "bce"
    -}
    proximoLs :: [Char] -> [Char]
    proximoLs [] = []
    proximoLs (h:t) = proximoL h : proximoLs t

    type Nome = String
    type Coordenada = (Int, Int)
    data Movimento= N | S | E | W deriving (Show,Eq) -- norte, sul, este, oeste
    type Movimentos = [Movimento]
    data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)
    
    {- |
    Função que calcule a posição de uma pessoa depois de executar um movimento:
    = Exemplo
    >>> posicao (Pos "Hugo" (0,0)) N = (0,1)
    -}
    posicaoaux :: PosicaoPessoa -> Movimento -> PosicaoPessoa
    posicaoaux (Pos n (x,y)) N = Pos n (x , y+1)
    posicaoaux (Pos n (x,y)) S = Pos n (x , y-1)
    posicaoaux (Pos n (x,y)) E = Pos n (x+1 , y)
    posicaoaux (Pos n (x,y)) W = Pos n (x-1 , y)
    {- |
    Função que calcule a posição de uma pessoa depois de executar uma lista de movimentos:
    = Exemplo
    >>> posicao (Pos "Hugo" (0,0)) [N,N,N,E,E) = (2,3)
    -}
    posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
    posicao pos [] = pos
    posicao pos (h:t) = posicao(posicaoaux pos h) t

    {- |
    Lista de posições de pessoas, actualize essa lista depois de todas executarem um movimento dado
    = Exemplo
    >>> posicoesM [(Pos "Hugo" (0,0)), (Pos "Henrique" (1,1))] N = [Pos "Hugo" (0,1),Pos "Henrique" (1,2)]
    -}
    posicoesM :: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
    posicoesM [] m = []
    posicoesM (h:t) m = posicaoaux h m : posicoesM t m

    {- |
    Lista de posições de pessoas, actialize essa lista depois de todas as pessoas executarem uma mesma sequência de movimentos.
    = Exemplos
    >>> posicoesMs [(Pos "hugo" (0,0)), (Pos "henrique" (1,1))] [N,N,E,E] = [Pos "hugo" (2,2),Pos "henrique" (3,3)]
    -}
    posicoesMs :: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
    posicoesMs [] _ = []
    posicoesMs p [] = p
    posicoesMs l [m] = posicoesM l m
    posicoesMs l (m:ms) = posicoesMs(posicoesM l m) ms