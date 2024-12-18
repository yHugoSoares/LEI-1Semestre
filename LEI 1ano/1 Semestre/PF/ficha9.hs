module Ficha9 where
import System.Random
{-
return :: a -> IO a
show :: a -> String
read :: String -> a

getChar -> lê um caracter
getline -> lê uma string/linha

putChar -> escreve caracter
putStr -> escreve string
putStrLn -> escreve uma string e muda de linha (\n)
print -> faz o equivalente a putStr + show

Random:
randomIO -> gera valor aleatorio
RandomRIO (a,b) -> gera um valor aleatorio entre a e b
-}

teste :: IO ()
teste = do
    putStr "Escreve um número...\n"
    x <- getChar
    num <- return (read [x] :: Int)
    y <- randomRIO(0,9 :: Int)
    if num == y then putStr ("\nAcertaste, o número: " ++ show y ++ "\n" )
    else putStr ("\nErraste o número, o gerado " ++ show y ++ "\n")

bingo :: IO ()
bingo = do
    putStrLn "O jogo do bingo vai começar!!"
    sorteio <- randomRIO(1,90 :: Int)
    putStrLn ("Número sorteado: " ++ show sorteio)
    proximoSorteio sorteio

proximoSorteio :: Int -> IO ()
proximoSorteio ultimoSorteio = do
    putStrLn "Deseja sortear mais um número? (S/N)"
    resposta <- getLine
    case resposta of
        "S" -> do
            novosorteio <- randomRIO (1,90 :: Int)
            if novosorteio /= ultimoSorteio
                then do 
                    putStrLn ("Número sorteado: " ++ show novosorteio)
                    proximoSorteio ultimoSorteio
                else do 
                    putStrLn "Número repetido! Sorteado novamente..."
                    proximoSorteio ultimoSorteio
        "N" -> putStrLn ("Obrigado por jogar!")
        _ -> do
            putStrLn ("Caracter inválido!\nEscreva 'S' para jogar novamente ou 'N' para parar de jogar")