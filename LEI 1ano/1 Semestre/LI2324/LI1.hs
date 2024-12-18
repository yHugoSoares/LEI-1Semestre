module LI1 where
  
    import Test.HUnit

    -- x+2 = f(x+1) where f y = y+1
    -- x+1 = f(x+0) where f y = y+1
    -- x+0 = x

    mySum :: Int -> Int -> Int
    mySum x n 
        |   n == 0 = x
        |   n > 0 = mySum (x+1) (n-1)
        |   n < 0 = mySum (x-1) (n+1)
        {-
        |   n > 0 = let f y = y+1 in f(mySum x (n-1))
        |   n < 0 = let f y = y-1 in f(mySum x (n+1))
        -}
    testmySum = test ["x + 0 = x, para x = 4" ~: 4 ~=? mySum 4 0]


    myMult :: Int -> Int -> Int
    myMult x 1 = x
    myMult x 0 = 0
    myMult x n
        |   n > 0 = x + myMult(x) (n-1)

    myExp :: Int -> Int -> Int
    myExp x 1 = x
    myExp _ 0 = 1
    myExp x n
        |   n > 0 = x * myExp x (n-1)
    
    {-|
    Procura um elemento e devolve o seu associado, caso contrário 0
    = Exemplos: 
    >>> busca "Ana" [("Ana",2),("Joao",3)] = 2
    >>> busca "Ana" [("Ana",2),("Ana",10)] = 2 
    >>> busca "Ana" [("Joao",3)] = 0
    >>> busca "Ana" [] = 0
    -}
    busca :: Eq a => a -> [(a, Int)] -> Int
    busca _ [] = 0
    busca n ((x,y):t)
        |   n == x = y
        |   otherwise = busca n t

    testbusca = test ["Se pedir Ana quando a lista contem (Ana,2) = 2" ~: 2 ~=? busca "Ana" [("Ana",2),("Pedro",4)]]

    {-|
    Procura um elemento e devolve o seu associado, caso contrário 0
    = Exemplos: 
    >>> buscaTotal "Ana" [("Ana",2),("Joao",3)] = [2]
    >>> buscaTotal "Ana" [("Ana",2),("Ana",10)] = [2,10]
    >>> buscaTotal "Ana" [("Joao",3)] = []
    >>> buscaTotal "Ana" [] = []
    -}
    buscaTotal :: Eq a => a -> [(a,Int)] -> [Int]
    buscaTotal _ [] = []
    buscaTotal n ((x,y):t)
        |   n == x = y : buscaTotal n t
        |   otherwise = buscaTotal n t

    testbuscaTotal = test ["Se pedir Ana quando a lista contem [(Ana,2),(Ana,10)] = [2,10]" ~: [2,10] ~=? buscaTotal "Ana" [("Ana",2),("Ana",10)],
                            "Se pedir Ana quando a lista contem [(Ana,2),(Joao,4)] = [2]" ~: [2] ~=? buscaTotal "Ana" [("Ana",2),("Joao",4)],
                            "Se pedir Ana quando a lista contem [(Joana,2),(Joao,4)] = []" ~: [] ~=? buscaTotal "Ana" [("Joana",2),("Joao",4)] ]

    {-|
    Encontra a menor ordenada, caso contrario 0
    = Exemplos:
    >>> menor [] = 0
    >>> menor [('x',1),('x',3)] = 1
    -}
    menor :: Eq a => [(a,Int)] -> Int
    menor [] = 0
    menor [(x,y)] = y
    menor ((x1,y1):(x2,y2):t)
        |   y1 >= y2 = menor ((x2,y2):t)
        |   otherwise = menor ((x1,y1):t)

    testmenor = test ["Contendo uma lista [('x',1),('x',3)] ele verifica os numeros e ve que o menor é 1" ~: 1 ~=? menor [('x',1),('x',3)]]
