module Ficha1 where
    import Data.Char

    perimetroi :: Float -> Float
    perimetroi r = 2*pi*r

    areai :: Float -> Float
    areai r = pi*r^2

    dist :: (Double, Double) -> (Double, Double) -> Double
    dist (a, b) (x, y) = sqrt((a-x)^2) + sqrt((b-y)^2)

    primUlt :: [Double] -> (Double, Double)
    primUlt as = (head(as),last(as))

    multiplo :: Int -> Int -> Bool
    multiplo m n
        |   mod m n == 0 = True
        |   otherwise = False

    nRaizes :: Int -> Int -> Int -> Int
    nRaizes a b c
        |   x > 0 = 2
        |   x == 0 = 1
        |   x < 0 = 0
            where
            x = (b^2)-4*a*c

    type Hora = (Int,Int)
    validar :: Hora -> Bool
    validar x = (fst x < 24) && (snd x < 60) && (fst x >= 0) && (snd x >= 0)

    comparacao :: Hora -> Hora -> Bool
    comparacao x y 
        |   (fst x) == (fst y) && (snd x) > (snd y) = True
        |   (fst x) > (fst y) = True
        |   otherwise = False

    convertmin :: Hora -> Int
    convertmin x = (fst x * 60) + snd x

    converthour :: Int -> Hora
    converthour x = (div x 60, mod x 60)

    diference :: Hora -> Hora -> Int
    diference x y = abs(convertmin x - convertmin y)

    adicionarmin :: Hora -> Int -> Hora
    adicionarmin x y = converthour(convertmin x + y)

    data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
    next :: Semaforo -> Semaforo
    next a
        |   a == Verde = Amarelo
        |   a == Amarelo = Vermelho
        |   a == Vermelho = Verde

    stop :: Semaforo -> Bool
    stop a
        |   a == Verde = False
        |   otherwise = True

    safe :: Semaforo -> Semaforo -> Bool
    safe a b
        |   a == Verde && b == Verde = False
        |   a == Verde && b == Amarelo = False
        |   a == Amarelo && b == Verde = False
        |   a == Amarelo && b == Amarelo = False
        |   otherwise = True

    data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
    posx :: Ponto -> Double
    posx (Cartesiano _ y) = abs y
    posx (Polar r _) = r

    posy :: Ponto -> Double
    posy (Cartesiano x _) = abs x
    posy (Polar r _) = r

    raio :: Ponto -> Double
    raio (Cartesiano x y) = sqrt(x^2+y^2)
    raio (Polar r _) = r

    angulo :: Ponto -> Double
    angulo (Cartesiano x y) = (atan2 y x)
    angulo (Polar _ y) = y

    convertcart :: Ponto -> Ponto
    convertcart (Polar r a) = (Cartesiano (r*cos(a)) (r*sin(a)) )
    convertcart (Cartesiano x y) = (Cartesiano x y)

    distponto :: Ponto -> Ponto -> Double    
    distponto (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt( (x1 - x2) ^2 + (y1 - y2) ^2 )
    distponto ponto1 ponto2 = distponto (convertcart ponto1) (convertcart ponto2)
 
    data Figura = Circulo Ponto Double
        |   Retangulo Ponto Ponto
        |   Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

    poligono :: Figura -> Bool
    poligono (Circulo a r) = False
    poligono _ = True

    vertices :: Figura -> [Ponto]
    vertices (Retangulo (Cartesiano p1 p2) (Cartesiano p3 p4)) = [(Cartesiano p1 p2), (Cartesiano p3 p4), (Cartesiano p1 p4), (Cartesiano p3 p2)]
    vertices (Retangulo p1 p2) = vertices (Retangulo (convertcart(p1))  (convertcart(p2)))
    vertices (Triangulo p1 p2 p3) = [convertcart(p1),convertcart(p2),convertcart(p3)]

    area :: Figura -> Double
    area (Triangulo p1 p2 p3) =
        let a = distponto p1 p2
            b = distponto p2 p3
            c = distponto p3 p1
            s = (a+b+c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
    
    area (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = abs ((x2 - x1) * (y2 - y1))  -- Área do retângulo
    area (Retangulo p1 p2) = area (Retangulo (convertcart p1) (convertcart p2))

    isLower' :: Char -> Bool
    isLower' c = c >= 'a' && c <= 'z'