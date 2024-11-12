module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Estado = Estado {posicao :: (Float, Float)
                      , velocidade :: (Float, Float)
                      }

estadoInicial :: Estado
estadoInicial = Estado (0,0) (-25,250)

colideTopo :: Estado -> Bool
colideTopo e = snd (posicao e) >= 225

colideBaixo :: Estado -> Bool
colideBaixo e = snd (posicao e) <= (-225)

colideEsquerda :: Estado -> Bool
colideEsquerda e = fst (posicao e) <= (-225)

colideDireita :: Estado -> Bool
colideDireita e = fst (posicao e) >= 225

draw :: Estado -> Picture
draw e = Translate x y $ rectangleSolid 50 50
    where (x,y) = posicao e

actualizaEstado :: Float -> Estado -> Estado
actualizaEstado t e = Estado (x+dx, y+dy) (vx, vy)
    where 
        (x,y) = posicao e
        (vx,vy) = velocidade e
        dx = t * vx
        dy = t * vy

{-distanciaX :: Float -> (Float,Float) -> (Float,Float) -> (Float,Float)
distanciaX tempo (vx,vy) (x,y) = (x+dx , y)
    where dx = tempo * vx
-}

type Coordenada = (Float, Float)
type Rectangulo = (Coordenada, Coordenada)

interseta :: Rectangulo -> Rectangulo -> Bool
interseta (((x1,y1), (x2,y2)), ((x3,y3), (x4,y4))) = ((x1 >= x3) && (x1 <= x4) && (y1 >= y3) && (y1 <= y4)) || ((x2 >= x3) && (x2 <= x4) && (y2 >= y3) && (y2 <= y4))

reageTempo :: Float -> Estado -> Estado
reageTempo t e
    | colideTopo e' || colideBaixo e'       = Estado (x,y) (vx,-vy)
    | colideDireita e' || colideEsquerda e' = Estado (x,y) (-vx,vy)
    | otherwise = e'
    where
        e' = actualizaEstado t e
        (x,y) = posicao e'
        (vx,vy) = velocidade e'

main :: IO ()
main = play
    (InWindow "" (500,500) (0,0))
    white
    200
    estadoInicial
    draw
    reageEvento
    reageTempo

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) _ = estadoInicial
reageEvento _ e = e