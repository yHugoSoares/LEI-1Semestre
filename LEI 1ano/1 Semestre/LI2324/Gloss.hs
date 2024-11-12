import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play 
    (InWindow "Bouncing Square" (500,500) (0,0))
    white
    10
    estadoInicial
    draw
    reageEvento
    reageTempo

data Direcao = Subir | Descer
type Estado = (Direcao, Float, Float)

estadoInicial :: Estado
estadoInicial = (Descer, 0, 0)

draw :: Estado -> Picture
draw (_, x,y) = Translate x y $ rectangleSolid 50 50 

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (d, x,y) = (d, x+10,y)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (d, x,y) = (d, x-10,y)
reageEvento _ (d, x,y) = (d, x,y)

reageTempo :: Float -> Estado -> Estado
reageTempo _ (Descer, x, y) =   let proximoYNormalmente = y-10
                                in if proximoYNormalmente < (-225) then 
                                    (Subir, x, y)
                                else 
                                    (Descer, x, proximoYNormalmente)
reageTempo _ (Subir, x, y) =    let proximoYNormalmente = y+10
                                in if proximoYNormalmente > 225 then
                                    (Descer, x, y)
                                else 
                                    (Subir, x, proximoYNormalmente)