module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Coordenadas = (Float,Float)
type Velocidade = (Float,Float)
type Tempo = Float
type Estado = (Coordenadas, Velocidade, Tempo)


estadoInicial :: Estado
estadoInicial = ((0,0), (0.4, -0.8), 0)


desenhaEstado :: Estado -> Picture
desenhaEstado ((x,y), (vx,vy), t) =
    Pictures [translate x y $ figura, tempo]
    where
        figura = color c $ circleSolid 20
        c = if vy<0 then red else green
        tempo = translate 150 150 $ scale 0.1 0.1 $ Text (show $ floor t)


reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s -- ignora qualquer outro evento
-- reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x,y),v,t) = ...
-- ...

reageTempo :: Float -> Estado -> Estado
reageTempo n ((x,y),(vx,vy),t) = ((x+vx', y+vy'),(vx',vy'), t+n)
    where 
        vy' = if y+vy<=(-190) || y+vy>=190 then -vy else vy
        vx' = if x+vx<=(-190) || x+vx>=190 then -vx else vx

fr:: Int
fr = 50


dm :: Display
dm = InWindow "Jogo Exemplo" -- título da janela
    (400, 400) -- dimensão da janela
    (200,200) -- posição no ecran


corFundo :: Color
corFundo = (greyN 0.5)


main :: IO ()
main = play dm -- janela onde irá decorrer o jogo
    corFundo -- cor do fundo da janela
    fr -- frame rate
    estadoInicial -- define estado inicial do jogo
    desenhaEstado -- desenha o estado do jogo
    reageEvento -- reage a um evento
    reageTempo -- reage ao passar do tempo