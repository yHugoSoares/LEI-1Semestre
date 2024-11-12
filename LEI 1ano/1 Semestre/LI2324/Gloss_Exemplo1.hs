module Main2 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Estado = (Float, Float)

estadoInicial :: Estado
estadoInicial = (0,0)

desenhaEstado :: Estado -> Picture
desenhaEstado (x,y) = translate x y poligono
   where poligono :: Picture
         poligono = color blue $ polygon [(0,0), (25,0), (25,25), (0,25), (0,0)]

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (x,y) = (x, y+5)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (x,y) = (x, y-5)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (x,y) = (x-5, y)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,y) = (x+5, y)
reageEvento _ s = s  -- ignora qualquer outro evento

reageTempo :: Float -> Estado -> Estado
reageTempo n (x,y) = (x, y-0.3)   -- diminui o valor do y


fr:: Int
fr = 50

dm :: Display
dm = InWindow
       "Novo Jogo"  -- título da janela
       (400, 400)   -- dimensão da janela
       (200,200)    -- posição no ecrã

corFundo = (White 0.5) 

main :: IO ()
main = do play dm             -- janela onde irá decorrer o jogo
               corFundo       -- cor do fundo da janela
               fr             -- frame rate
               estadoInicial  -- define estado inicial do jogo
               desenhaEstado  -- desenha o estado do jogo
               reageEvento    -- reage a um evento
               reageTempo     -- reage ao passar do tempo