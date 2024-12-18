import Data.Maybe
import Control.Monad
import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Coordinates = (Int, Int)

data Player = Nought | Cross

data Board = Board
  { noughts :: [Coordinates]
  , crosses :: [Coordinates]
  , player  :: Player
  }

emptyBoard :: Board
emptyBoard = Board [] [] Nought

pushToken :: Coordinates -> Board -> Board
pushToken c b = case player b of
  Nought -> b { noughts = c : noughts b, player = Cross  }
  Cross  -> b { crosses = c : crosses b, player = Nought }

type Size = Float

resize :: Size -> Path -> Path
resize k = fmap (\ (x, y) -> (x * k, y * k))

drawNought :: Size -> Coordinates -> Picture
drawNought k (x, y) =
  let x' = k * fromIntegral x
      y' = k * fromIntegral y
  in color green $ translate x' y' $ thickCircle (0.1 * k) (0.3 * k)

drawCross :: Size -> Coordinates -> Picture
drawCross k (x, y) =
  let x' = k * fromIntegral x
      y' = k * fromIntegral y
  in color red $ translate x' y' $ Pictures
     $ fmap (polygon . resize k)
     [ [ (-0.35, -0.25), (-0.25, -0.35), (0.35,0.25), (0.25, 0.35) ]
     , [ (0.35, -0.25), (0.25, -0.35), (-0.35,0.25), (-0.25, 0.35) ]
     ]

drawBoard :: Size -> Board -> Picture
drawBoard k b = Pictures $ grid : ns ++ cs where

  ns = fmap (drawNought k) $ noughts b
  cs = fmap (drawCross k)  $ crosses b

  grid :: Picture
  grid = color black $ Pictures $ fmap (line . resize k)
       [ [(-1.5, -0.5), (1.5 , -0.5)]
       , [(-1.5, 0.5) , (1.5 , 0.5)]
       , [(-0.5, -1.5), (-0.5, 1.5)]
       , [(0.5 , -1.5), (0.5 , 1.5)]
       ]

checkCoordinate :: Size -> Float -> Maybe Int
checkCoordinate k f' =
  let f = f' / k
  in  (-1) <$ guard (-1.5 < f && f < -0.5)
  <|> 0    <$ guard (-0.5 < f && f < 0.5)
  <|> 1    <$ guard (0.5  < f && f < 1.5)

handleKeys :: Size -> Event -> Board -> Board
handleKeys k (EventKey (MouseButton LeftButton) Down _ (x', y')) b =
  fromMaybe b $ do
    x <- checkCoordinate k x'
    y <- checkCoordinate k y'
    return $ pushToken (x, y) b
handleKeys k _ b = b

main :: IO ()
main =
  let window = InWindow "Tic Tac Toe" (300, 300) (10, 10)
      size   = 100.0
  in play window white 1 emptyBoard (drawBoard size) (handleKeys size) (flip const)
