module Exercises.Chapter2 where

import Exercises.Chapter1 ( Projectile (Projectile, position), Environment(..), tick)
import Canvas (Canvas, writeCanvas, initCanvas, canvasWidth, canvasHeight, setPixel)
import VecPoint (Vec(..), Point (..), vMult, normalize)
import Color ( Color(Color) )

{- Exercise: Putting it together -}
plotProjectile :: Projectile -> Environment -> Canvas -> Canvas
plotProjectile pr@(Projectile (Point x y z) vel) env canv
  | y <= 0 = canv
  | otherwise =
    let red = Color 1 0 0
        width = canvasWidth canv
        height = canvasHeight canv
        nextProjectile = tick pr env
        (Point newX newY _) = position nextProjectile
        nextCanvas =
          if (0 < round newX && round newX < width) && (0 < round newY && round newY < height)
            then setPixel red (round newX) (height - round newY) canv
            else canv
     in plotProjectile nextProjectile env nextCanvas

runChapter2 :: IO ()
runChapter2 = do
  let p = Projectile (Point 0 1 0) (11.25 `vMult` normalize (Vec 1 1.8 0))
      e = Environment (Vec 0 (-0.1) 0) (Vec (-0.01) 0 0)
      initialCanvas = initCanvas 900 550
  writeCanvas "Exercises/parabola.ppm" (plotProjectile p e initialCanvas)
