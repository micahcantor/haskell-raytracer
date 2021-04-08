module Exercises.Chapter4 where

import Canvas ( Canvas, initCanvas, canvasWidth, writeCanvas )
import VecPoint ( Point(Point) )
import Transformation
    ( mpMult, scaling, translation, rotationY, setPixels )
import Color ( Color(Color) )

{- Putting it together -}
plotClock :: Canvas -> Canvas
plotClock c =
  let white = Color 1 1 1
      twelveO'Clock = Point 0 0 1
      width = fromIntegral $ canvasWidth c
      rotate n = rotationY (n * (pi / 6))
      toOrigin = translation (width / 2) 0 (width / 2)
      radiusScale = scaling (3 / 8 * width) 0 (3 / 8 * width)
      calcPixel p n = (toOrigin * radiusScale * rotate n) `mpMult` p
      points = zipWith calcPixel (replicate 12 twelveO'Clock) [0 .. 11]
      pairs = [(round x, round z) | Point x _ z <- points]
   in setPixels white pairs c

runChapter4 :: IO ()
runChapter4 = writeCanvas "Exercises/clock.ppm" (plotClock $ initCanvas 200 200)