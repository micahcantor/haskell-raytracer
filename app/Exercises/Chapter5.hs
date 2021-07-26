module Exercises.Chapter5 where

import Data.Matrix ( mapPos )
import Types ( Point(Point), Ray(Ray), Color(Color), Canvas )
import Canvas ( initCanvas, canvasWidth, writeCanvas )
import VecPoint (pSub, normalize )
import Shape ( intersect, defaultSphere )
import Intersection ( hit ) 

{- Putting it together -}
drawSphereShadow :: Canvas -> Canvas
drawSphereShadow canvas =
  mapPos
    ( \(y, x) a ->
        let wallX = (pixelSize * fromIntegral x) - halfWall
            wallY = halfWall - (pixelSize * fromIntegral y)
            wallPosition = Point wallX wallY wallZ
            r = Ray rayStart (normalize (wallPosition `pSub` rayStart))
            xs = intersect defaultSphere r
         in case hit xs of
              Just _ -> red
              Nothing -> black
    )
    canvas
  where
    red = Color 1 0 0
    black = Color 0 0 0
    rayStart = Point 0 0 (-5)
    sphereCenter = Point 0 0 0
    wallZ = 10.0
    wallSize = 7.0
    pixelSize = wallSize / fromIntegral (canvasWidth canvas)
    halfWall = wallSize / 2

runChapter5 :: IO ()
runChapter5 = do
  let canvas = drawSphereShadow (initCanvas 400 400)
  writeCanvas "Exercises/sphere-shadow.ppm" canvas
