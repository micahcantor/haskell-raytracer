module Exercises.Chapter6 where

import Canvas (Canvas, canvasWidth, initCanvas, writeCanvas)
import Color (Color (Color))
import Data.Matrix (mapPos)
import Intersection (Intersection (..), hit, intersect)
import Light (PointLight (PointLight), lighting)
import Material (defaultMaterial, setMaterialColor)
import Ray (Ray (Ray), position)
import Sphere (normalAt, setSphereMaterial, unitSphere)
import VecPoint (Point (Point), normalize, pSub, vNeg)

{- Putting it together -}
drawSphere :: Canvas -> Canvas
drawSphere canvas =
  mapPos
    ( \(y, x) a ->
        let wallX = (pixelSize * fromIntegral x) - halfWall
            wallY = halfWall - (pixelSize * fromIntegral y)
            wallPosition = Point wallX wallY wallZ
            r@(Ray origin direction) = Ray rayStart (normalize (wallPosition `pSub` rayStart))
            xs = intersect sphere r
         in case hit xs of
              Just (Intersection t s) ->
                let point = position r t
                    normal = normalAt s point
                    eye = vNeg direction
                 in lighting material light point eye normal
              Nothing -> black
    )
    canvas
  where
    red = Color 235 0 0
    black = Color 0 0 0
    material = setMaterialColor defaultMaterial (Color 1 0.2 1)
    sphere = setSphereMaterial unitSphere material
    light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
    rayStart = Point 0 0 (-5)
    sphereCenter = Point 0 0 0
    wallZ = 10.0
    wallSize = 7.0 :: Float
    pixelSize = wallSize / fromIntegral (canvasWidth canvas)
    halfWall = wallSize / 2

runChapter6 :: IO ()
runChapter6 = do
  let canvas = drawSphere (initCanvas 400 400)
  writeCanvas "Exercises/sphere-shaded.ppm" canvas
