module Exercises.Chapter6 where

import Canvas (Canvas, canvasWidth, initCanvas, writeCanvas, setPixel)
import Color (Color (Color), cMult, toPPM)
import Data.Matrix (mapPos, getElem)
import Intersection (Intersection (..), hit, intersect)
import Light (PointLight (PointLight), lighting)
import Material (Material(..), defaultMaterial)
import Ray (Ray (Ray), position)
import Sphere (Sphere (..), normalAt, unitSphere)
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
              Just (Intersection t sphereHit) ->
                let point = position r t
                    normal = normalAt sphereHit point
                    eye = vNeg direction
                 in lighting material light point eye normal
              Nothing -> black
    )
    canvas
  where
    black = Color 0 0 0
    material = defaultMaterial { color = Color 1 0.2 1 }
    sphere = unitSphere { material = material }
    light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
    rayStart = Point 0 0 (-5)
    sphereCenter = Point 0 0 0
    wallZ = 10.0
    wallSize = 7.0 :: Float
    pixelSize = wallSize / fromIntegral (canvasWidth canvas)
    halfWall = wallSize / 2

main :: IO ()
main = do
  let canvas = drawSphere (initCanvas 100 100)
  writeCanvas "Exercises/sphere-shaded2.ppm" canvas