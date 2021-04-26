module Exercises.Chapter6 where

import Canvas (canvasWidth, initCanvas, writeCanvas, setPixel)
import Color (cMult, toPPM)
import Data.Matrix (mapPos, getElem)
import Light (lighting)
import Material (defaultMaterial)
import Ray (position)
import VecPoint (normalize, pSub, vNeg)
import Shape (intersect, normalAt, defaultSphere )
import Intersection ( hit )
import Types
    ( Shape(material),
      Intersection(Intersection),
      Material(color),
      Point(Point),
      Ray(Ray),
      PointLight(PointLight),
      Color(Color),
      Canvas )

{- Putting it together -}
drawSphere :: Canvas -> Canvas
drawSphere canvas =
  mapPos
    ( \(y, x) _ ->
        let wallX = (pixelSize * fromIntegral x) - halfWall
            wallY = halfWall - (pixelSize * fromIntegral y)
            wallPosition = Point wallX wallY wallZ
            r@(Ray origin direction) = Ray rayStart (normalize (wallPosition `pSub` rayStart))
            xs = intersect sphere r
         in case hit xs of
              Just (Intersection t sphereHit) ->
                let point = Ray.position r t
                    normal = normalAt sphereHit point
                    eye = vNeg direction
                 in lighting material sphere light point eye normal False
              Nothing -> black
    )
    canvas
  where
    black = Color 0 0 0
    material = defaultMaterial { color = Color 1 0.2 1 }
    sphere = defaultSphere  { material = material }
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
  writeCanvas "Exercises/sphere-shaded.ppm" canvas