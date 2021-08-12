module Exercises.Chapter14 where

import Constants ( defaultSphere, defaultCylinder, defaultGroup )
import Transformation
    ( rotationY, scaling, translation, rotationZ, viewTransform )
import Types
    ( Point(Point),
      Vec(Vec),
      Shape(minY, maxY, transform),
      Camera(hSize, vSize, fov, camTransform),
      Canvas,
      World(objects) )
import Shape ( addChildren )
import Camera ( defaultCamera, render )
import World ( defaultWorld )
import Canvas ( writeCanvas ) 

hexagonCorner :: Shape
hexagonCorner =
  defaultSphere {transform = translation 0 0 (-1) * scaling 0.25 0.25 0.25}

hexagonEdge :: Shape
hexagonEdge =
  let t = translation 0 0 (-1)
          * rotationY (- pi / 6)
          * rotationZ (- pi / 2)
          * scaling 0.25 1 0.25
   in defaultCylinder {transform = t, minY = 0, maxY = 1}

hexagonSide :: Shape
hexagonSide =
  fst (addChildren defaultGroup [hexagonCorner, hexagonEdge])

hexSide2 :: Shape
hexSide2 =
  hexagonSide {transform = rotationY (- pi / 3)}

hexTest :: Shape
hexTest = fst (addChildren defaultGroup [hexagonSide, hexSide2])

hexagon :: Shape
hexagon =
  let side n = hexagonSide {transform = rotationY (n * (pi / 3))}
      sides = map side [0 .. 5]
   in fst (addChildren defaultGroup sides)

drawScene :: Canvas
drawScene = render camera world
  where
    world = defaultWorld {objects = [hexTest]}
    camera =
      defaultCamera
        { hSize = 333,
          vSize = 250,
          fov = pi / 3,
          camTransform = viewTransform (Point (-2.6) 1.5 (-3.9)) (Point (-0.6) 1 (-0.8)) (Vec 0 1 0)
        }

runChapter14 = writeCanvas "hexagon.ppm" drawScene