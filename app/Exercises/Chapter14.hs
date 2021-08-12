module Exercises.Chapter14 where

import Camera (defaultCamera, render)
import Canvas (writeCanvas)
import Constants (defaultCylinder, defaultGroup, defaultMaterial, defaultSphere, white)
import Debug.Trace (traceShow)
import Shape (addChildren, updateTransform, updateMaterial)
import Transformation
  ( rotationY,
    rotationZ,
    scaling,
    translation,
    viewTransform,
  )
import Types
  ( Camera (camTransform, fov, hSize, vSize),
    Canvas,
    Color (..),
    Light (..),
    Material (..),
    Point (Point),
    Shape (..),
    Vec (Vec),
    World (objects),
    children,
    parent,
  )
import World (defaultWorld)

hexagonCorner :: Shape
hexagonCorner =
  defaultSphere {transform = translation 0 0 (-1) * scaling 0.25 0.25 0.25}

hexagonEdge :: Shape
hexagonEdge =
  let t =
        translation 0 0 (-1)
          * rotationY (- pi / 6)
          * rotationZ (- pi / 2)
          * scaling 0.25 1 0.25
   in defaultCylinder {transform = t, minY = 0, maxY = 1}

hexagonSide :: Shape
hexagonSide =
  fst (addChildren defaultGroup [hexagonCorner, hexagonEdge])

hexagon :: Shape
hexagon =
  let side n = updateTransform hexagonSide (rotationY (n * (pi / 3)))
      sides = map side [0 .. 5]
      mat = defaultMaterial {color = Color 1 0.2 0.4}
      hex = fst (addChildren defaultGroup sides)
   in updateMaterial hex mat

drawScene :: Canvas
drawScene = render camera world
  where
    world = defaultWorld {objects = [hexagon]}
    light = PointLight (Point 5 8 (-9)) white
    camera =
      defaultCamera
        { hSize = 800,
          vSize = 400,
          fov = 0.3,
          camTransform = viewTransform (Point 2 4 (-9)) (Point 0 0 0) (Vec 0 1 0)
        }

runChapter14 = writeCanvas "hexagon.ppm" drawScene