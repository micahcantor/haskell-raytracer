module Exercises.Chapter12 where

import Camera (render, defaultCamera)
import Material (defaultMaterial, glass, white)
import Shape (defaultCube, defaultPlane)
import Transformation (viewTransform)
import Types
  ( Camera (transform, fov, hSize, vSize),
    Canvas,
    Color (Color),
    Material (color, reflective),
    Point (Point),
    PointLight (PointLight),
    Shape (cubeMaterial, material),
    Vec (Vec),
    World (lights, objects),
  )
import World (defaultWorld)
import Canvas (writeCanvas)

drawScene :: Canvas
drawScene = render camera world
  where
    world =
      defaultWorld
        { objects = [floor, redGlassCube]
        }
    camera =
      defaultCamera
        { hSize = 1000,
          vSize = 750,
          fov = pi / 3,
          camTransform = viewTransform (Point (-2.6) 1.5 (-3.9)) (Point (-0.6) 1 (-0.8)) (Vec 0 1 0)
        }
    floor =
      defaultPlane
        { material =
            defaultMaterial {reflective = 0.4}
        }
    redGlassCube =
      defaultCube
        { material =
            glass {color = Color 0.3 0 0}
        }

runChapter12 = writeCanvas "glass-cube.ppm" drawScene