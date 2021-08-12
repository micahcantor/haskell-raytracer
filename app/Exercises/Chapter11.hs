module Exercises.Chapter11 where

import Camera (defaultCamera, render)
import Canvas (writeCanvas)
import Constants
  ( defaultMaterial,
    defaultPlane,
    glass,
    glassSphere,
    white,
  )
import Material (stripePattern)
import Transformation (rotationX, scaling, translation, viewTransform)
import Types
  ( Camera (..),
    Canvas,
    Color (Color),
    Light (PointLight),
    Material (..),
    Pattern (..),
    Point (Point),
    Shape (..),
    Vec (Vec),
    World (lights, objects),
  )
import World (defaultWorld)

drawScene :: Canvas
drawScene = render camera world
  where
    world =
      defaultWorld
        { lights = [PointLight (Point (-4.9) 4.9 (-1)) white],
          objects = [floor, blueGlassSphere, left]
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
            defaultMaterial
              { pattern = Just $ stripePattern (Color 0.1 0.1 0.1) white,
                reflective = 0.4,
                specular = 0.1
              }
        }
    blueGlassSphere =
      glassSphere
        { transform = translation 0.6 0.7 (-0.6) * scaling 0.7 0.7 0.7,
          material = glass {color = Color 0 0 0.2}
        }
    left =
      blueGlassSphere {transform = translation (-0.7) 0.5 (-0.8) * scaling 0.5 0.5 0.5}

runChapter11 :: IO ()
runChapter11 = writeCanvas "reflection-spheres.ppm" drawScene