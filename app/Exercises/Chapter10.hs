module Exercises.Chapter10 (runChapter10) where

import Camera
  ( defaultCamera,
    render,
  )
import Canvas (writeCanvas)
import Material (black, checkerPattern, defaultMaterial, gradientPattern, ringPattern, stripePattern, white)
import Shape (defaultPlane, defaultSphere)
import Transformation (rotationX, scaling, translation, viewTransform)
import Types
  ( Camera (camTransform, fov, hSize, vSize),
    Canvas,
    Color (Color),
    Material (..),
    Pattern (..),
    Point (Point),
    PointLight (PointLight),
    Shape (material, transform),
    Vec (Vec),
    World (lights, objects),
  )
import World (defaultWorld)

drawScene :: Canvas
drawScene = render camera world
  where
    world =
      defaultWorld
        { lights = [PointLight (Point (-10) 10 (-10)) (Color 1 1 1)],
          objects = [floor, middle, left, right]
        }
    camera =
      defaultCamera
        { hSize = 200,
          vSize = 150,
          fov = pi / 3,
          camTransform = viewTransform (Point 0 1.5 (-5)) (Point 0 1 0) (Vec 0 1 0)
        }
    floor =
      defaultPlane {material = defaultMaterial {pattern = stripePattern white (Color 1 0 0)}}
    middle =
      defaultSphere
        { transform = translation (-0.5) 1 0.5,
          material =
            defaultMaterial
              { pattern = (gradientPattern (Color 1 0 0) white) {patTransform = translation (-1) 0 0 * scaling 2 1 1},
                diffuse = 0.7,
                specular = 0.3
              }
        }
    left =
      defaultSphere
        { transform = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          material =
            defaultMaterial
              { pattern = (gradientPattern (Color 1 0 0) white) {patTransform = translation (-1) 0 0 * scaling 2 1 1},
                diffuse = 0.7,
                specular = 0.3
              }
        }
    right =
      defaultSphere
        { transform = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5,
          material =
            defaultMaterial
              { pattern = (gradientPattern (Color 1 0 0) white) {patTransform = translation (-1) 0 0 * scaling 2 1 1},
                diffuse = 0.7,
                specular = 0.3
              }
        }

runChapter10 :: IO ()
runChapter10 = writeCanvas "patterned-spheres.ppm" drawScene