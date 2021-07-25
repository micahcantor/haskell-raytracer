module Exercises.Chapter11 where

import Camera
  ( defaultCamera,
    render,
  )
import Canvas (writeCanvas)
import Material (black, checkerPattern, defaultMaterial, gradientPattern, ringPattern, stripePattern, white)
import Shape (defaultPlane, defaultSphere, glassSphere)
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
        { lights = [PointLight (Point (-4.9) 4.9 (-1)) white],
          objects = [floor, blueGlassSphere, left]
        }
    camera =
      defaultCamera
        { hSize = 200,
          vSize = 150,
          fov = pi / 3,
          camTransform = viewTransform (Point (-2.6) 1.5 (-3.9)) (Point (-0.6) 1 (-0.8)) (Vec 0 1 0)
        }
    floor =
      defaultPlane
        { material =
            defaultMaterial
              { pattern = stripePattern (Color 0.35 0.35 0.35) (Color 0.65 0.65 0.65),
                reflective = 0.4,
                specular = 0
              }
        }
    blueGlassSphere =
      glassSphere
        { transform = translation 0.6 0.7 (-0.6) * scaling 0.7 0.7 0.7, 
          material =
            defaultMaterial
              { color = Color 0 0 0.2,
                ambient = 0,
                diffuse = 0.4,
                specular = 0.9,
                reflective = 0.9,
                transparency = 0.9,
                shininess = 300
              }
        }
    left =
      blueGlassSphere { transform = translation (-0.7 ) 0.5 (-0.8) * scaling 0.5 0.5 0.5 }

runChapter11 :: IO ()
runChapter11 = writeCanvas "reflection-spheres.ppm" drawScene