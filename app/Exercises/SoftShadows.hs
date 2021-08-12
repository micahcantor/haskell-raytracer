module Exercises.SoftShadows where

import Constants ( defaultSphere, defaultPlane, defaultMaterial )
import Camera (defaultCamera, render)
import Canvas (writeCanvas)
import Light (areaLight)
import Transformation (scaling, translation, viewTransform)
import Types
  ( Camera (..),
    Canvas,
    Color (Color),
    Light (..),
    Material (..),
    Point (Point),
    Shape (..),
    Vec (Vec),
    World (lights, objects),
  )
import World (defaultWorld)

drawScene :: Canvas
drawScene = render camera world
  where
    world = defaultWorld {objects = [floor, s1, s2], lights = [light]}
    camera =
      defaultCamera
        { hSize = 1200,
          vSize = 480,
          fov = 0.7854,
          camTransform = viewTransform (Point (-3) 1 2.5) (Point 0 0.5 0) (Vec 0 1 0)
        }
    light =
      areaLight (Point (-1) 2 4) (Vec 2 0 0, 10) (Vec 0 2 0, 10) True (Color 1.5 1.5 1.5)
    floor =
      defaultPlane {material = defaultMaterial {ambient = 0.025, diffuse = 0.67, specular = 0}}
    s1 =
      defaultSphere
        { transform = translation (-0.25) 0.33 0 * scaling 0.33 0.33 0.33,
          material = defaultMaterial {color = Color 0.5 0.5 1, ambient = 0.1, specular = 0, diffuse = 0.6, reflective = 0.3}
        }
    s2 =
      s1
        { transform = translation 0.5 0.5 0 * scaling 0.5 0.5 0.5,
          material = (material s1) {color = Color 1 0 0}
        }

runSoftShadows = writeCanvas "soft-shadows.ppm" drawScene