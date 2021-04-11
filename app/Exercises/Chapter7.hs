module Exercises.Chapter7 where

import Camera
  ( Camera (fov, hSize, transform, vSize),
    defaultCamera,
    render,
  )
import Canvas (Canvas, writeCanvas)
import Color (Color (Color))
import Light (PointLight (PointLight))
import Material (Material (color, diffuse, specular), defaultMaterial)
import Sphere (Sphere (material, transformation), unitSphere)
import Transformation (rotationX, rotationY, scaling, translation, viewTransform)
import VecPoint (Point (Point), Vec (Vec))
import World (World (..), defaultWorld)

drawScene :: Canvas
drawScene =
  render camera world
  where
    world = 
      defaultWorld 
        { lights = [PointLight (Point (-10) 10 (-10)) (Color 1 1 1), PointLight (Point 10 10 (-10)) (Color 1 1 1)],
          objects = [floor, leftWall, rightWall, middle, left, right]}
    camera =
      defaultCamera
        { hSize = 700,
          vSize = 525,
          fov = pi / 3,
          transform = viewTransform (Point 0 1.5 (-5)) (Point 0 1 0) (Vec 0 1 0)
        }
    floor =
      unitSphere
        { transformation = scaling 10 0.01 10,
          material = defaultMaterial {color = Color 1 0.9 0.9, specular = 0}
        }
    leftWall =
      unitSphere
        { transformation = translation 0 0 5 * rotationY (- pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          material = material floor
        }
    rightWall =
      unitSphere
        { transformation = translation 0 0 5 * rotationY (pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          material = material floor
        }
    middle =
      unitSphere
        { transformation = translation (-0.5) 1 0.5,
          material = defaultMaterial {color = Color 0.1 1 0.5, diffuse = 0.7, specular = 0.3}
        }
    left =
      unitSphere
        { transformation = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          material = defaultMaterial {color = Color 1 0.8 0.1, diffuse = 0.7, specular = 0.3}
        }
    right = middle {transformation = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5}

runChapter7 :: IO ()
runChapter7 = writeCanvas "three-spheres-shadows.ppm" drawScene