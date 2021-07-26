module Exercises.Chapter7 where

import Camera
  ( defaultCamera,
    render,
  )
import Canvas (writeCanvas)
import Material (defaultMaterial)
import Shape (defaultSphere)
import Transformation (rotationX, rotationY, scaling, translation, viewTransform)
import World (defaultWorld)
import Types
    ( Shape(..),
      Material(color, diffuse, specular),
      Point(Point),
      Vec(Vec),
      World(lights, objects),
      PointLight(PointLight),
      Color(Color),
      Canvas,
      Camera(hSize, vSize, fov, camTransform), getMaterial )

drawScene :: Canvas
drawScene = render camera world
  where
    world =
      defaultWorld
        { lights = [PointLight (Point (-10) 10 (-10)) (Color 1 1 1), PointLight (Point 10 10 (-10)) (Color 1 1 1)],
          objects = [floor, leftWall, rightWall, middle, left, right]
        }
    camera =
      defaultCamera
        { hSize = 800,
          vSize = 600,
          fov = pi / 3,
          camTransform = viewTransform (Point 0 1.5 (-5)) (Point 0 1 0) (Vec 0 1 0)
        }
    floor =
      defaultSphere
        { sphereTransform = scaling 10 0.01 10,
          sphereMaterial = defaultMaterial {color = Color 1 0.9 0.9, specular = 0}
        }
    leftWall =
      defaultSphere
        { sphereTransform = translation 0 0 5 * rotationY (- pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          sphereMaterial = getMaterial floor
        }
    rightWall =
      defaultSphere
        { sphereTransform = translation 0 0 5 * rotationY (pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          sphereMaterial = getMaterial floor
        }
    middle =
      defaultSphere
        { sphereTransform = translation (-0.5) 1 0.5,
          sphereMaterial = defaultMaterial {color = Color 0.1 1 0.5, diffuse = 0.7, specular = 0.3}
        }
    left =
      defaultSphere
        { sphereTransform = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          sphereMaterial = defaultMaterial {color = Color 1 0.8 0.1, diffuse = 0.7, specular = 0.3}
        }
    right = middle {sphereTransform = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5}

runChapter7 :: IO ()
runChapter7 = writeCanvas "three-spheres-shadows.ppm" drawScene