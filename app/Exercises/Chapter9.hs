module Exercises.Chapter9 (runChapter9) where

import Camera
  ( defaultCamera,
    render,
  )
import Canvas (writeCanvas )
import Shape (defaultPlane, defaultSphere)
import World ( defaultWorld )
import Material
    ( defaultMaterial )
import Transformation ( scaling, translation, viewTransform )
import Types
    ( Shape(..),
      Material(color, diffuse, specular),
      Point(Point),
      Vec(Vec),
      World(lights, objects),
      PointLight(PointLight),
      Color(Color),
      Canvas,
      Camera(hSize, vSize, fov, camTransform) )

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
        { hSize = 800,
          vSize = 600,
          fov = pi / 3,
          camTransform = viewTransform (Point 0 1.5 (-5)) (Point 0 1 0) (Vec 0 1 0)
        }
    floor =
      defaultPlane
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

runChapter9 :: IO ()
runChapter9 = writeCanvas "spheres-on-plane.ppm" drawScene