module Exercises.Chapter9 (runChapter9) where

import Camera
  ( defaultCamera,
    render,
  )
import Constants ( defaultSphere, defaultPlane, defaultMaterial )
import Canvas (writeCanvas )
import World ( defaultWorld )
import Transformation ( scaling, translation, viewTransform )
import Types
    ( Point(Point),
      Vec(Vec),
      Shape(material, transform),
      Camera(hSize, vSize, fov, camTransform),
      Canvas,
      Color(Color),
      Light(PointLight),
      Material(color, diffuse, specular),
      World(lights, objects) )

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
        { transform = translation (-0.5) 1 0.5,
          material = defaultMaterial {color = Color 0.1 1 0.5, diffuse = 0.7, specular = 0.3}
        }
    left =
      defaultSphere
        { transform = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          material = defaultMaterial {color = Color 1 0.8 0.1, diffuse = 0.7, specular = 0.3}
        }
    right = middle {transform = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5}

runChapter9 :: IO ()
runChapter9 = writeCanvas "spheres-on-plane.ppm" drawScene