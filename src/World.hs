module World where

import Color (Color (Color), cAdd)
import Data.SortedList as SL (fromSortedList)
import Light (PointLight (..), lighting)
import Material (Material (..), defaultMaterial)
import Ray (Ray (Ray))
import Shape (Computation (..), Intersection (..), Intersections, Shape (..), defaultSphere, hit, intersect, prepareComputation)
import Transformation (scaling)
import VecPoint (Point (Point), Vec (Vec), magnitude, normalize, pSub)

data World = World {lights :: [PointLight], objects :: [Shape]}

defaultWorld :: World
defaultWorld =
  let lights = [PointLight (Point (-10) 10 (-10)) (Color 1 1 1)]
      s1 = defaultSphere {material = defaultMaterial {color = Color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2}}
      s2 = defaultSphere {transform = scaling 0.5 0.5 0.5}
      objects = [s1, s2]
   in World lights objects

intersect :: World -> Ray -> Intersections
-- combine the intersections of each object in world with ray
intersect (World _ objects) r = mconcat $ map (`Shape.intersect` r) objects

shadeHit :: World -> Computation -> Color
-- blend the colors produced by the hits of each light source in the world
shadeHit w@(World lights _) (Computation _ _ object point eyev normalv overPoint) =
  let applyLighting light =
        lighting (material object) light point eyev normalv (isShadowed w overPoint light)
      colors = map applyLighting lights
      blend (Color r1 g1 b1) (Color r2 g2 b2) =
        Color (max r1 r2) (max g1 g2) (max b1 b2)
   in foldr1 blend colors

colorAt :: World -> Ray -> Color
colorAt w r =
  let black = Color 0 0 0
      intersections = w `World.intersect` r
   in case hit intersections of
        Nothing -> black
        Just intersection -> shadeHit w (prepareComputation r intersection)

isShadowed :: World -> Point -> PointLight -> Bool
-- calculate intersections from a given point to all lights in world,
-- if there is a hit, then return true if the t value is less than the magnitude of the ray,
-- i.e. the hit is between the point and the light
isShadowed world point (PointLight lightPos _) =
  let pointToLight = lightPos `pSub` point
      distance = magnitude pointToLight
      ray = Ray point (normalize pointToLight)
      intersections = world `World.intersect` ray
   in case hit intersections of
        Nothing -> False
        Just (Intersection t _) -> t < distance
