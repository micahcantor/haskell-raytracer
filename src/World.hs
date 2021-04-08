module World where

import Color (Color (Color), cAdd)
import Data.SortedList as SL (fromSortedList)
import Intersection (Computation (..), Intersection (Intersection), Intersections, hit, prepareComputation, sphereIntersect)
import Light (PointLight (..), lighting)
import Material (Material (..), defaultMaterial)
import Ray (Ray (Ray))
import Sphere (Sphere (..), unitSphere)
import Transformation (scaling)
import VecPoint (Point (Point), Vec (Vec))

data World = World {lights :: [PointLight], objects :: [Sphere]}

defaultWorld :: World
defaultWorld =
  let lights = [PointLight (Point (-10) 10 (-10)) (Color 1 1 1)]
      s1 = unitSphere {material = defaultMaterial {color = Color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2}}
      s2 = unitSphere {transformation = scaling 0.5 0.5 0.5}
   in World lights [s1, s2]

intersect :: World -> Ray -> Intersections
-- combine the intersections of each object in world with ray
intersect (World _ objects) r = mconcat $ map (`sphereIntersect` r) objects

shadeHit :: World -> Computation -> Color
-- sum the colors produced by the hits of each light source in the world
shadeHit (World lights _) (Computation _ _ object point eyev normalv) =
  let colors = map (\light -> lighting (material object) light point eyev normalv) lights
   in foldr1 cAdd colors 
  
colorAt :: World -> Ray -> Color
colorAt w r =
  let black = Color 0 0 0
      intersections = w `intersect` r
      hitIntersection = hit intersections
   in case hitIntersection of
        Nothing -> black
        Just intersection -> shadeHit w (prepareComputation r intersection)
