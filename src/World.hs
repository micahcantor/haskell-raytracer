module World where

import Color (cAdd, cMult)
import Data.SortedList as SL (fromSortedList)
import Intersection (hit, prepareComputation)
import Light (lighting)
import Material (defaultMaterial, black)
import Shape (defaultSphere, intersect)
import Transformation (scaling)
import Types
  ( Color (..),
    Computation (..),
    Intersection (Intersection),
    Intersections,
    Material (color, diffuse, specular, reflective),
    Point (..),
    PointLight (PointLight),
    Ray (..),
    Shape (material, transform),
    World (..),
    Vec(..)
  )
import VecPoint (magnitude, normalize, pSub)

defaultWorld :: World
defaultWorld =
  let lights = [PointLight (Point (-10) 10 (-10)) (Color 1 1 1)]
      s1 = defaultSphere {material = defaultMaterial {color = Color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2}}
      s2 = defaultSphere {transform = scaling 0.5 0.5 0.5}
      objects = [s1, s2]
   in World lights objects

maxRecursions :: Int
maxRecursions = 4

intersect :: World -> Ray -> Intersections
-- combine the intersections of each object in world with ray
intersect (World _ objects) r = mconcat $ map (`Shape.intersect` r) objects

shadeHit :: World -> Computation -> Int -> Color
-- blend the colors produced by the hits of each light source in the world
shadeHit w@(World lights _) comps@(Computation _ _ object point eyev normalv reflectv overPoint) remaining =
  let applyLighting light =
        let surface = lighting (material object) object light point eyev normalv (isShadowed w overPoint light)
            reflected = reflectedColor w comps remaining
         in surface `cAdd` reflected
      colors = map applyLighting lights
      blend (Color r1 g1 b1) (Color r2 g2 b2) =
        Color (max r1 r2) (max g1 g2) (max b1 b2)
   in foldr1 blend colors

colorAt :: World -> Ray -> Int -> Color
colorAt world ray remaining =
  let black = Color 0 0 0
      intersections = world `World.intersect` ray
   in case hit intersections of
        Nothing -> black
        Just intersection -> shadeHit world (prepareComputation ray intersection) remaining

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

reflectedColor :: World -> Computation -> Int -> Color
-- spawns a new reflection ray when intersected material is reflective
-- remaining argument limits the number of recursions to avoid infinite loops
reflectedColor w comps remaining
  | remaining < 1 = black
  | matReflective == 0 = black
  | otherwise = matReflective `cMult` color
  where
    matReflective = reflective $ material $ object comps
    reflectRay = Ray (over comps) (reflect comps)
    color = colorAt w reflectRay (remaining - 1)
