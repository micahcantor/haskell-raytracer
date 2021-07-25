module World where

{-# LANGUAGE NamedFieldPuns #-}

import Color (cAdd, cMult)
import Data.SortedList as SL (fromSortedList)
import Intersection (hit, prepareComputation, headSL, schlick, atSL)
import Light (lighting)
import Material (defaultMaterial, black, white, testPattern)
import Shape (defaultSphere, intersect, defaultPlane)
import Transformation (scaling, translation)
import Types
  ( Color (..),
    Computation (..),
    Intersection (Intersection),
    Intersections,
    Material (..),
    Point (..),
    PointLight (PointLight),
    Ray (..),
    Shape (material, transform),
    World (..),
    Vec(..), toIntersections
  )
import VecPoint (magnitude, normalize, pSub, dot, vMult, vSub)
import Debug.Trace


defaultWorld :: World
defaultWorld =
  let lights = [PointLight (Point (-10) 10 (-10)) white]
      s1 = defaultSphere {material = defaultMaterial {color = Color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2}}
      s2 = defaultSphere {transform = scaling 0.5 0.5 0.5}
      objects = [s1, s2]
   in World lights objects

maxRecursions :: Int
maxRecursions = 5

intersect :: World -> Ray -> Intersections
-- combine the intersections of each object in world with ray
intersect (World _ objects) r = mconcat $ map (`Shape.intersect` r) objects

shadeHit :: World -> Computation -> Int -> Color
-- blend the colors produced by the hits of each light source in the world
shadeHit w@(World lights _) comps remaining =
  let Computation{ object, point, eye, normal, over } = comps
      applyLighting light
        | objReflective > 0 && objTransparency > 0 =
            surface `cAdd` (reflectance `cMult` reflected) `cAdd` ((1 - reflectance) `cMult` refracted)
        | otherwise = surface `cAdd` reflected `cAdd` refracted
        where
          surface = lighting (material object) object light point eye normal (isShadowed w over light)
          reflected = reflectedColor w comps remaining
          refracted = refractedColor w comps remaining
          objMaterial = material object
          (objReflective, objTransparency) = (reflective objMaterial, transparency objMaterial)
          reflectance = schlick comps
      colors = map applyLighting lights
      blend (Color r1 g1 b1) (Color r2 g2 b2) =
        Color (max r1 r2) (max g1 g2) (max b1 b2)
   in foldr1 blend colors

colorAt :: World -> Ray -> Int -> Color
colorAt world ray remaining =
  let xs = world `World.intersect` ray
   in case hit xs of
        Just intersection -> shadeHit world (prepareComputation ray intersection xs) remaining
        Nothing -> black

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
        Just (Intersection t _) -> t < distance
        Nothing -> False

reflectedColor :: World -> Computation -> Int -> Color
-- spawns a new reflection ray when intersected material is reflective
-- remaining argument limits the number of recursions to avoid infinite loops
reflectedColor w comps remaining
  | remaining <= 0 = black
  | matReflective == 0 = black
  | otherwise = reflectColor
  where
    Computation {object, over, reflect} = comps
    matReflective = reflective $ material object
    reflectRay = Ray over reflect
    reflectColor = matReflective `cMult` colorAt w reflectRay (remaining - 1)

refractedColor :: World -> Computation -> Int -> Color 
refractedColor w comps remaining
  | remaining <= 0 = black
  | sin2_t > 1 = black -- total internal reflection, so no refraction
  | matTransparency == 0 = black
  | otherwise = 
    trace
     (unlines [
       "transp: " ++ show matTransparency, 
       "eye: " ++ show eye,
       "normal: " ++ show normal,
       "under: " ++ show under,
       "n1: " ++ show n1,
       "n2: " ++ show n2])
    refractColor
  where
    Computation {object, eye, normal, under, n1, n2} = comps
    matTransparency = transparency (material object)
    nRatio = n1 / n2
    cos_i = eye `dot` normal
    sin2_t = (nRatio ^ 2) * (1 - (cos_i ^ 2))
    cos_t = sqrt (1 - sin2_t)
    direction = ((nRatio * cos_i - cos_t) `vMult` normal) `vSub` (nRatio `vMult` eye)
    refractRay = Ray under direction
    refractColor = matTransparency `cMult` colorAt w refractRay (remaining - 1)

testRefractedColor :: String 
testRefractedColor =
  let [s1, s2] = objects defaultWorld
      a = s1 {material = defaultMaterial {ambient = 1.0, pattern = testPattern}}
      b = s2 {material = defaultMaterial {transparency = 1.0, refractive = 1.5}}
      w = defaultWorld {objects = [a, b]}
      r = Ray (Point 0 0 0.1) (Vec 0 1 0)
      xs = toIntersections [Intersection (-0.9899) a, Intersection (-0.4899) b, Intersection 0.4899 b, Intersection 0.9899 a]
      comps = prepareComputation r (xs `atSL` 2) xs
   in show $ refractedColor w comps 5