{-# LANGUAGE NamedFieldPuns #-}

module World where

import Color (scale)
import Data.SortedList as SL (fromSortedList)
import Intersection (atSL, headSL, hit, prepareComputation, schlick)
import Light (lighting)
import Material (black, defaultMaterial, testPattern, white)
import Shape (defaultPlane, defaultSphere, intersect)
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
    Shape (..),
    Vec (..),
    World (..),
    getMaterial,
    getTransformation,
    toIntersections,
  )
import VecPoint (dot, magnitude, normalize, pSub, vMult, vSub)

defaultWorld :: World
defaultWorld =
  let light = PointLight (Point (-10) 10 (-10)) white
      s1 = defaultSphere {sphereMaterial = defaultMaterial {color = Color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2}}
      s2 = defaultSphere {sphereTransform = scaling 0.5 0.5 0.5}
      objects = [s1, s2]
   in World light objects

maxRecursions :: Int
maxRecursions = 5

intersect :: World -> Ray -> Intersections
-- combine the intersections of each object in world with ray
intersect (World _ objects) r = mconcat $ map (`Shape.intersect` r) objects

shadeHit :: World -> Computation -> Int -> Color
shadeHit w@(World light _) comps remaining
  | objReflective > 0 && objTransparency > 0 =
    surface + (reflectance `scale` reflected) + ((1 - reflectance) `scale` refracted)
  | otherwise = surface + reflected + refracted
  where
    Computation {object, point, eye, normal, over} = comps
    objMaterial = getMaterial object
    surface = lighting objMaterial object light point eye normal (isShadowed w over light)
    reflected = reflectedColor w comps remaining
    refracted = refractedColor w comps remaining
    (objReflective, objTransparency) = (reflective objMaterial, transparency objMaterial)
    reflectance = schlick comps

colorAt :: World -> Ray -> Int -> Color
colorAt world ray remaining =
  case hit intersections of
    Just i@(Intersection t shape) -> shadeHit world (comps i) remaining
    Nothing -> black
  where
    intersections = world `World.intersect` ray
    comps i = prepareComputation ray i intersections

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
    matReflective = reflective (getMaterial object)
    reflectRay = Ray over reflect
    reflectColor = matReflective `scale` colorAt w reflectRay (remaining - 1)

refractedColor :: World -> Computation -> Int -> Color
refractedColor w comps remaining
  | remaining <= 0 = black
  | sin2_t > 1 = black -- total internal reflection, so no refraction
  | matTransparency == 0 = black
  | otherwise = refractColor
  where
    Computation {object, eye, normal, under, n1, n2} = comps
    matTransparency = transparency (getMaterial object)
    nRatio = n1 / n2
    cos_i = eye `dot` normal
    sin2_t = (nRatio ^ 2) * (1 - (cos_i ^ 2))
    cos_t = sqrt (1 - sin2_t)
    direction = ((nRatio * cos_i - cos_t) `vMult` normal) `vSub` (nRatio `vMult` eye)
    refractRay = Ray under direction
    color = colorAt w refractRay (remaining - 1)
    refractColor = matTransparency `scale` color
