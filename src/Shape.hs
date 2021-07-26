module Shape where

import Data.Matrix (transpose)
import Types
    ( Shape(..),
      Intersections,
      Intersection(..),
      Computation(Computation),
      Point(..),
      Vec(..),
      Ray(..),
      Material(..),
      toIntersections )
import qualified Data.SortedList as SL
import Transformation (identity, inverse, mpMult, mvMult, scaling, translation)
import VecPoint (dot, epsilon, normalize, pSub, vMult, vNeg, vpAdd)
import Material ( defaultMaterial, glass )
import Ray ( transform )

{- Main shape functions -}
intersect :: Shape -> Ray -> Intersections
intersect sphere@(Sphere material transform) ray
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    (Ray origin direction) = Ray.transform ray (inverse transform)
    sphereToRay = origin `pSub` Point 0 0 0 -- sphere is by default centered at origin
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) sphere
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) sphere

intersect plane@(Plane material transform) ray
  | abs directionY < epsilon = SL.toSortedList [] -- if y component of vec is zero, parallel to xz plane
  | otherwise = toIntersections [Intersection t plane]
  where
    Ray origin direction = Ray.transform ray (inverse transform)
    Point _ originY _ = origin
    Vec _ directionY _ = direction
    t = -originY / directionY

normalAt :: Shape -> Point -> Vec
normalAt (Sphere _ transform) point = normalize worldNormal
  where
    inverseTransform = inverse transform
    (Point x y z) = inverseTransform `mpMult` point
    localNormal = Vec x y z
    worldNormal = transpose inverseTransform `mvMult` localNormal

normalAt (Plane _ transform) point = normalize worldNormal
  where
    inverseTransform = inverse transform
    localNormal = Vec 0 1 0 
    worldNormal = transpose inverseTransform `mvMult` localNormal

{- Default shapes -}
defaultSphere :: Shape
defaultSphere = Sphere defaultMaterial identity

glassSphere :: Shape
glassSphere = Sphere glass identity

defaultPlane :: Shape
defaultPlane = Plane defaultMaterial identity