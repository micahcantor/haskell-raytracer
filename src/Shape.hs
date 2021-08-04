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
      toIntersections,
      (~=) )
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

intersect cube@Cube {} (Ray (Point x1 y1 z1) (Vec x2 y2 z2))
  | tmin > tmax = SL.toSortedList []
  | otherwise = SL.toSortedList [Intersection tmin cube, Intersection tmax cube]
  where
    checkAxis origin direction
      | tmin > tmax = (tmax, tmin)
      | otherwise = (tmin, tmax)
      where
        tmin = ((-1) - origin) / direction
        tmax = (1 - origin) / direction
    (xtmin, xtmax) = checkAxis x1 x2
    (ytmin, ytmax) = checkAxis y1 y2
    (ztmin, ztmax) = checkAxis z1 z2
    tmin = maximum [xtmin, ytmin, ztmin]
    tmax = minimum [xtmax, ytmax, ztmax]

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

normalAt (Cube _ transform) point
  | maxc ~= abs x = Vec x 0 0
  | maxc ~= abs y = Vec 0 y 0
  | maxc ~= abs z = Vec 0 0 z
  where
    (Point x y z) = inverse transform `mpMult` point
    maxc = maximum (map abs [x, y, z])

{- Default shapes -}
defaultSphere :: Shape
defaultSphere = Sphere defaultMaterial identity

glassSphere :: Shape
glassSphere = Sphere glass identity

defaultPlane :: Shape
defaultPlane = Plane defaultMaterial identity

defaultCube :: Shape
defaultCube = Cube defaultMaterial identity