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

-- Main shape functions:
intersect :: Shape -> Ray -> Intersections
intersect shape@(Shape localIntersect _ _ transform) ray =
  let localRay = Ray.transform ray (inverse transform)
   in localIntersect shape localRay

normalAt :: Shape -> Point -> Vec
normalAt (Shape _ localNormalAt _ transform) point =
  let inverseTransform = inverse transform
      localPoint = inverseTransform `mpMult` point
      localNormal = localNormalAt localPoint
      worldNormal = transpose inverseTransform `mvMult` localNormal
   in normalize worldNormal

{- Spheres -}
defaultSphere :: Shape
defaultSphere = Shape sphereIntersect sphereNormalAt defaultMaterial identity

glassSphere :: Shape
glassSphere = defaultSphere {material = glass}

sphereIntersect :: Shape -> Ray -> Intersections
sphereIntersect sphere (Ray origin direction)
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    sphereToRay = origin `pSub` Point 0 0 0 -- sphere is by default centered at origin
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) sphere
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) sphere

sphereNormalAt :: Point -> Vec
sphereNormalAt (Point x y z) = Vec x y z

{- Planes -}
defaultPlane :: Shape
defaultPlane = Shape planeIntersect planeNormalAt defaultMaterial identity

planeIntersect :: Shape -> Ray -> Intersections
planeIntersect plane (Ray (Point _ originY _) (Vec _ directionY _)) 
  | abs directionY < epsilon = SL.toSortedList [] -- if y component of vec is zero, parallel to xz plane
  | otherwise = toIntersections [Intersection t plane]
  where
    t = -originY / directionY

planeNormalAt :: Point -> Vec
-- constant normal of the xz-plane, other planes obtained through transformaiton
planeNormalAt _ = Vec 0 1 0 
