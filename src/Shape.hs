{-# LANGUAGE MultiWayIf #-}

module Shape where

import Data.Matrix (transpose)
import qualified Data.SortedList as SL
import Material (defaultMaterial, glass)
import Ray (transform)
import Transformation (identity, inverse, mpMult, mvMult, scaling, translation)
import Types
  ( Computation (Computation),
    Intersection (..),
    Intersections,
    Material (..),
    Point (..),
    Ray (..),
    Shape (..),
    Vec (..),
    getTransformation,
    toIntersections,
    (~=),
  )
import VecPoint (dot, epsilon, normalize, pSub, vMult, vNeg, vpAdd)

{- Main shape functions -}
intersect :: Shape -> Ray -> Intersections
intersect shape ray =
  case shape of
    Sphere {} ->
      let sphereToRay = origin `pSub` Point 0 0 0 -- sphere is by default centered at origin
          a = direction `dot` direction
          b = 2 * (direction `dot` sphereToRay)
          c = (sphereToRay `dot` sphereToRay) - 1
          d = (b ^ 2) - (4 * a * c)
          t1 = Intersection (((- b) - sqrt d) / (2 * a)) shape
          t2 = Intersection (((- b) + sqrt d) / (2 * a)) shape
       in toIntersections (if d < 0 then [] else [t1, t2])
    Plane {} ->
      let Point _ y1 _ = origin
          Vec _ y2 _ = direction
          t = - y1 / y2
       in toIntersections ([Intersection t shape | abs y2 >= epsilon])
    Cube {} ->
      let Ray (Point x1 y1 z1) (Vec x2 y2 z2) = ray
          (xtmin, xtmax) = checkAxis x1 x2
          (ytmin, ytmax) = checkAxis y1 y2
          (ztmin, ztmax) = checkAxis z1 z2
          tmin = maximum [xtmin, ytmin, ztmin]
          tmax = minimum [xtmax, ytmax, ztmax]
          xs1 = Intersection tmin shape
          xs2 = Intersection tmax shape
       in toIntersections (if tmin > tmax then [] else [xs1, xs2])
  where
    transform = getTransformation shape
    Ray origin direction = Ray.transform ray (inverse transform)
    checkAxis origin direction
      | tmin > tmax = (tmax, tmin)
      | otherwise = (tmin, tmax)
      where
        tmin = ((-1) - origin) / direction
        tmax = (1 - origin) / direction

normalAt :: Shape -> Point -> Vec
normalAt shape point =
  case shape of
    Sphere {} ->
      let Point x y z = inverseTransform `mpMult` point
          localNormal = Vec x y z
       in normalize (worldNormal localNormal)
    Plane {} ->
      let localNormal = Vec 0 1 0
       in normalize (worldNormal localNormal)
    Cube {} ->
      let Point x y z = inverseTransform `mpMult` point
          maxc = maximum (map abs [x, y, z])
       in if | maxc ~= abs x -> Vec x 0 0
             | maxc ~= abs y -> Vec 0 y 0
             | maxc ~= abs z -> Vec 0 0 z 
  where
    inverseTransform = inverse (getTransformation shape)
    worldNormal localNormal = transpose inverseTransform `mvMult` localNormal

{- Default shapes -}
defaultSphere :: Shape
defaultSphere = Sphere defaultMaterial identity

glassSphere :: Shape
glassSphere = Sphere glass identity

defaultPlane :: Shape
defaultPlane = Plane defaultMaterial identity

defaultCube :: Shape
defaultCube = Cube defaultMaterial identity