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
    Shape (minY, maxY),
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
      let Point _ originY _ = origin
          Vec _ directionY _ = direction
          t = - originY / directionY
       in toIntersections ([Intersection t shape | abs directionY >= epsilon])
    Cube {} ->
      let Ray (Point originX originY originZ) (Vec directionX directionY directionZ) = ray
          (xtmin, xtmax) = checkAxis originX directionX
          (ytmin, ytmax) = checkAxis originY directionY
          (ztmin, ztmax) = checkAxis originZ directionZ
          tmin = maximum [xtmin, ytmin, ztmin]
          tmax = minimum [xtmax, ytmax, ztmax]
       in if tmin > tmax
            then toIntersections []
            else toIntersections [Intersection tmin shape, Intersection tmax shape]
    Cylinder {minY, maxY} ->
      let Point originX originY originZ = origin
          Vec directionX directionY directionZ = direction
          a = (directionX ^ 2) + (directionZ ^ 2)
          b = (2 * originX * directionX) + (2 * originZ * directionZ)
          c = (originX ^ 2) + (originZ ^ 2) - 1
          d = (b ^ 2) - (4 * a * c)
          t0 = (-b - sqrt d) / (2 * a)
          t1 = (-b + sqrt d) / (2 * a)
          -- swap t0 and t1 if t0 > t1
          t0' = if t0 > t1 then t1 else t0
          t1' = if t0 > t1 then t0 else t1
          y0 = originY + (t0' * directionY)
          y1 = originY + (t1' * directionY)
          intersection_y0 =
            [Intersection t0' shape | minY < y0 && y0 < maxY]
          intersection_y1 =
            [Intersection t1' shape | minY < y1 && y1 < maxY]
          capIntersections = intersectCaps shape (Ray origin direction)
       in toIntersections (intersection_y0 ++ intersection_y1 ++ capIntersections)
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
      let localNormal = Vec x y z
       in normalize (worldNormal localNormal)
    Plane {} ->
      let localNormal = Vec 0 1 0
       in normalize (worldNormal localNormal)
    Cube {} ->
      let maxc = maximum [abs x, abs y, abs z]
       in if | maxc ~= abs x -> Vec x 0 0
             | maxc ~= abs y -> Vec 0 y 0
             | maxc ~= abs z -> Vec 0 0 z
    Cylinder {minY, maxY} ->
      let d = x ^ 2 + z ^ 2
       in if | d < 1 && y >= maxY - epsilon -> Vec 0 1 0
             | d < 1 && y <= minY + epsilon -> Vec 0 (-1) 0
             | otherwise -> Vec x 0 z
  where
    inverseTransform = inverse (getTransformation shape)
    Point x y z = inverseTransform `mpMult` point
    worldNormal localNormal = transpose inverseTransform `mvMult` localNormal

intersectCaps :: Shape -> Ray -> [Intersection]
intersectCaps cyl@Cylinder {minY, maxY, closed} (Ray origin direction)
  | not closed || directionY ~= 0 = []
  | otherwise = hitCap t_bottom ++ hitCap t_top
  where
    (Point originX originY originZ, Vec directionX directionY directionZ) =
      (origin, direction)
    t_bottom = (minY - originY) / directionY
    t_top = (maxY - originY) / directionY
    hitCap t =
      let x = originX + t * directionX
          z = originZ + t * directionZ
       in [Intersection t cyl | x ^ 2 + z ^ 2 <= 1]

{- Default shapes -}
defaultSphere :: Shape
defaultSphere = Sphere defaultMaterial identity

glassSphere :: Shape
glassSphere = Sphere glass identity

defaultPlane :: Shape
defaultPlane = Plane defaultMaterial identity

defaultCube :: Shape
defaultCube = Cube defaultMaterial identity

defaultCylinder :: Shape
defaultCylinder = Cylinder defaultMaterial identity posInf negInf False

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0