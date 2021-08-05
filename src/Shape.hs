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
    (~/=),
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
      let t = - o_y / d_y
       in toIntersections ([Intersection t shape | abs d_y >= epsilon])
    Cube {} ->
      let (xtmin, xtmax) = checkAxis o_x d_x
          (ytmin, ytmax) = checkAxis o_y d_y
          (ztmin, ztmax) = checkAxis o_z d_z
          tmin = maximum [xtmin, ytmin, ztmin]
          tmax = minimum [xtmax, ytmax, ztmax]
       in if tmin > tmax
            then toIntersections []
            else toIntersections [Intersection tmin shape, Intersection tmax shape]
    Cylinder {minY, maxY} ->
      let a = (d_x ^ 2) + (d_z ^ 2)
          b = (2 * o_x * d_x) + (2 * o_z * d_z)
          c = (o_x ^ 2) + (o_z ^ 2) - 1
          bodyIntersections = calcBodyIntersections a b c minY maxY
          capIntersections = calcCapIntersections shape
       in toIntersections (bodyIntersections ++ capIntersections)
    Cone {minY, maxY} ->
      let a = (d_x ^ 2) - (d_y ^ 2) + (d_z ^ 2)
          b = (2 * o_x * d_x) - (2 * o_y * d_y) + (2 * o_z * d_z)
          c = (o_x ^ 2) - (o_y ^ 2) + (o_z ^ 2)
          t = - c / (2 * b) -- used when a is zero and b is not
          bodyIntersections = calcBodyIntersections a b c minY maxY
          capIntersections = calcCapIntersections shape
       in toIntersections (bodyIntersections ++ capIntersections)
  where
    transform = getTransformation shape
    Ray origin direction = Ray.transform ray (inverse transform)
    Point o_x o_y o_z = origin
    Vec d_x d_y d_z = direction

    checkAxis origin direction
      | tmin > tmax = (tmax, tmin)
      | otherwise = (tmin, tmax)
      where
        tmin = ((-1) - origin) / direction
        tmax = (1 - origin) / direction

    calcBodyIntersections a b c minY maxY
      | a ~= 0 && b ~/= 0 = [Intersection t shape]
      | otherwise = intersection_t0 ++ intersection_t1
      where
        d = (b ^ 2) - (4 * a * c) -- 0
        t = - c / (2 * b) -- used when a is zero and b is not
        t0 = ((- b) - sqrt d) / (2 * a)
        t1 = ((- b) + sqrt d) / (2 * a)
        y0 = o_y + (t0 * d_y)
        y1 = o_y + (t1 * d_y)
        intersection_t0 =
          [Intersection t0 shape | minY < y0 && y0 < maxY]
        intersection_t1 =
          [Intersection t1 shape | minY < y1 && y1 < maxY]

    calcCapIntersections shape
      | not closed || d_y ~= 0 = []
      | otherwise = case shape of
          Cylinder {} -> hitCap t_bottom 1 ++ hitCap t_top 1
          Cone {} -> hitCap t_bottom minY ++ hitCap t_top maxY
      where
        (minY, maxY, closed) = case shape of
          Cylinder {minY, maxY, closed} -> (minY, maxY, closed)
          Cone {minY, maxY, closed} -> (minY, maxY, closed)
        t_bottom = (minY - o_y) / d_y
        t_top = (maxY - o_y) / d_y
        hitCap t radius =
          let x = o_x + t * d_x
              z = o_z + t * d_z
           in [Intersection t shape | x ^ 2 + z ^ 2 <= radius ^ 2]


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
    Cone {minY, maxY} ->
      let d = x ^ 2 + z ^ 2
          normalY = if y > 0 then - (sqrt d) else sqrt d
       in if | d < 1 && y >= maxY - epsilon -> Vec 0 1 0
             | d < 1 && y <= minY + epsilon -> Vec 0 (-1) 0
             | otherwise -> Vec x normalY z
  where
    inverseTransform = inverse (getTransformation shape)
    Point x y z = inverseTransform `mpMult` point
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

defaultCylinder :: Shape
defaultCylinder = Cylinder defaultMaterial identity negInf posInf False

defaultCone :: Shape
defaultCone = Cone defaultMaterial identity negInf posInf False

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0