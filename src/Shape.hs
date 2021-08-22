{-# LANGUAGE MultiWayIf #-}

module Shape where

import Data.Matrix (transpose)
import qualified Data.SortedList as SL
import qualified Ray (transform)
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
    toIntersections,
    (~/=),
    (~=), fromIntersections, Transformation
  )
import VecPoint (dot, epsilon, normalize, pSub, vMult, vNeg, vpAdd, cross)
import Constants (triangle)

{- Main shape functions -}
intersect :: Shape -> Ray -> Intersections
intersect shape ray =
  let transformedRay = Ray.transform ray (inverse (transform shape))
   in localIntersect shape transformedRay

localIntersect :: Shape -> Ray -> Intersections
localIntersect shape ray@(Ray origin direction) =
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
          bodyIntersections = calcBodyIntersections a b c minY maxY
          capIntersections = calcCapIntersections shape
        in toIntersections (bodyIntersections ++ capIntersections)
    Triangle {p1, e1, e2} ->
      let dir_cross_e2 = direction `cross` e2
          det = e1 `dot` dir_cross_e2
          f = 1 / det
          p1_to_origin = origin `pSub` p1
          u = f * (p1_to_origin `dot` dir_cross_e2)
          origin_cross_e1 = p1_to_origin `cross` e1
          v = f * (direction `dot` origin_cross_e1)
          t = f * (e2 `dot` origin_cross_e1)
       in toIntersections $
            if | abs det < epsilon -> []
               | u < 0 || u > 1 -> []
               | v < 0 || u + v > 1 -> []
               | otherwise -> [Intersection t shape]
    Group {children} ->
      foldMap (`intersect` ray) children
  where
    Point o_x o_y o_z = origin
    Vec d_x d_y d_z = direction

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

checkAxis :: Double -> Double -> (Double, Double)
checkAxis origin direction
  | tmin > tmax = (tmax, tmin)
  | otherwise = (tmin, tmax)
  where
    tmin = ((-1) - origin) / direction
    tmax = (1 - origin) / direction

normalAt :: Shape -> Point -> Vec
normalAt shape point =
  normalToWorld shape localNormal
  where
    localPoint = worldToObject shape point
    localNormal = localNormalAt shape localPoint

localNormalAt :: Shape -> Point -> Vec
localNormalAt shape (Point x y z) = case shape of
  Sphere {} -> Vec x y z
  Plane {} -> Vec 0 1 0
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
  Triangle {norm} -> norm
  Group {} -> error "no local normal for groups"

{- Group helpers -}
addChild :: Shape -> Shape -> (Shape, Shape)
addChild group@Group {children} shape =
  let newShape = shape { parent = Just group }
      newGroup = group { children = newShape : children }
   in (newGroup, newShape)

addChildren :: Shape -> [Shape] -> (Shape, [Shape])
addChildren group = foldr f (group, [])
  where
    f oldShape (oldGroup, oldShapes) =
      let (newGroup, newShape) = addChild oldGroup oldShape
       in (newGroup, newShape : oldShapes)

-- Update the parents of an updated group's children
-- Necessary because of lack of mutation.
updateGroupParents :: Shape -> (Shape -> Shape) -> Shape
updateGroupParents group@Group{} update =
  let updated = update group
   in updated {children = map (`updateGroup` update) (children updated)}
updateGroup primitive update =
  case parent primitive of
    Nothing -> primitive
    Just p -> primitive {parent = Just (update p)}

updateTransform :: Shape -> Transformation -> Shape
updateTransform shape t =
  updateGroupParents shape (\s -> s {transform = t})

updateMaterial :: Shape -> Material -> Shape
updateMaterial shape m =
  updateGroupParents shape (\s -> s {material = m})

worldToObject :: Shape -> Point -> Point
worldToObject shape point =
  case parent shape of
    Nothing -> inverse (transform shape) `mpMult` point
    Just p ->
      let objectPoint = worldToObject p point
       in inverse (transform shape) `mpMult` objectPoint

normalToWorld :: Shape -> Vec -> Vec
normalToWorld shape normal =
  case parent shape of
     Nothing -> newNormal
     Just p ->
       let worldNormal = normalToWorld p newNormal
        in worldNormal
  where
    newNormal =
      normalize $ transpose (inverse (transform shape)) `mvMult` normal

triangleInfo :: Shape -> String
triangleInfo t =
  unwords ["p1:", show (p1 t), 
           "p2:", show (p2 t), 
           "p3:", show (p3 t), 
           "e1:", show (e1 t),
           "e2:", show (e2 t),
           "norm:", show (norm t)]