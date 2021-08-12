module Intersection where

import Data.Foldable (find)
import Data.List (delete)
import Data.SortedList as SL (drop, filter, fromSortedList, toSortedList, uncons)
import Ray (position)
import Shape (normalAt)
import Transformation (translation)
import Types
  ( Computation (..),
    Intersection (..),
    Intersections,
    Material (refractive),
    Point (..),
    Ray (..),
    Shape (..),
    Vec (..),
    toIntersections,
  )
import VecPoint (dot, epsilon, reflect, vMult, vNeg, vpAdd, vpSub)

headSL :: Intersections -> Intersection
headSL = head . SL.fromSortedList

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = SL.fromSortedList xs !! i

hit :: Intersections -> Maybe Intersection
-- returns the first nonzero intersection, if it exists
hit = find (\(Intersection t _) -> t > 0)

prepareComputation :: Ray -> Intersection -> Intersections -> Computation
-- precomputes the state of an intersection
prepareComputation r@(Ray origin direction) hit@(Intersection t object) xs =
  let point = Ray.position r t
      eyev = vNeg direction
      normalv = normalAt object point
      normalDotEye = normalv `dot` eyev
      newNormalv
        | normalDotEye < 0 = vNeg normalv
        | otherwise = normalv
      reflectv = VecPoint.reflect direction newNormalv
      inside = normalDotEye < 0
      surfaceNormalFactor = epsilon
      overPoint = point `vpAdd` (surfaceNormalFactor `vMult` newNormalv)
      underPoint = point `vpSub` (surfaceNormalFactor `vMult` newNormalv)
      (n1, n2) = computeRefraction hit xs
   in Computation inside t object point eyev newNormalv reflectv overPoint underPoint n1 n2

computeRefraction :: Intersection -> Intersections -> (Double, Double)
computeRefraction hit intersections =
  go (fromSortedList intersections) [] (0, 0)
  where
    go :: [Intersection] -> [Shape] -> (Double, Double) -> (Double, Double)
    go [] _ (n1, n2) = (n1, n2)
    go (i@(Intersection t object) : xs) containers (n1, n2)
      | i == hit = (n1', n2')
      | otherwise = go xs containers' (n1', n2')
      where
        getFirstRefractive = refractive . material . head
        n1'
          | i == hit = case containers of
            [] -> 1.0
            _ -> getFirstRefractive containers
          | otherwise = n1
        containers'
          | object `elem` containers =
            delete object containers
          | otherwise =
            object : containers
        n2'
          | i == hit = case containers' of
            [] -> 1.0
            _ -> getFirstRefractive containers'
          | otherwise = n2
        

schlick :: Computation -> Double
-- uses the schlick approximation to calculate reflectance
schlick comps
  | n_1 > n_2 && sin2_t > 1.0 = 1.0
  | n_1 > n_2 && sin2_t < 1.0 = r0 + (1 - r0) * (1 - cos_t) ^ 5
  | otherwise = r0 + (1 - r0) * (1 - cos) ^ 5
  where
    (eyev, normalv, n_1, n_2) = (eye comps, normal comps, n1 comps, n2 comps)
    cos = eyev `dot` normalv
    n = n_1 / n_2
    sin2_t = n ^ 2 * (1 - cos ^ 2)
    cos_t = sqrt (1.0 - sin2_t)
    r0 = ((n_1 - n_2) / (n_1 + n_2)) ^ 2
