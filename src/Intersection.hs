module Intersection where

import Data.List (delete)
import Data.SortedList as SL (drop, filter, fromSortedList, toSortedList, uncons)
import Ray (position)
import Shape (normalAt)
import Transformation (translation)
import Types
  ( Computation (Computation),
    Intersection (..),
    Intersections,
    Material (refractiveIndex),
    Point (..),
    Ray (..),
    Shape (material, transform),
    Vec (..),
    toIntersections,
  )
import VecPoint (dot, epsilon, reflect, vMult, vNeg, vpAdd, vpSub)

headSL :: Intersections -> Intersection
headSL xs = head $ SL.fromSortedList xs

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = head $ SL.fromSortedList $ SL.drop i xs

hit :: Intersections -> Maybe Intersection
-- returns the first nonzero intersection, if it exists
hit xs =
  fmap fst $ SL.uncons $ SL.filter (\(Intersection t _) -> t > 0) xs

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
      reflectv = reflect direction newNormalv
      inside = normalDotEye < 0
      surfaceNormalFactor = 200 * epsilon -- 
      overPoint = point `vpAdd` (surfaceNormalFactor `vMult` newNormalv)
      underPoint = point `vpSub` (surfaceNormalFactor `vMult` newNormalv)
      (n1, n2) = computeRefraction hit xs
   in Computation inside t object point eyev newNormalv reflectv overPoint underPoint n1 n2

computeRefraction :: Intersection -> Intersections -> (Float, Float)
computeRefraction hit intersections = go (fromSortedList intersections) [] (0, 0)
  where
    go :: [Intersection] -> [Shape] -> (Float, Float) -> (Float, Float)
    go [] _ (n1, n2) = (n1, n2)
    go (i@(Intersection t object) : xs) containers (n1, n2) =
      let calcRefractiveIndex lst
            | null lst = 1.0
            | otherwise = (refractiveIndex . material . head) lst
          first
            | i == hit = calcRefractiveIndex containers
            | otherwise = n1
          newContainers
            | object `elem` containers = delete object containers
            | otherwise = object : containers
          second
            | i == hit = calcRefractiveIndex newContainers
            | otherwise = n2
       in go (if i == hit then [] else xs) newContainers (first, second)
