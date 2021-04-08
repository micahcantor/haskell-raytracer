module Main where

import Test.Framework ( defaultMain )
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import IntersectionTest as Intersection ( tests )
import LightTest as Light (tests)
import RayTest as Ray (tests)
import SphereTest as Sphere (tests)
import TransformationTest as Transformation (tests)
import VecPointTest as VecPoint (tests)
import WorldTest as World (tests)
import Test.HUnit (Test(TestList))

main :: IO ()
main = defaultMain (hUnitTestToTests allTests) 
  where
    allTests = TestList [Intersection.tests, Light.tests, Ray.tests, Sphere.tests, Transformation.tests, VecPoint.tests, World.tests]