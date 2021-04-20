module Main where

import CameraTest as Camera (tests)
import IntersectionTest as Intersection (tests)
import LightTest as Light (tests)
import MaterialTest as Material (tests)
import RayTest as Ray (tests)
import ShapeTest as Shape (tests)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test (TestList))
import TransformationTest as Transformation (tests)
import VecPointTest as VecPoint (tests)
import WorldTest as World (tests)

main :: IO ()
main = defaultMain (hUnitTestToTests allTests)
  where
    allTests = TestList [Camera.tests, Intersection.tests, Light.tests, Material.tests, Ray.tests, Shape.tests, Transformation.tests, VecPoint.tests, World.tests]