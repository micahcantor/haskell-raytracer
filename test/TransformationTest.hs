module TransformationTest (tests) where

import Test.HUnit (Test (..), assertEqual)
import Transformation
  ( identity,
    scaling,
    translation,
    viewTransform,
  )
import Types ( Point(Point), Vec(Vec) )

testViewTransformDefault :: Test
testViewTransformDefault = TestCase $ do
  let from = Point 0 0 0
      to = Point 0 0 (-1)
      up = Vec 0 1 0
  assertEqual "default" identity (viewTransform from to up)

testViewTransformPositiveZ :: Test
testViewTransformPositiveZ = TestCase $ do
  let from = Point 0 0 0
      to = Point 0 0 1
      up = Vec 0 1 0
  assertEqual "scaling" (scaling (-1) 1 (-1)) (viewTransform from to up)

testViewTransformTranslate :: Test
testViewTransformTranslate = TestCase $ do
  let from = Point 0 0 8
      to = Point 0 0 0
      up = Vec 0 1 0
  assertEqual "translation" (translation 0 0 (-8)) (viewTransform from to up)

tests :: Test
tests = TestList [testViewTransformDefault, testViewTransformPositiveZ, testViewTransformTranslate]