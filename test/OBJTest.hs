module OBJTest where

import Data.Map ((!))
import IO.OBJ (parseObjFile, ParseResult (..), objToGroup)
import Shape (triangleInfo)
import Test.HUnit (Test (..), assertEqual)
import Types (Point (..), Shape (..))

testParseVertices :: Test 
testParseVertices = TestCase $ do
  result <- parseObjFile "test/data/test-9.obj"
  let vtxs = vertices result
  assertEqual "parse vertex 1" (Point (-1) 1 0) (vtxs ! 1)
  assertEqual "parse vertex 2" (Point (-1) 0.5 0) (vtxs ! 2)
  assertEqual "parse vertex 3" (Point 1 0 0) (vtxs ! 3)
  assertEqual "parse vertex 4" (Point 1 1 0) (vtxs ! 4)

testParseTriangleData :: Test
testParseTriangleData = TestCase $ do
  result <- parseObjFile "test/data/test-10.obj"
  let vtxs = vertices result
  let [g] = groups result
  let [t1, t2] = reverse (children g)
  assertEqual "10: t1-p1" (vtxs ! 1) (p1 t1)
  assertEqual "10: t1-p2" (vtxs ! 2) (p2 t1)
  assertEqual "10: t1-p3" (vtxs ! 3) (p3 t1)
  assertEqual "10: t2-p1" (vtxs ! 1) (p1 t2)

testParsePolygonData :: Test
testParsePolygonData = TestCase $ do
  result <- parseObjFile "test/data/test-11.obj"
  let vtxs = vertices result
  let [g] = groups result
  let [t1, t2, t3] = reverse (children g)
  assertEqual "11: t1-p1" (vtxs ! 1) (p1 t1)
  assertEqual "11: t1-p2" (vtxs ! 2) (p2 t1)
  assertEqual "11: t1-p3" (vtxs ! 3) (p3 t1)
  assertEqual "11: t2-p1" (vtxs ! 1) (p1 t2)
  assertEqual "11: t2-p2" (vtxs ! 3) (p2 t2)
  assertEqual "11: t2-p3" (vtxs ! 4) (p3 t2)
  assertEqual "11: t3-p1" (vtxs ! 1) (p1 t3)
  assertEqual "11: t3-p2" (vtxs ! 4) (p2 t3)
  assertEqual "11: t3-p3" (vtxs ! 5) (p3 t3)

testParseSubgroups :: Test
testParseSubgroups = TestCase $ do
  result <- parseObjFile "test/data/test-12.obj"
  let vtxs = vertices result
  let [_, g1, g2] = reverse (groups result)
  let t1 = head (children g1)
  let t2 = head (children g2)
  assertEqual "12: t1-p1" (vtxs ! 1) (p1 t1)
  assertEqual "12: t1-p2" (vtxs ! 2) (p2 t1)
  assertEqual "12: t1-p3" (vtxs ! 3) (p3 t1)
  assertEqual "12: t2-p1" (vtxs ! 1) (p1 t2)
  assertEqual "12: t2-p2" (vtxs ! 3) (p2 t2)
  assertEqual "12: t2-p3" (vtxs ! 4) (p3 t2)

testConvertToGroup :: Test 
testConvertToGroup = TestCase $ do
  result <- parseObjFile "test/data/test-12.obj"
  let g = objToGroup result
  assertEqual "convert to group" 2 (length (children g))

tests =
  TestList
    [ testParseVertices,
      testParseTriangleData,
      testParsePolygonData,
      testParseSubgroups,
      testConvertToGroup
    ]