module OBJTest where

import IO.OBJ
import Test.HUnit (Test (..), assertEqual)
import Types ( Shape(..), Point(..) )
import Shape (triangleInfo)

testParseVertexData :: Test
testParseVertexData = TestCase $ do
  parsed <- parseObj "test/data/test-10.obj"
  let t1 = head (children parsed)
  let t2 = children parsed !! 1
  print (triangleInfo t1)
  print (triangleInfo t2)
  assertEqual "10: t1-p1" (Point (-1) 1 0) (p1 t1)
  assertEqual "10: t1-p2" (Point (-1) 0 0) (p2 t1)
  assertEqual "10: t1-p3" (Point 1 0 0) (p3 t1)
  assertEqual "10: t2-p1" (Point (-1) 1 0) (p1 t2)

tests =
  TestList
    [testParseVertexData]