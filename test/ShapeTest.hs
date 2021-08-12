module ShapeTest where

import Intersection (atSL, headSL)
import Shape (defaultCube, defaultPlane, defaultSphere, intersect, normalAt, defaultCylinder, defaultCone, defaultGroup, addChild, addChildren, worldToObject, normalToWorld, localNormalAt)
import Test.HUnit (Test (..), assertEqual, assertBool)
import Transformation (translation, scaling, rotationY)
import Types
  ( Intersection (Intersection),
    Point (Point),
    Ray (Ray),
    Shape (..),
    Vec (Vec),
    toIntersections, (~=), fromIntersections
  )
import VecPoint (normalize)

testSphereNormalAt :: Test
testSphereNormalAt = TestCase $ do
  let s = defaultSphere
      n1 = normalAt s (Point 1 0 0)
      n2 = normalAt s (Point 0 1 0)
      n3 = normalAt s (Point 0 0 1)
      n4 = normalAt s (Point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
  assertEqual "x axis" (Vec 1 0 0) n1
  assertEqual "y axis" (Vec 0 1 0) n2
  assertEqual "z axis" (Vec 0 0 1) n3

testSphereNormalAtTranslated :: Test
testSphereNormalAtTranslated = TestCase $ do
  let s = defaultSphere {transform = translation 0 1 0}
      n = normalAt s (Point 0 1.70711 (-0.70711))
  assertEqual "equality" (Vec 0 0.70711 (-0.70711)) n

testPlaneIntersectParallel :: Test
testPlaneIntersectParallel = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 10 0) (Vec 0 0 1)
      xs = p `intersect` r
  assertEqual "when parallel, xs is empty" (toIntersections []) xs

testPlaneIntersectCoplanar :: Test
testPlaneIntersectCoplanar = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      xs = p `intersect` r
  assertEqual "when coplanar, xs is empty" (toIntersections []) xs

testPlaneIntersectAbove :: Test
testPlaneIntersectAbove = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 1 0) (Vec 0 (-1) 0)
      xs = p `intersect` r
  assertEqual "above" (Intersection 1 p) (headSL xs)

testPlaneIntersectBelow :: Test
testPlaneIntersectBelow = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 (-1) 0) (Vec 0 1 0)
      xs = p `intersect` r
  assertEqual "below" (Intersection 1 p) (headSL xs)

testCubeIntersectHit :: Test
testCubeIntersectHit = TestCase $ do
  let c = defaultCube
      origins =
        [ Point 5 0.5 0,
          Point (-5) 0.5 0,
          Point 0.5 5 0,
          Point 0.5 (-5) 0,
          Point 0.5 0 5,
          Point 0.5 0 (-5),
          Point 0 0.5 0
        ]
      directions =
        [ Vec (-1) 0 0,
          Vec 1 0 0,
          Vec 0 (-1) 0,
          Vec 0 1 0,
          Vec 0 0 (-1),
          Vec 0 0 1,
          Vec 0 0 1
        ]
      t1s = [4, 4, 4, 4, 4, 4, -1]
      t2s = [6, 6, 6, 6, 6, 6, 1]
      rays = zipWith Ray origins directions
      xs = map (intersect c) rays
      get_t (Intersection t _) = t
  assertEqual "t1 when ray strikes cube" t1s (map (get_t . headSL) xs)
  assertEqual "t2 when ray strikes cube" t2s (map (get_t . (`atSL` 1)) xs)

testCubeIntersectMiss :: Test
testCubeIntersectMiss = TestCase $ do
  let c = defaultCube
      origins =
        [ Point (-2) 0 0,
          Point 0 (-2) 0,
          Point 0 0 (-2),
          Point 2 0 2,
          Point 0 2 2,
          Point 2 2 0
        ]
      directions =
        [ Vec 0.2673 0.5345 0.8018,
          Vec 0.8018 0.2673 0.5345,
          Vec 0.5345 0.8018 0.2673,
          Vec 0 0 (-1),
          Vec 0 (-1) 0,
          Vec (-1) 0 0
        ]
      rays = zipWith Ray origins directions
      xs = map (intersect c) rays
  assertBool "no intersections when ray misses cube" (all null xs)

testCubeNormalAt :: Test
testCubeNormalAt = TestCase $ do
  let c = defaultCube
      points =
        [ Point 1 0.5 (-0.8),
          Point (-1) (-0.2) 0.9,
          Point (-0.4) 1 (-0.1),
          Point 0.3 (-1) (-0.7),
          Point (-0.6) 0.3 1,
          Point 0.4 0.4 (-1),
          Point 1 1 1,
          Point (-1) (-1) (-1)
        ]
      normals =
        [ Vec 1 0 0,
          Vec (-1) 0 0,
          Vec 0 1 0,
          Vec 0 (-1) 0,
          Vec 0 0 1,
          Vec 0 0 (-1),
          Vec 1 0 0,
          Vec (-1) 0 0
        ]
  assertEqual "cube normal at various points" normals (map (localNormalAt c) points)

testCylinderIntersectHit :: Test
testCylinderIntersectHit = TestCase $ do
  let cyl = defaultCylinder
      origins =
        [ Point 1 0 (-5),
          Point 0 0 (-5),
          Point 0.5 0 (-5)
        ]
      directions =
        [ Vec 0 0 1,
          Vec 0 0 1,
          Vec 0.1 1 1
        ]
      t0s = [5, 4, 6.80798]
      t1s = [5, 6, 7.08872]
      normalized = map normalize directions
      rays = zipWith Ray origins directions
      xs = map (intersect cyl) rays
      get_t (Intersection t _) = t
  assertEqual "t0 when ray strikes cyl" t0s (map (get_t . headSL) xs)
  assertEqual "t1 when ray strikes cyl" t1s (map (get_t . (`atSL` 1)) xs)

testCylinderIntersectMiss :: Test
testCylinderIntersectMiss = TestCase $ do
  let cyl = defaultCylinder
      origins =
        [ Point 1 0 0,
          Point 0 0 0,
          Point 0 0 (-5)
        ]
      directions =
        [ Vec 0 1 0,
          Vec 0 1 0,
          Vec 1 1 1
        ]
      normalized = map normalize directions
      rays = zipWith Ray origins directions
      xs = map (intersect cyl) rays
   in assertBool "no intersection when ray misses cyl" (all null xs)

testCylinderNormalAt :: Test
testCylinderNormalAt = TestCase $ do
  let cyl = defaultCylinder
      points =
        [ Point 1 0 0,
          Point 0 5 (-1),
          Point 0 (-2) 1,
          Point (-1) 1 0
        ]
      normals =
        [ Vec 1 0 0,
          Vec 0 0 (-1),
          Vec 0 0 1,
          Vec (-1) 0 0
        ]
  assertEqual "normal at various points" normals (map (localNormalAt cyl) points)

testIntersectConstrainedCylinder :: Test
testIntersectConstrainedCylinder = TestCase $ do
  let cyl = defaultCylinder {minY = 1, maxY = 2}
      origins =
        [ Point 0 1.5 0,
          Point 0 3 (-5),
          Point 0 0 (-5),
          Point 0 2 (-5),
          Point 0 1 (-5),
          Point 0 1.5 (-2)
        ]
      directions =
        [ Vec 0.1 1 0,
          Vec 0 0 1,
          Vec 0 0 1,
          Vec 0 0 1,
          Vec 0 0 1,
          Vec 0 0 1
        ]
      counts = [0, 0, 0, 0, 0, 2]
      rays = zipWith Ray origins directions
      xs = map (intersect cyl) rays
  assertEqual "intersect constrained cyl" counts (map length xs)

testIntersectClosedCylinder :: Test
testIntersectClosedCylinder = TestCase $ do
  let cyl = defaultCylinder {minY = 1, maxY = 2, closed = True}
      origins =
        [ Point 0 3 0,
          Point 0 3 (-2),
          Point 0 4 (-2),
          Point 0 0 (-2),
          Point 0 (-1) (-2)
        ]
      directions =
        [ Vec 0 (-1) 0,
          Vec 0 (-1) 2,
          Vec 0 (-1) 1,
          Vec 0 1 2,
          Vec 0 2 1
        ]
      counts = replicate 5 2
      rays = zipWith Ray origins directions
      xs = map (intersect cyl) rays
  assertEqual "intersect closed cyl" counts (map length xs)

testClosedCylinderNormalAt :: Test
testClosedCylinderNormalAt = TestCase $ do
  let cyl = defaultCylinder {minY = 1, maxY = 2, closed = True}
      points =
        [ Point 0 1 0,
          Point 0.5 1 0,
          Point 0 1 0.5,
          Point 0 2 0,
          Point 0.5 2 0,
          Point 0 2 0.5
        ]
      normals =
        [ Vec 0 (-1) 0,
          Vec 0 (-1) 0,
          Vec 0 (-1) 0,
          Vec 0 1 0,
          Vec 0 1 0,
          Vec 0 1 0
        ]
  assertEqual "normal at closed cylinder" normals (map (localNormalAt cyl) points)

testConeIntersect :: Test
testConeIntersect = TestCase $ do
  let cone = defaultCone
      origins =
        [ Point 0 0 (-5),
          Point 0 0 (-5),
          Point 1 1 (-5)
        ]
      directions =
        [ Vec 0 0 1,
          Vec 1 1 1,
          Vec (-0.5) (-1) 1
        ]
      t0s = [5, 8.66025, 4.55006]
      t1s = [5, 8.66025, 49.44994]
      rays = zipWith Ray origins (map normalize directions)
      xs = map (intersect cone) rays
      get_t (Intersection t _) = t
      result_t0s = map (get_t . headSL) xs
      result_t1s = map (get_t . (`atSL` 1)) xs
  assertBool "all rays strike two intersections" (all (\lst -> length lst == 2) xs)
  assertBool "t0 when ray strikes cone" (all (== True) (zipWith (~=) t0s result_t0s))
  assertBool "t1 when ray strikes cone" (all (== True) (zipWith (~=) t1s result_t1s))

testConeIntersectParallel :: Test
testConeIntersectParallel = TestCase $ do
  let cone = defaultCone
      direction = normalize (Vec 0 1 1)
      r = Ray (Point 0 0 (-1)) direction
      xs = cone `intersect` r
      get_t (Intersection t _) = t
  assertEqual "cone intersected once" 1 (length xs)
  assertBool "cone intersected at t" (get_t (headSL xs) ~= 0.35355)

testConeIntersectCap :: Test 
testConeIntersectCap = TestCase $ do
  let cone = defaultCone {minY = -0.5, maxY = 0.5, closed = True}
      origins =
        [ Point 0 0 (-5),
          Point 0 0 (-0.25),
          Point 0 0 (-0.25)
        ]
      directions =
        [ Vec 0 1 0,
          Vec 0 1 1,
          Vec 0 1 0
        ]
      counts = [0, 2, 4]
      rays = zipWith Ray origins (map normalize directions)
      xs = map (intersect cone) rays
  assertEqual "counts of intersections with caps" counts (map length xs)

testConeNormalAt :: Test 
testConeNormalAt = TestCase $ do
  let cone = defaultCone 
      points =
        [ Point 0 0 0,
          Point 1 1 1,
          Point (-1) (-1) 0
        ]
      normals =
        [ Vec 0 0 0,
          Vec 1 (-sqrt 2) 1,
          Vec (-1) 1 0
        ]
  assertEqual "normals on infinte cone" normals (map (localNormalAt cone) points)

testEmptyGroupIntersect :: Test
testEmptyGroupIntersect = TestCase $ do
  let g = defaultGroup
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      xs = g `intersect` r
  assertEqual "empty group has no intersection" (toIntersections []) xs

testGroupIntersect :: Test 
testGroupIntersect = TestCase $ do
  let s1 = defaultSphere
      s2 = defaultSphere {transform = translation 0 0 (-3)}
      s3 = defaultSphere {transform = translation 5 0 0}
      (g, [s1', s2', s3']) = addChildren defaultGroup [s1, s2, s3]
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      xs = fromIntersections (g `intersect` r)
      objAt i xs = (\(Intersection _ obj) -> obj) (xs !! i)
  print (length $ children g)
  assertEqual "four intersections" 4 (length xs)
  assertEqual "first is s2'" s2' (objAt 0 xs)
  assertEqual "second is s2'" s2' (objAt 1 xs)
  assertEqual "third is s1'" s1' (objAt 2 xs)
  assertEqual "fourth is s1'" s1' (objAt 3 xs)

testIntersectTransformedGroup :: Test 
testIntersectTransformedGroup = TestCase $ do
  let g = defaultGroup {transform = scaling 2 2 2}
      s = defaultSphere {transform = translation 5 0 0}
      (g', _) = addChild g s 
      r = Ray (Point 10 0 (-10)) (Vec 0 0 1)
      xs = fromIntersections $ g' `intersect` r
  assertEqual "two intersections" 2 (length xs)

testWorldToObject :: Test 
testWorldToObject = TestCase $ do
  let s = defaultSphere {transform = translation 5 0 0}
      g1 = defaultGroup {transform = rotationY (pi / 2)}
      g2 = defaultGroup {transform = scaling 2 2 2}
      (g1', g2') = addChild g1 g2
      (g2'', s') = addChild g2' s
      p = worldToObject s' (Point (-2) 0 (-10))
  assertEqual "world to object" (Point 0 0 (-1)) p

testNormalToWorld :: Test 
testNormalToWorld = TestCase $ do
  let s = defaultSphere {transform = translation 5 0 0}
      g1 = defaultGroup {transform = rotationY (pi / 2)}
      g2 = defaultGroup {transform = scaling 1 2 3}
      (g1', g2') = addChild g1 g2
      (g2'', s') = addChild g2' s
      n = normalToWorld s' (Vec (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
  assertEqual "normal to world" (Vec 0.2857 0.4286 (-0.8571)) n

testNormalAtGroupMember :: Test 
testNormalAtGroupMember = TestCase $ do
  let s = defaultSphere {transform = translation 5 0 0}
      g1 = defaultGroup {transform = rotationY (pi / 2)}
      g2 = defaultGroup {transform = scaling 1 2 3}
      (g1', g2') = addChild g1 g2
      (g2'', s') = addChild g2' s
      n = normalAt s' (Point 1.7321 1.1547 (-5.5774))
  assertEqual "normal at group member" (Vec 0.2857 0.4286 (-0.8571)) n

tests :: Test
tests =
  TestList
    [ testSphereNormalAt,
      testSphereNormalAtTranslated,
      testPlaneIntersectParallel,
      testPlaneIntersectCoplanar,
      testPlaneIntersectAbove,
      testPlaneIntersectBelow,
      testCubeIntersectHit,
      testCubeIntersectMiss,
      testCubeNormalAt,
      testCylinderIntersectMiss,
      testCubeIntersectHit,
      testCylinderNormalAt,
      testIntersectConstrainedCylinder,
      testIntersectClosedCylinder,
      testClosedCylinderNormalAt,
      testConeIntersect,
      testConeIntersectParallel,
      testConeIntersectCap,
      testConeNormalAt,
      testEmptyGroupIntersect,
      testGroupIntersect,
      testIntersectTransformedGroup,
      testWorldToObject,
      testNormalToWorld,
      testNormalAtGroupMember
    ]