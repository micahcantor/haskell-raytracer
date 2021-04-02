module Chapter5 where

import Chapter1 (Point (..), Vec (..), dot, normalize, pSub, vMult, vpAdd)
import Chapter2 (Canvas (..), Color (..), canvasWidth, initCanvas, setPixel, writeCanvas)
import Chapter4 (Transformation (..), fromPoint, inverse, mpMult, mvMult, scaling, translation)
import Data.Matrix (identity, mapPos)
import qualified Data.SortedList as SL
import Test.HUnit (Assertion, Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)

data Ray = Ray Point Vec -- origin, direction
  deriving (Show, Eq)

data Sphere = Sphere Point Float Transformation -- center, radius, transform
  deriving (Show, Eq)

data Intersection = Intersection Float Sphere -- t value, intersected object
  deriving (Show, Eq)

instance Ord Intersection where
  (Intersection t1 _) <= (Intersection t2 _) = t1 <= t2

type Intersections = SL.SortedList Intersection

unitSphere :: Sphere
unitSphere = Sphere (Point 0 0 0) 1 (identity 4)

setSphereTransform :: Sphere -> Transformation -> Sphere
setSphereTransform (Sphere c r _) = Sphere c r

headSL :: Intersections -> Intersection
headSL xs = head $ SL.fromSortedList xs

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = head $ SL.fromSortedList $ SL.drop i xs

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `vpAdd` (t `vMult` direction)

intersect :: Sphere -> Ray -> Intersections
intersect s@(Sphere center r t) ray
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    (Ray origin direction) = transform ray (inverse t)
    sphereToRay = origin `pSub` center
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) s
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) s

hit :: Intersections -> Maybe Intersection
-- returns the first nonzero intersection, if it exists
hit xs =
  let zeroIntersection = Intersection 0 unitSphere
   in fmap fst $ SL.uncons $ SL.filterGE zeroIntersection xs

transform :: Ray -> Transformation -> Ray
transform (Ray p v) m = Ray (m `mpMult` p) (m `mvMult` v)

{- Putting it together -}
drawSphereShadow :: Canvas -> Canvas
drawSphereShadow canvas =
  mapPos
    ( \(y, x) a ->
        let wallX = (pixelSize * fromIntegral x) - halfWall
            wallY = halfWall - (pixelSize * fromIntegral y)
            wallPosition = Point wallX wallY wallZ
            r = Ray rayStart (normalize (wallPosition `pSub` rayStart))
            xs = intersect unitSphere r
         in case hit xs of
              Just _ -> red
              Nothing -> black
    )
    canvas
  where
    red = Color 235 0 0
    black = Color 0 0 0
    rayStart = Point 0 0 (-5)
    sphereCenter = Point 0 0 0
    wallZ = 10.0
    wallSize = 7.0 :: Float
    pixelSize = wallSize / fromIntegral (canvasWidth canvas)
    halfWall = wallSize / 2

runChapter5 :: IO ()
runChapter5 = do
  let canvas = drawSphereShadow (initCanvas 400 400)
  writeCanvas "sphere-shadow.ppm" canvas

{- Tests -}
testPosition :: Test
testPosition = TestCase $ do
  let r = Ray (Point 2 3 4) (Vec 1 0 0)
  assertEqual "position1" (Point 2 3 4) (position r 0)
  assertEqual "position2" (Point 3 3 4) (position r 1)

testIntersectionTangent :: Test
testIntersectionTangent = TestCase $ do
  let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
      s = unitSphere
      xs = s `intersect` r
  assertEqual "first" (Intersection 5.0 s) (headSL xs)
  assertEqual "second" (Intersection 5.0 s) (xs `atSL` 1)

testIntersectionInside :: Test
testIntersectionInside = TestCase $ do
  let r = Ray (Point 0 0 0) (Vec 0 0 1)
      s = unitSphere
      xs = s `intersect` r
  assertEqual "first" (Intersection (-1.0) s) (headSL xs)
  assertEqual "second" (Intersection 1.0 s) (xs `atSL` 1)

testIntersectionBehind :: Test
testIntersectionBehind = TestCase $ do
  let r = Ray (Point 0 0 5) (Vec 0 0 1)
      s = unitSphere
      xs = s `intersect` r
  assertEqual "first" (Intersection (-6.0) s) (headSL xs)
  assertEqual "second" (Intersection (-4.0) s) (xs `atSL` 1)

testIntersectionScaled :: Test
testIntersectionScaled = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      s = setSphereTransform unitSphere (scaling 2 2 2)
      xs = s `intersect` r
  assertEqual "first" (Intersection 3 s) (headSL xs)
  assertEqual "second" (Intersection 7 s) (xs `atSL` 1)

testIntersectionTranslated :: Test
testIntersectionTranslated = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      s = setSphereTransform unitSphere (translation 5 0 0)
      xs = s `intersect` r
  assertEqual "length" (length xs) 0

testHitPos :: Test
testHitPos = TestCase $ do
  let s = unitSphere
      i1 = Intersection 1 s
      i2 = Intersection 2 s
      xs = SL.toSortedList [i1, i2]
  assertEqual "first" (hit xs) (Just i1)

testHitSomeNeg :: Test
testHitSomeNeg = TestCase $ do
  let s = unitSphere
      i1 = Intersection (-1) s
      i2 = Intersection 2 s
      xs = SL.toSortedList [i1, i2]
  assertEqual "second" (hit xs) (Just i2)

testHitAllNeg :: Test
testHitAllNeg = TestCase $ do
  let s = unitSphere
      i1 = Intersection (-1) s
      i2 = Intersection (-2) s
      xs = SL.toSortedList [i1, i2]
  assertEqual "nothing" (hit xs) Nothing

testHitMix :: Test
testHitMix = TestCase $ do
  let s = unitSphere
      i1 = Intersection 5 s
      i2 = Intersection 7 s
      i3 = Intersection (-3) s
      i4 = Intersection 2 s
      xs = SL.toSortedList [i1, i2, i3, i4]
  assertEqual "fourth" (hit xs) (Just i4)

tests :: Test
tests =
  TestList
    [ testPosition,
      testIntersectionInside,
      testIntersectionTangent,
      testIntersectionBehind,
      testIntersectionScaled,
      testIntersectionTranslated,
      testHitPos,
      testHitSomeNeg,
      testHitAllNeg,
      testHitMix
    ]