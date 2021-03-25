module Chapter1 where

data Vec = Vec Float Float Float deriving (Show)
data Point = Point Float Float Float deriving (Show)

vAdd :: Vec -> Vec -> Vec
vAdd (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

vpAdd :: Point -> Vec -> Point
vpAdd (Point x1 y1 z1) (Vec x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

vSub :: Vec -> Vec -> Vec
vSub (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

vpSub :: Point -> Vec -> Point
vpSub (Point x1 y1 z1) (Vec x2 y2 z2) = Point (x1 - x2) (y1 - y2) (z1 - z2)

vNeg :: Vec -> Vec
vNeg (Vec x y z) = Vec (-x) (-y) (-z)

pNeg :: Point -> Point
pNeg (Point x y z) = Point (-x) (-y) (-z)

vMult :: Float -> Vec -> Vec
vMult c (Vec x y z) = Vec (c*x) (c*y) (c*z)

pMult :: Float -> Point -> Point
pMult c (Point x y z) = Point (c*x) (c*y) (c*z)

vDiv :: Vec -> Float -> Vec
vDiv (Vec x y z) c = Vec (x / c) (y / c) (z / c)

pDiv :: Point -> Float -> Point
pDiv (Point x y z) c = Point (x / c) (y / c) (z / c)

magnitude :: Vec -> Float
magnitude (Vec x y z) = sqrt ((x^2) + (y^2) + (z^2))

normalize :: Vec -> Vec
normalize v@(Vec x y z) = Vec (x / mag) (y / mag) (z / mag)
  where mag = magnitude v

dot :: Vec -> Vec -> Float
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Vec -> Vec -> Vec
cross (Vec x1 y1 z1) (Vec x2 y2 z2) 
  = Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

{- Exercise: Putting it together -}
data Projectile = Projectile {position :: Point, velocity :: Vec }
data Environment = Environment Vec Vec -- gravity and wind

tick :: Projectile -> Environment -> Projectile
tick (Projectile pos vel) (Environment gravity wind) =
  let position = pos `vpAdd` vel
      velocity = vel `vAdd` gravity `vAdd` wind
   in Projectile position velocity

go :: Projectile -> Environment -> Int -> IO ()
go pr@(Projectile po@(Point x y z) vel) e n
  | y <= 0 = putStrLn ("hit ground after " ++ show n ++ " ticks")
  | otherwise = do
      print po
      go (tick pr e) e (n + 1) 

runChapter1 :: IO ()
runChapter1 = do
  let p = Projectile (Point 0 1 0) (normalize (Vec 1 1 0))
  let e = Environment (Vec 0 (-0.1) 0) (Vec (-0.01) 0 0)
  go p e 0
  