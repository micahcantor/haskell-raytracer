module Exercises.Chapter1 where

import VecPoint ( vAdd, vpAdd, normalize, dot )
import Types ( Point(..), Vec(..) )

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