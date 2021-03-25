module Chapter2 where

import Data.Matrix ( matrix, setElem, toLists, Matrix(..) )
import Chapter1

data Color = Color Float Float Float
type Canvas = Matrix Color

instance Show Color where
    show (Color r g b) = show r ++ " " ++ show g ++ " " ++ show b

cAdd :: Color -> Color-> Color
cAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (b1 + b2) (g1 + g2)

cSub :: Color -> Color-> Color
cSub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (b1 - b2) (g1 - g2)

cMult :: Float -> Color -> Color
cMult c (Color r g b) = Color (c * r) (c * g) (c * b)

hadamard :: Color -> Color -> Color
hadamard (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)

initCanvas :: Int -> Int -> Canvas
initCanvas w h = matrix w h $ \(_, _) -> Color 0 0 0

canvasToPPM :: Canvas -> String
canvasToPPM c =
  let maxColor = 255
      w = show (nrows c)
      h = show (ncols c)
      header = "P3\n" ++ w ++ " " ++ h ++ "\n" ++ show maxColor ++ "\n"
      rowToString = foldl (\acc x -> acc ++ " " ++ show x) ""
      toStrLines = foldl1 (\acc x -> acc  ++ "\n" ++ x)
   in header ++ toStrLines (map rowToString (toLists c))

writeCanvas :: FilePath -> Canvas -> IO ()
writeCanvas file c = writeFile file (canvasToPPM c)

{- Exercise: Putting it together -}
plotProjectile :: Projectile -> Environment -> Canvas -> Canvas
plotProjectile pr@(Projectile (Point x y z) vel) env canv
    | y <= 0 = canv
    | otherwise = 
        let red = Color 255 0 0
            nextProjectile = tick pr env
            (Point newX newY _) = position nextProjectile
            nextCanvas = setElem red (round newX, ncols canv - round newY) canv
         in plotProjectile nextProjectile env nextCanvas

runChapter2 :: IO ()
runChapter2 = do
    let p = Projectile (Point 0 1 0) (11.25 `vMult` normalize (Vec 1 1.8 0)) 
    let e = Environment (Vec 0 (-0.1) 0) (Vec (-0.01) 0 0)
    let initialCanvas = initCanvas 900 550
    writeCanvas "test.ppm" (plotProjectile p e initialCanvas)

