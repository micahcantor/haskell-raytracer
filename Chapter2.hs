module Chapter2 where

import System.IO ( IOMode(WriteMode), hPutStrLn, withFile )
import Data.Matrix ( matrix, setElem, toLists, Matrix(..) )
import Chapter1
    ( normalize,
      tick,
      vMult,
      Environment(..),
      Point(Point),
      Projectile(Projectile, position),
      Vec(Vec) )

data Color = Color Float Float Float
type Canvas = Matrix Color

instance Show Color where
    show (Color r g b) = show r ++ " " ++ show g ++ " " ++ show b

cAdd :: Color -> Color-> Color
cAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (b1 + b2) (g1 + g2)

cSub :: Color -> Color -> Color
cSub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (b1 - b2) (g1 - g2)

cMult :: Float -> Color -> Color
cMult c (Color r g b) = Color (c * r) (c * g) (c * b)

hadamard :: Color -> Color -> Color
hadamard (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)

initCanvas :: Int -> Int -> Canvas
initCanvas width height = matrix height width (\_ -> Color 0 0 0)

setPixel :: Color -> Int -> Int -> Canvas -> Canvas
setPixel co x y = setElem co (y, x)

canvasHeight :: Canvas -> Int
canvasHeight = nrows

canvasWidth :: Canvas -> Int
canvasWidth = ncols

canvasDimensions :: Canvas -> (Int, Int)
canvasDimensions c = (canvasWidth c, canvasHeight c)

writeCanvas :: FilePath -> Canvas -> IO ()
writeCanvas fp c = do
    let maxColor = 255
        w = show (ncols c)
        h = show (nrows c)
        header = "P3\n" ++ w ++ " " ++ h ++ "\n" ++ show maxColor
        rowToString x = unwords $ map show x
        ppmLines = map rowToString (toLists c)
    withFile fp WriteMode (\handle -> do
        hPutStrLn handle header
        mapM_ (hPutStrLn handle) ppmLines) 

{- Exercise: Putting it together -}
plotProjectile :: Projectile -> Environment -> Canvas -> Canvas
plotProjectile pr@(Projectile (Point x y z) vel) env canv
    | y <= 0 = canv
    | otherwise = 
        let red = Color 255 0 0
            width = canvasWidth canv
            height = canvasHeight canv
            nextProjectile = tick pr env
            (Point newX newY _) = position nextProjectile
            nextCanvas = 
                if (0 < round newX && round newX < width) && (0 < round newY && round newY < height)
                 then setPixel red (round newX) (height - round newY) canv
                 else canv
         in plotProjectile nextProjectile env nextCanvas

runChapter2 :: IO ()
runChapter2 = do
    let p = Projectile (Point 0 1 0) (11.25 `vMult` normalize (Vec 1 1.8 0)) 
        e = Environment (Vec 0 (-0.1) 0) (Vec (-0.01) 0 0)
        initialCanvas = initCanvas 900 550
    writeCanvas "parabola.ppm" (plotProjectile p e initialCanvas)

