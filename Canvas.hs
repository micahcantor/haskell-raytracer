module Canvas where

import System.IO ( IOMode(WriteMode), hPutStrLn, withFile )
import Data.Matrix ( Matrix(..), matrix, setElem, toLists )
import Color ( Color(..) )

type Canvas = Matrix Color

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
