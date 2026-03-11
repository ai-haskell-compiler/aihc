import Graphics.Imlib
import Data.Bits
import System 

valueToAlpha (ImlibColor a r g b) = ImlibColor a' r g b
    where a' = round ((r' + g' + b') / 3)
          r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b

usage = do 
    putStrLn "value2alpha infile outfile"
    exitFailure

main = do
    args <- getArgs
    if (length args /= 2) then usage else return ()
    let [file,newfile] = args
    image <- loadImage file
    contextSetImage image
    imageSetFormat "png"
    withImage (\w h xs -> map valueToAlpha xs)
    imageSetHasAlpha True
    saveImage newfile
