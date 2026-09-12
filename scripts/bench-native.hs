{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Measure native compilation from generated GRIN or source text.
module Main (main) where

import Aihc.Arm64.Lir qualified as Arm64
import Aihc.Grin qualified as Grin
import Aihc.Lir qualified as Lir
#ifdef BASELINE
import Aihc.Lir.Lower qualified as Lower
import Data.ByteString.Lazy qualified as BL
#endif
import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["generate", count, width, path] -> generate (read count) (read width) path
    ["generated", count, width, object] -> compileProgram (generatedProgram (read count) (read width)) object
    ["generate-data", count, path] -> TIO.writeFile path ("data @payload align 8 = { zero " <> T.pack count <> " }\n\nexport func @main() -> i64 {\nentry:\n  return 42\n}\n")
    ["grin", source, object] -> do
      program <- TIO.readFile source >>= checked . Grin.parseProgram
      compileProgram program object
    ["lir", source, object] -> do
      lir <- TIO.readFile source >>= checked . Lir.parseModule
      writeLir lir object
    _ -> ioError (userError "Use generated COUNT WIDTH OBJECT, generate COUNT WIDTH FILE, generate-data COUNT FILE, grin SOURCE OBJECT, or lir SOURCE OBJECT.")

compileProgram :: Grin.GrinProgram -> FilePath -> IO ()
compileProgram program object = do
  gc <- Grin.lowerGc <$> checked (Grin.toCpsGrin program)
  writeGc gc object

writeGc :: Grin.GcGrinProgram -> FilePath -> IO ()
#ifdef BASELINE
writeGc gc object = do
  lir <- checked (Lower.lowerModule Lower.posixTarget64 False gc)
  checked (Arm64.compileLirObjectWith False lir) >>= BL.writeFile object
#else
writeGc = Arm64.writeGrinObjectWith False False Nothing
#endif

writeLir :: Lir.Module -> FilePath -> IO ()
#ifdef BASELINE
writeLir lir object = checked (Arm64.compileLirObjectWith False lir) >>= BL.writeFile object
#else
writeLir = Arm64.writeLirObjectWith False
#endif

-- | Construct the same program as the source generator, without parser costs.
generatedProgram :: Int -> Int -> Grin.GrinProgram
generatedProgram count width = Grin.GrinProgram [] [(Grin.GrinVar "+#" 0 Grin.IntRep, 2)] [] [] (map function [0 .. count - 1])
  where
    function index = Grin.GrinFunction (Grin.FunctionName ("$f" <> T.pack (show index))) [variable 0] Grin.IntRep body
      where
        variable step = Grin.GrinVar "v" (1 + index * (width + 1) + step) Grin.IntRep
        body = foldr instruction (Grin.GrinConstant [Grin.GrinVarValue (variable width)]) [1 .. width]
        instruction step = Grin.GrinBind [variable step] (Grin.GrinPrimitiveCall Grin.IntRep "+#" [Grin.GrinVarValue (variable (step - 1)), Grin.GrinLitValue (Grin.GrinLitInt Grin.IntRep 1)])

checked :: (Show error) => Either error value -> IO value
checked = either (ioError . userError . show) pure

generate :: Int -> Int -> FilePath -> IO ()
generate count width path = withFile path WriteMode $ \handle -> do
  TIO.hPutStrLn handle "primitive +#%0 :: IntRep/2"
  forM_ [0 .. count - 1] $ \index -> do
    let variable step = "v%" <> number (1 + index * (width + 1) + step) <> " :: IntRep"
    TIO.hPutStrLn handle ("\n$f" <> number index <> " (" <> variable 0 <> ") -> IntRep =")
    forM_ [1 .. width] $ \step ->
      TIO.hPutStrLn handle ("  (" <> variable step <> ") <- primitive-call @IntRep +# (" <> variable (step - 1) <> ") (1 :: IntRep)")
    TIO.hPutStrLn handle ("  constant (" <> variable width <> ")")
  where
    number = T.pack . show
