import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Data.List as List
import Data.Foldable (forM_)
import Data.Char (toLower)


data Safety = Unsafe | Safe deriving (Show)

formatSafety :: Safety -> String
formatSafety s =
   case s of
      Unsafe -> "unsafe"
      Safe -> "safe"

lowerInit :: String -> String
lowerInit (c:cs) = toLower c : cs
lowerInit "" = ""

list :: [(String, Safety, String, [String])]
list =
   ("plan_dft_1d", Safe,
    "PlanDFT1d",
      "C.CInt ->" :
      "Ptr (Complex a) -> Ptr (Complex a) ->" :
      "Sign -> Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_2d", Safe,
    "PlanDFT2d",
      "C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr (Complex a) ->" :
      "Sign -> Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_3d", Safe,
    "PlanDFT3d",
      "C.CInt -> C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr (Complex a) ->" :
      "Sign -> Flags -> IO (Plan a)" :
      []) :

   ("plan_dft", Safe,
    "PlanDFT",
      "C.CInt -> Ptr C.CInt ->" :
      "Ptr (Complex a) -> Ptr (Complex a) ->" :
      "Sign -> Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_r2c_1d", Safe,
    "PlanDFTr2c1d",
      "C.CInt ->" :
      "Ptr a -> Ptr (Complex a) ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_r2c_2d", Safe,
    "PlanDFTr2c2d",
      "C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr (Complex a) ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_r2c_3d", Safe,
    "PlanDFTr2c3d",
      "C.CInt -> C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr (Complex a) ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_r2c", Safe,
    "PlanDFTr2c",
      "C.CInt -> Ptr C.CInt ->" :
      "Ptr a -> Ptr (Complex a) ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_c2r_1d", Safe,
    "PlanDFTc2r1d",
      "C.CInt ->" :
      "Ptr (Complex a) -> Ptr a ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_c2r_2d", Safe,
    "PlanDFTc2r2d",
      "C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr a ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_c2r_3d", Safe,
    "PlanDFTc2r3d",
      "C.CInt -> C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr a ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_dft_c2r", Safe,
    "PlanDFTc2r",
      "C.CInt -> Ptr C.CInt ->" :
      "Ptr (Complex a) -> Ptr a ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_r2r_1d", Safe,
    "PlanR2r1d",
      "C.CInt ->" :
      "Ptr a -> Ptr a ->" :
      "Kind -> Flags -> IO (Plan a)" :
      []) :

   ("plan_r2r_2d", Safe,
    "PlanR2r2d",
      "C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr a ->" :
      "Kind -> Kind -> Flags -> IO (Plan a)" :
      []) :

   ("plan_r2r_3d", Safe,
    "PlanR2r3d",
      "C.CInt -> C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr a ->" :
      "Kind -> Kind -> Kind -> Flags -> IO (Plan a)" :
      []) :

   ("plan_r2r", Safe,
    "PlanR2r",
      "C.CInt -> Ptr C.CInt ->" :
      "Ptr a -> Ptr a ->" :
      "Ptr Kind -> Flags -> IO (Plan a)" :
      []) :


   ("plan_many_dft", Safe,
    "PlanManyDFT",
      "C.CInt -> Ptr C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Sign -> Flags -> IO (Plan a)" :
      []) :

   ("plan_many_dft_r2c", Safe,
    "PlanManyDFTr2c",
      "C.CInt -> Ptr C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_many_dft_c2r", Safe,
    "PlanManyDFTc2r",
      "C.CInt -> Ptr C.CInt -> C.CInt ->" :
      "Ptr (Complex a) -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Flags -> IO (Plan a)" :
      []) :

   ("plan_many_r2r", Safe,
    "PlanManyR2r",
      "C.CInt -> Ptr C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Ptr a -> Ptr C.CInt ->" :
      "C.CInt -> C.CInt ->" :
      "Ptr Kind -> Flags -> IO (Plan a)" :
      []) :


   ("plan_guru_dft", Safe,
    "PlanGuruDFT",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr (Complex a) -> Ptr (Complex a) -> Sign -> Flags -> IO (Plan a)" :
      []) :
   ("plan_guru_dft_r2c", Safe,
    "PlanGuruDFTr2c",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr a -> Ptr (Complex a) -> Flags -> IO (Plan a)" :
      []) :
   ("plan_guru_dft_c2r", Safe,
    "PlanGuruDFTc2r",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr (Complex a) -> Ptr a -> Flags -> IO (Plan a)" :
      []) :
   ("plan_guru_r2r", Safe,
    "PlanGuruR2r",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr a -> Ptr a -> Ptr Kind -> Flags -> IO (Plan a)" :
      []) :

   ("plan_guru_split_dft", Safe,
    "PlanGuruSplitDFT",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr a -> Ptr a -> Ptr a -> Ptr a -> Flags -> IO (Plan a)" :
      []) :
   ("plan_guru_split_dft_r2c", Safe,
    "PlanGuruSplitDFTr2c",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr a -> Ptr a -> Ptr a -> Flags -> IO (Plan a)" :
      []) :
   ("plan_guru_split_dft_c2r", Safe,
    "PlanGuruSplitDFTc2r",
      "C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim ->" :
      "Ptr a -> Ptr a -> Ptr a -> Flags -> IO (Plan a)" :
      []) :

   ("destroy_plan", Safe,
    "DestroyPlan",
      "Plan a -> IO ()" :
      []) :

   ("execute", Safe,
    "Execute",
      "Plan a -> IO ()" :
      []) :
   ("execute_dft", Safe,
    "ExecuteDFT",
      "Plan a -> Ptr (Complex a) -> Ptr (Complex a) -> IO ()" :
      []) :
   ("execute_dft_r2c", Safe,
    "ExecuteDFTr2c",
      "Plan a -> Ptr a -> Ptr (Complex a) -> IO ()" :
      []) :
   ("execute_dft_c2r", Safe,
    "ExecuteDFTc2r",
      "Plan a -> Ptr (Complex a) -> Ptr a -> IO ()" :
      []) :
   ("execute_r2r", Safe,
    "ExecuteR2r",
      "Plan a -> Ptr a -> Ptr a -> IO ()" :
      []) :
   ("execute_split_dft", Safe,
    "ExecuteSplitDFT",
      "Plan a -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO ()" :
      []) :
   ("execute_split_dft_r2c", Safe,
    "ExecuteSplitDFTr2c",
      "Plan a -> Ptr a -> Ptr a -> Ptr a -> IO ()" :
      []) :
   ("execute_split_dft_c2r", Safe,
    "ExecuteSplitDFTc2r",
      "Plan a -> Ptr a -> Ptr a -> Ptr a -> IO ()" :
      []) :

   ("malloc", Unsafe,
    "Malloc",
      "C.CSize -> IO (Ptr a)" :
      []) :
   ("free", Unsafe,
    "Free",
      "Ptr a -> IO ()" :
      []) :

   ("alloc_real", Unsafe,
    "AllocReal",
      "C.CSize -> IO (Ptr a)" :
      []) :
   ("alloc_complex", Unsafe,
    "AllocComplex",
      "C.CSize -> IO (Ptr (Complex a))" :
      []) :
   ("free", Unsafe,
    "FreeComplex",
      "Ptr (Complex a) -> IO ()" :
      []) :

   []


-- cf. lapack-ffi-tools
insertExportList :: [String] -> String -> String
insertExportList exports modul =
   (\(prefix,suffixes) ->
      map fst prefix ++
      case suffixes of
         [] -> []
         (_,suffix):_ ->
            "(\n" ++ unlines (map (printf "   %s,") exports) ++
            "   ) " ++ suffix) $
   break (List.isPrefixOf "where" . snd) $
   zip modul (List.tails modul)


main :: IO ()
main = do
   args <- getArgs
   case args of
      [] ->
         forM_ list $ \(_,_,funcName,typeExpr) -> do
            printf "\ntype %s a =\n" funcName
            mapM_ (printf "   %s\n") typeExpr

      ["--generic", tmplPath] -> do
         tmpl <- readFile tmplPath
         let export (_,_,funcType,_) =
               printf "FFT.%s, %s" funcType (lowerInit funcType)
         putStr $ insertExportList (map export list) tmpl
         forM_ list $ \(_,_,funcType,_) -> do
            let funcName = lowerInit funcType
            printf "\n"
            printf "newtype %s a =\n" funcType
            printf "   %s {run%s :: FFT.%s a}\n" funcType funcType funcType
            printf "\n"
            printf "%s :: (Class.Real a) => FFT.%s a\n" funcName funcType
            printf "%s =\n" funcName
            printf "   run%s $\n" funcType
            printf "   Class.switchReal\n"
            printf "      (%s FFTF.%s)\n" funcType funcName
            printf "      (%s FFTD.%s)\n" funcType funcName

      [floatName, cPrefix] ->
         forM_ list $ \(cName,safety,funcName,_) -> do
            printf "\nforeign import ccall %s \"fftw3.h %s_%s\"\n"
               (formatSafety safety) cPrefix cName
            printf "   %s :: FFT.%s %s\n"
               (lowerInit funcName) funcName floatName

      _ -> fail "wrong number of arguments"
