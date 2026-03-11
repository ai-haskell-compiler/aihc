module Main where

import           Text.CSV.Lazy.ByteString
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.Environment (getArgs)
import           System.Console.GetOpt
import           System.Exit
import           Control.Monad (when, unless)
import           System.IO
import           Data.Char (isDigit)
import           Data.List (elemIndex)
import           Data.Maybe (fromJust)

version = "0.3"

-- lazily read a CSV file, select some columns, and print it out again.
main = do
  opts  <- cmdlineOpts =<< getArgs
  let delim = head $ [ c | Delimiter c <- opts ]++","

  when   (Version   `elem` opts) $ do hPutStrLn stderr $ "csvSelect "++version
                                      exitSuccess
  unless (Unchecked `elem` opts) $ do
      content <- lazyRead opts
      case csvErrors (parseDSV True delim content) of
        errs@(_:_)  -> do hPutStrLn stderr (unlines (map ppCSVError errs))
                          exitWith (ExitFailure 2)
        []          -> return ()
  out <- case [ f | Output f <- opts ] of
              []     -> return stdout
              [file] -> openBinaryFile file WriteMode
              _      -> do hPutStrLn stderr "Too many outputs: only one allowed"
                           exitWith (ExitFailure 3)
  content <- lazyRead opts
  case selectFieldMix [ e | Select e <- opts ]
                      (csvTableFull (parseDSV True delim content)) of
      Left err        -> do hPutStrLn stderr $ "CSV missing fields: "
                                               ++unwords err
                            exitWith (ExitFailure 4)
      Right selection -> do BS.hPut out $ ppCSVTable selection
                            hClose out

-- | The standard Data.CSV.Lazy.selectFields chooses only by field name.
--   This version chooses with any mixture of numeric index or field name.
selectFieldMix :: [ Either Int String ] -> CSVTable -> Either [String] CSVTable
selectFieldMix fields table
    | null table          = Left (map (either show id) fields)
    | not (null missing)  = Left missing
    | otherwise           = Right (map select table)
  where
    header     = map (BS.unpack . csvFieldContent) (head table)
    lenheader  = length header
    missing    = map show (filter (>lenheader)   [ i | Left i     <- fields ])
                 ++ filter (`notElem` header) [ name | Right name <- fields ]
    reordering = map (\e-> case e of Left i  -> i
                                     Right s -> fromJust $ elemIndex s header)
                     fields
    select fields = map (fields!!) reordering

-- | Read a single input file, or stdin.
lazyRead :: [Flag] -> IO BS.ByteString
lazyRead opts =
    case [ f | Input f <- opts ] of
         []     -> BS.hGetContents stdin
         [file] -> BS.readFile file
         _      -> do hPutStrLn stderr "Too many input files: only one allowed"
                      exitWith (ExitFailure 1)


-- Command-line options
data Flag 
     = Version | Input String | Output String | Unchecked
     | Delimiter Char | Select (Either Int String)
     deriving (Show,Eq)
    
options :: [OptDescr Flag]
options =
  [ Option ['v','V'] ["version"]   (NoArg Version)        "show version number"
  , Option ['o']     ["output"]    (ReqArg Output "FILE") "output FILE"
  , Option ['i']     ["input"]     (ReqArg Input  "FILE") "input FILE"
  , Option ['u']     ["unchecked"] (NoArg Unchecked)  "ignore CSV format errors"
  , Option ['d']     ["delimiter"] (ReqArg (Delimiter . head) "@")
                                                      "delimiter char is @"
  ]
    
cmdlineOpts :: [String] -> IO [Flag]
cmdlineOpts argv = 
   case getOpt Permute options argv of
     (o,fs,[] ) -> return (o ++ map field fs)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: csvSelect [OPTION...] (num|fieldname)...\n"
                 ++"    select numbered/named columns from a CSV file"

field :: String -> Flag
field str | all isDigit str = Select (Left (read str))
          | otherwise       = Select (Right str)
