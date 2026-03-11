module Main where

-- base
import Control.Monad ( unless )
import Data.Monoid( (<>) )
import Control.Applicative( (<|>), (<**>) )
import System.IO ( stdin )

import Options.Applicative( Parser, strOption, optional, short, long
                          , flag', metavar, help, execParser, info
                          , fullDesc, header, helper, progDesc
                          , argument, str )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Vector as V

---- statistics
import Statistics.Sample ( range, mean, variance, stdDev, stdErrMean )

---- fastlogger
import System.Log.FastLogger( TimedFastLogger, ToLogStr, LogType'( LogStderr )
                            , defaultBufSize, newTimeCache, simpleTimeFormat
                            , toLogStr, newTimedFastLogger, withTimedFastLogger )

---- microlens
import Lens.Micro ((^.))

-- local library imports
import qualified Data.Fits as FITS
import Data.Fits ( HeaderDataUnit(..), dimensions, extension, mainData
                 , Axes, Dimensions(..), axes, bitpix
                 , pixDimsByCol, parsePix, isBitPixInt, isBitPixFloat, pixsUnwrapI, pixsUnwrapD )
import Data.Fits.Read ( readHDUs )

-- | Paramaterized input type for files or standard input.
data Input = FileInput FilePath -- ^ The 'FileInput' constructor needs a path name
           | StdInput           -- ^ The `StdInput` constructor stands in for the obvious

-- | Parameterized output type for files or standard output.
data Output = FileOutput FilePath -- ^ When you have a file use 'FileOutput'
            | StdOutput           -- ^ For a screen dump use 'StdOutput'

fileInput :: Parser Input
fileInput = FileInput <$> argument str
    ( metavar "FILE"
   <> help "Input FITS file to parse" )

stdInput :: Parser Input
stdInput = flag' StdInput
    ( long "stdin"
   <> help "Input FITS from stdin" )

fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
    ( long "outfile"
   <> short 'o'
   <> help "Output filename to store" )

stdOutput :: Parser Output
stdOutput = flag' StdOutput
    ( long "stdout"
   <> help "Output picture to stdout" )

inputParse :: Parser Input
inputParse = fileInput <|> stdInput

outputParse :: Parser Output
outputParse = fileOutput <|> stdOutput

data FitsConfig = FitsConfig
    { infile :: Input
    , outImage :: Maybe Output
    }

fitsConfigParser :: Parser FitsConfig
fitsConfigParser = FitsConfig <$> inputParse <*> optional outputParse

main :: IO ()
main = workOnFITS =<< execParser opts
    where
        opts = info (fitsConfigParser <**> helper)
            ( fullDesc
           <> progDesc "Parse, generate, and render FITS files"
           <> header "fits-parse - a FITS swiss army knife" )

workOnFITS :: FitsConfig -> IO ()
workOnFITS (FitsConfig i o) = do
    timeCache <- newTimeCache simpleTimeFormat
    withTimedFastLogger timeCache (LogStderr defaultBufSize) $ \logger -> do
        fits <- bs i
        myLog logger $ "[DEBUG] input file size " ++ show (BS.length fits) ++ " bytes\n"
        case readHDUs fits of
          Left err -> myLog logger ("[ERROR] cannot parse HDUs " ++ show err)
          Right hdus -> do
            myLog logger ("[DEBUG] found " ++ show (length hdus) ++ " hdu record(s)\n")
            mapM_ (processHDU logger) hdus
  where
    bs (FileInput f) = BS.readFile f
    bs StdInput = BS.hGetContents stdin

processHDU :: TimedFastLogger -> HeaderDataUnit -> IO ()
processHDU logger hdu = do
    myLog logger $ "[DEBUG] Bit Format " ++ show bpf ++ "\n"
    myLog logger $ "[DEBUG] data block size " ++ show (BS.length pd) ++ " bytes\n"
    myLog logger $ "[DEBUG] " ++ show (length (hdu ^. dimensions . axes)) ++ " Axes\n"
    unless (null ax) (mapM_ logAxes (hdu ^. dimensions . axes))
    let pixCount = sum (pixDimsByCol $ (hdu ^. dimensions . axes))
    pixs <- parsePix pixCount bpf (LBS.fromStrict pd)
    let pxsI = if isBitPixInt bpf then pixsUnwrapI bpf pixs else []
        pxsD = if isBitPixFloat bpf then pixsUnwrapD bpf pixs else []
        pVI = V.fromList pxsI
        pVD = V.fromList pxsD
    myLog logger $ "[DEBUG] Total Pix Count: " ++ show (length pixs) ++ "\n"
    myLog logger $ "[DEBUG] Unwrapped Int Count: " ++ show (length pxsI) ++ "\n"
    myLog logger $ "[DEBUG] Unwrapped Double Count: " ++ show (length pxsD) ++ "\n"
    myLog logger $ "[DEBUG] Vector Int Count: " ++ show (length pVI) ++ "\n"
    myLog logger $ "[DEBUG] Vector Double Count: " ++ show (length pVD) ++ "\n"
    if (length ax == 2) && isBitPixFloat bpf
      then
        bitMapProcess logger ax pVD
      else
        myLog logger "[DEBUG] skipping bitmap analysis.\n"

  where
    hd = hdu ^. FITS.header
    pd = hdu ^. mainData
    ax = hdu ^. dimensions . axes
    bpf = hdu ^. dimensions . bitpix
    logAxes a = myLog logger $ "[DEBUG] Axis: "
                            ++ show a
                            ++ " count: "
                            ++ show a ++ "\n"

{- | If we happen to be working in floating point 2D, let's try the
     following.
-}
bitMapProcess :: TimedFastLogger
              -> Axes          -- ^ Metadata about the column oriented axes
              -> V.Vector Double -- ^ Data is stored in column-row major order
              -> IO ()
bitMapProcess logger []    _   = myLog logger "[ERROR] BitMap processing run with no axes.\n"
bitMapProcess logger [_]   _   = myLog logger "[ERROR] BitMap processing run with only one axis.\n"
bitMapProcess logger (y:x) v = do
  myLog logger $ "[DEBUG] Mean:     " ++ show (mean v) ++ "\n"
  myLog logger $ "[DEBUG] Range:    " ++ show (range v) ++ "\n"
  myLog logger $ "[DEBUG] Variance: " ++ show (variance v) ++ "\n"
  myLog logger $ "[DEBUG] Std deviation: " ++ show (stdDev v) ++ "\n"
  myLog logger $ "[DEBUG] Std error of the mean: " ++ show (stdErrMean v) ++ "\n"


myLog:: ToLogStr msg => TimedFastLogger -> msg -> IO ()
myLog logger msg = logger $ \ft -> toLogStr ft <> toLogStr ": " <> toLogStr msg
