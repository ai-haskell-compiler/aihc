module Main where

import qualified Options.Applicative as OP

import Shell.Utility.ParseArgument (parseNumber)
import Shell.Utility.Exit (exitFailureMsg)

import qualified Control.Concurrent.PooledIO.Independent as Pool

import qualified System.Environment as Env
import System.Directory (renameFile)
import System.Process (callProcess, readProcess)

import qualified Data.Foldable as Fold
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Ix as Ix
import Data.Tuple.HT (mapSnd)
import Data.Monoid ((<>))

import Control.Applicative (pure, (<*>), (<|>))

import Text.Printf (printf)


{-
qpdf --show-npages input.pdf

gs -q -dNODISPLAY --permit-file-read=$PATH_TO_PDF -c "($PATH_TO_PDF) (r) file runpdfbegin pdfpagecount = quit"
-}
countPages :: FilePath -> IO Int
countPages path = do
   countStr <-
      readProcess "gs"
         ["-q", "-dNODISPLAY", "--permit-file-read=" ++ path, "-c",
          "(" ++ path ++ ") (r) file runpdfbegin pdfpagecount = quit"]
         ""
   case reads countStr of
      [(count, _)] -> return count
      _ ->
         exitFailureMsg $
         "'gs' returned an incomprehensible page count, namely: " ++ countStr


-- cf. numeric-prelude
divUp :: (Integral a) => a -> a -> a
divUp x y = - div (-x) y

splitPages :: Int -> Int -> NonEmpty.T [] Int
splitPages numJobs numPages =
   fmap (flip divUp numJobs) $
   NonEmpty.mapTail (take numJobs) $ NonEmptyC.iterate (numPages+) 0

splitInChunks :: Int -> Int -> NonEmpty.T [] Int
splitInChunks chunkSize numPages =
   flip NonEmpty.snoc numPages $ takeWhile (<numPages) $ iterate (chunkSize+) 0

splitInBoundedChunks :: Int -> Int -> NonEmpty.T [] Int
splitInBoundedChunks chunkSize numPages =
   splitPages (divUp numPages chunkSize) numPages

run ::
   Int -> Int -> Maybe Int ->
   (Int -> Int -> NonEmpty.T [] Int) ->
   FilePath -> FilePath -> [String] -> IO ()
run numJobs firstPage mLastPage split input output gsArgs = do
   lastPage <- maybe (countPages input) return mLastPage
   let num = Ix.rangeSize (firstPage, lastPage)
   let ranges =
         zip [0::Integer ..] $
         NonEmpty.mapAdjacent
            (\p0 p1 -> (p0,p1-1))
            (fmap (firstPage+) $ split numJobs num)
   Pool.runException (Just numJobs) $
      flip map ranges $ \(job,(p0,p1)) -> do
         callProcess "gs" $
            ("-dFirstPage=" ++ show p0) :
            ("-dLastPage=" ++ show p1) :
            (printf "-sOutputFile=%s-%d" output job) :
            gsArgs ++
            [input]
   let pages = do
         (job,range) <- ranges
         page <- Ix.range (1, Ix.rangeSize range)
         return (job, page)
   Fold.for_ (zip [1::Integer ..] pages) $ \(dstPage, (job, srcPage)) -> do
      renameFile
         (printf output srcPage ++ "-" ++ printf "%d" job)
         (printf output dstPage)


parsePositiveNumber :: (Read a, Ord a, Num a) => String -> OP.ReadM a
parsePositiveNumber name =
   OP.eitherReader $ parseNumber name (0<) "positive"

parser :: OP.Parser ([String] -> IO ())
parser =
   pure run
   <*>
      (OP.option (parsePositiveNumber "number of jobs") $
         OP.short 'j' <>
         OP.long "jobs" <>
         OP.metavar "NUMBER" <>
         OP.value 1 <>
         OP.help "Number of parallel threads")
   <*>
      (OP.option (parsePositiveNumber "page") $
         OP.long "first-page" <>
         OP.metavar "ONEBASED" <>
         OP.value 1 <>
         OP.help "First page to render")
   <*>
      (OP.option (fmap Just $ parsePositiveNumber "page") $
         OP.long "last-page" <>
         OP.metavar "ONEBASED" <>
         OP.value Nothing <>
         OP.help "Last page to render")
   <*>
      (
         (OP.option
               (fmap (\chunkSize _numJobs numPages ->
                        splitInChunks chunkSize numPages) $
                parsePositiveNumber "number of pages") $
            OP.long "chunk-size" <>
            OP.metavar "NUMBER" <>
            OP.help "Split pages into chunks of NUMBER pages, except the last chunk")
         <|>
         (OP.option
               (fmap (\chunkSize _numJobs numPages ->
                        splitInBoundedChunks chunkSize numPages) $
                parsePositiveNumber "number of pages") $
            OP.long "max-chunk-size" <>
            OP.metavar "NUMBER" <>
            OP.value splitPages <>
            OP.help "Split pages into chunks of balanced size containing at most NUMBER pages")
      )
   <*>
      OP.strArgument
         (OP.metavar "INPUT" <>
          OP.help "Input Document")
   <*>
      OP.strArgument
         (OP.metavar "OUTPUT" <>
          OP.help "Output Filename Pattern")

info :: OP.Parser a -> OP.ParserInfo a
info p =
   OP.info
      (OP.helper <*> p)
      (OP.fullDesc <>
       OP.progDesc "Let Ghostscript render pages in parallel.")

main :: IO ()
main = do
   args <- Env.getArgs
   let (optParseArgs, gsArgs) = mapSnd (drop 1) $ break ("--" ==) args
   ($gsArgs) =<<
      OP.handleParseResult
         (OP.execParserPure OP.defaultPrefs (info parser) optParseArgs)
