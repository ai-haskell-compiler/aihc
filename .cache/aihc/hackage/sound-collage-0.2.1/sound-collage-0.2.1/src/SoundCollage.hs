{-# LANGUAGE RebindableSyntax #-}
{- |
Very much inspired by NoiseReduction.

A refined algorithm may increase the clusters for the full scans.
E.g. if a block in the original song
has highest Fourier coefficient at frequency k,
then we might not only scan chunks from the pool with dominant frequency k,
but instead scan some frequencies around k, too.
This way we can hold a subset of the pool chunks in memory for some time.
It would also solve the current problem
of not finding a pool chunk with matching maximum frequency.

An alternative algorithm may use a full-table scan
for searching the best matching chunk
but perform the scan only on some spectral coefficients.
For instance we could choose the spectral coefficients
with the largest variance across all spectra in the pool.
Then we choose a number of indices,
such that all feature vectors fit in memory.
Then we could choose the best matching spectrum
from a number of vectors that best match the selected Fourier coefficients.

Another alternative is to keep only the n largest Fourier coefficients
of each chunk from the pool.
The other coefficients are treated as zero.
This is simpler than the above method and probably maintains more information.
We could measure the ratio of the largest to smallest kept frequency
and we could measure the sum of suppressed frequencies
in order to get an idea of the loss of information.

A variation of the above approach is to approximate the frequency spectrum
by a piecewise constant function.
A hierarchical cluster algorithm could divide the spectrum
into ranges of balanced area.

Both of these strategies allow to hold a compressed form
of the whole pool in memory,
but the detailed search at the second stage requires lot of disk accesses.
Optimally, the first stage is good enough
such that we do not need the second stage, at all.

Every one of these approaches could be complemented
with a lossy audio compression
that reduces the spectral information
to the ones that the human auditory system can perceive.
-}
module SoundCollage (
   testChopCompose,
   runDecompose, runDecomposeSlow, runAssociate, runAdjacent, runCompose,
   Parameters(..), paramChunkSize, defltParams, parametersFromPool,
   ) where

import qualified Sound.SoxLib as SoxLib
import qualified Sound.Frame as Sample

import qualified Numeric.FFTW.Rank1 as Trafo1

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Basic.Binary as Bin

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Foreign.Storable (Storable, peek, sizeOf, )

import qualified Shell.Utility.Log as Shell
import Shell.Utility.Verbosity (Verbosity)

import qualified PathFormat as PathFmt
import qualified System.Path.Directory as Dir
import qualified System.Path.IO as PathIO
import qualified System.Path.PartClass as PathC
import qualified System.Path as Path
import qualified System.IO as IO
import System.Path ((</>), (<++>), )

import Control.Monad.Trans.Except (Except, throwE, runExcept, )
import Control.Monad (forM_, zipWithM_, (<=<), )

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Map (Map, )

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.List.Key as Key
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapPair, mapFst, mapSnd, swap, )
import Data.Ord.HT (comparing, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, mapMaybe, )
import Data.Bool.HT (if', )
import Data.Monoid (mappend, )

import qualified Data.Complex as Complex98
import Data.Int (Int16, )

import Text.Printf (printf, )

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing as Real
import qualified Algebra.Additive as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



pillow ::
   (Trans.C a, Storable a) =>
   Int -> SV.Vector a
pillow n =
   SV.sample n (\i -> sin (pi * fromIntegral i / fromIntegral n))

chop ::
   (Additive.C a, Storable a) =>
   Int -> Int -> SVL.Vector a -> [SVL.Vector a]
chop numOverlap shift =
   takeWhile (not . SVL.null) .
   iterate (SVL.drop shift) .
   SVL.append (SVL.fromChunks [SV.replicate ((numOverlap-1) * shift) zero])

{- |
The chunks in the chunks list must have length numOverlap*shift.
This is not checked.
If you use the result of 'chop'  then you are safe.
-}
compose ::
   (Additive.C a, Storable a) =>
   Int -> Int -> [SVL.Vector a] -> SVL.Vector a
compose numOverlap shift =
   foldl1 SigSt.mix .
   zipWith SVL.drop [0, shift ..] .
   reverse . map SVL.concat .
   ListHT.sliceHorizontal numOverlap


{- |
test reconstruction
-}
testChopCompose :: IO ()
testChopCompose =
   let shift = 42
       overlap = 5
       chunkSize = shift*overlap
       pilPre = SVL.fromChunks [pillow chunkSize]
       pilPost = SVL.map (2 / fromIntegral overlap *) pilPre
       xs :: SVL.Vector Float
       xs =
          SVL.fromChunks
             [SV.sample 10000 (\i -> cos (pi * fromIntegral i / 10000))]
   in  SVL.writeFile "/tmp/test.f32" $
          SVL.zipWith (\_ y -> y) xs $
          compose overlap shift $
          map (FiltNRG.envelope pilPost) $
          map (FiltNRG.envelope pilPre) $
          chop overlap shift $
          SVL.append xs $ SVL.fromChunks [SV.replicate chunkSize zero]


data Parameters =
   Parameters {
      paramShift, paramOverlap, paramChannels :: Int
   }

defltParams :: Parameters
defltParams =
   Parameters {
      paramShift = 4096,
      paramOverlap = 2,
      paramChannels = 1
   }


paramChunkSize :: Parameters -> Int
paramChunkSize params =
   let shift = paramShift params
       overlap = paramOverlap params
   in  shift*overlap

maxCoeff :: SV.Vector Float -> Int
maxCoeff ys =
   Key.maximum (SV.index ys) $ take (SV.length ys) [0..]

featuresFromChunk ::
   Parameters -> SV.Vector Float -> SV.Vector Float
featuresFromChunk _params =
   Array.toStorableVector .
   Array.map Complex98.magnitude . Trafo1.fourierRC .
   Array.mapShape (\(Shape.ZeroBased n) -> Shape.Cyclic n) .
   Array.fromStorableVector

chopChannel ::
   Parameters -> SVL.Vector Float -> [SV.Vector Float]
chopChannel params input =
   let shift = paramShift params
       overlap = paramOverlap params
       chunkSize = shift*overlap

       pilPre = pillow chunkSize

   in  takeWhile ((chunkSize==) . SV.length) $
       map (FiltNRG.envelope pilPre . SV.concat . SVL.chunks) $
       chop overlap shift $
       SVL.append input $
       SVL.fromChunks [SV.replicate chunkSize zero]




spectrumSuffix, chunkSuffix, indexSuffix, originSuffix, targetSuffix :: String
spectrumSuffix = "-spec.f32"
chunkSuffix = "-chunk.s16"
indexSuffix = "-index.txt"
originSuffix = "-origin.txt"
targetSuffix = "-target.txt"


withChopped ::
   (PathC.AbsRel ar) =>
   Parameters -> Path.File ar ->
   ([(SV.Vector Float, SV.Vector Float)] -> IO ()) ->
   IO ()
withChopped params src f =
   withFilePath (SoxLib.withRead SoxLib.defaultReaderInfo) src $ \fmtInPtr -> do
      fmtIn <- peek fmtInPtr
      let numChan =
             fromMaybe 1 $ SoxLib.channels $ SoxLib.signalInfo fmtIn

      inputs <-
         fmap (SVL.deinterleave numChan . SVL.map Bin.toCanonical) $
         SoxLib.readStorableVectorLazy fmtInPtr (SVL.ChunkSize 16384)

      f $ uncurry zip $
         mapPair
            (foldl1 (zipWith (SV.zipWith (+))),
             map SV.interleave . List.transpose) $
         unzip $
         map ((\chunks -> (map (featuresFromChunk params) chunks, chunks)) .
              chopChannel params)
            inputs


createDirectoriesFor :: (PathC.AbsRel ar) => Path.File ar -> IO ()
createDirectoriesFor =
   Dir.createDirectoryIfMissing True . Path.takeDirectory

runDecomposeSlow ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1) =>
   Parameters -> Path.File ar0 -> PathFmt.File ar1 -> IO ()
runDecomposeSlow params src dst =
   withChopped params src $ \chopped ->

      let write n (spec,chunk) = do
            let path = PathFmt.printf dst (maxCoeff spec)
            createDirectoriesFor path
            PathIO.appendFile (path <++> indexSuffix) $
               show (Path.toString src, n::Int) ++ "\n"
            withFilePath SV.appendFile (path <++> spectrumSuffix) spec
            withFilePath SV.appendFile (path <++> chunkSuffix) $
               (SV.map (Bin.fromCanonicalWith Real.roundSimple) chunk
                  :: SV.Vector Int16)

      in  zipWithM_ write [0..] chopped

runDecompose ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1) =>
   Parameters -> Path.File ar0 -> PathFmt.File ar1 -> IO ()
runDecompose params src dst =
   withChopped params src $ \chopped ->

      let keyValue n (spec,chunk) =
            (maxCoeff spec,
             (Seq.singleton n,
              Seq.singleton spec,
              Seq.singleton
                 (SV.map (Bin.fromCanonicalWith Real.roundSimple) chunk
                    :: SV.Vector Int16)))

          write maxi (index, specs, chunks) = do
            let path = PathFmt.printf dst maxi
            createDirectoriesFor path
            PathIO.appendFile (path <++> indexSuffix) $ unlines $
               map (\n -> show (Path.toString src, n::Int)) $ Fold.toList index
            withFilePath SVL.appendFile (path <++> spectrumSuffix) $
               SVL.fromChunks $ Fold.toList specs
            withFilePath SVL.appendFile (path <++> chunkSuffix) $
               SVL.fromChunks $ Fold.toList chunks

      in  Fold.sequence_ $ Map.mapWithKey write $
          Map.fromListWith (flip mappend) $
          zipWith keyValue [0..] chopped

getDirectoryContents ::
   (PathC.AbsRel ar) => Path.Dir ar -> IO [Path.RelFile]
getDirectoryContents = fmap snd . Dir.relDirectoryContents

fileSize :: (PathC.AbsRel ar) => Path.File ar -> IO Integer
fileSize path = PathIO.withFile path IO.ReadMode IO.hFileSize

divByteSize :: (Sample.C a) => a -> Integer -> Integer
divByteSize x n =
   div n (fromIntegral (Sample.sizeOfElement x))

maybeSuffix ::
   (PathC.AbsRel ar) =>
   String -> Path.File ar -> Maybe (Path.File ar)
maybeSuffix suffix =
   Path.mapFileNameF $ ListHT.maybeSuffixOf suffix

maybeSpectrumSuffix ::
   (PathC.AbsRel ar) => Path.File ar -> Maybe (Path.File ar)
maybeSpectrumSuffix = maybeSuffix spectrumSuffix

isSuffixOf :: (PathC.AbsRel ar) => String -> Path.File ar -> Bool
isSuffixOf suffix path = List.isSuffixOf suffix $ Path.toString path


parametersFromPool :: (PathC.AbsRel ar) => Path.Dir ar -> IO (Int, Int)
parametersFromPool dir = do
   (specFiles, chunkFiles) <-
      fmap
         (unzip .
          mapMaybe
            (\file ->
               fmap ((,) (dir </> file)) $
               fmap ((dir </>) . (<++> chunkSuffix)) $
               maybeSpectrumSuffix file)) $
      getDirectoryContents dir
   specSize <-
      fmap (divByteSize (0::Float) . foldl gcd 0) $ mapM fileSize specFiles
   chunkSize <-
      fmap (divByteSize (0::Int16) . foldl gcd 0) $ mapM fileSize chunkFiles
   let evenSize = (specSize-1)*2
   let oddSize  = specSize*2-1
   let (evenQuot, evenRem) = divMod chunkSize evenSize
   let (oddQuot,  oddRem)  = divMod chunkSize oddSize
   fmap (mapPair (fromInteger, fromInteger)) $
      if' (evenRem == 0) (return (evenSize, evenQuot)) $
      if' (oddRem == 0)  (return (oddSize,  oddQuot)) $
      ioError $ userError "inconsistent file sizes in pool"


withFilePath ::
   (PathC.AbsRel ar) => (FilePath -> a) -> (Path.File ar -> a)
withFilePath f = f . Path.toString

data SplitPath ar = SplitPath (Path.Dir ar) Path.RelFile
   deriving (Eq, Ord, Show)

loadSpectra ::
   (PathC.AbsRel ar) =>
   Parameters -> SplitPath ar -> IO [(SV.Vector Float, Int)]
loadSpectra params (SplitPath dir name) =
   fmap (flip zip [0..]) $
   PathIO.withFile (dir </> name <++> spectrumSuffix) IO.ReadMode $ \h ->
      let chunkSize = div (paramChunkSize params) 2 + 1
          go = do
             spec <- SV.hGet h chunkSize
             let actualSize = SV.length spec
              in if' (actualSize == chunkSize) (fmap (spec:) go) $
                 if' (actualSize == 0) (return []) $
                 ioError $ userError "loading spectra: bucket size not multiple of chunk size"
      in  go

loadIndex :: (PathC.AbsRel ar) => Path.File ar -> IO [(Path.AbsRelFile, Int)]
loadIndex path =
   fmap (map (mapFst Path.path . read) . lines) $ PathIO.readFile path

success :: a -> Except e a
success = return

keyClash :: key -> Except key a -> Except key a -> Except key a
keyClash key _ _ = throwE key

catchKeyClash ::
   (Show key) => String -> Map key (Except key a) -> IO (Map key a)
catchKeyClash name em =
   case runExcept $ Trav.sequenceA em of
      Right m -> return m
      Left key ->
         ioError $ userError $ name ++ ": duplicate key " ++ show key

loadOriginMap ::
   (PathC.AbsRel ar) =>
   SplitPath ar -> IO (Map (Path.RelFile, Int) (Path.AbsRelFile, Int))
loadOriginMap (SplitPath dir name) = do
   index <- loadIndex $ dir </> name <++> indexSuffix
   catchKeyClash "load origin map" $
      Map.fromListWithKey keyClash $
      zipWith (\n orig -> ((name, n), success orig)) [0..] index

loadOriginIndex ::
   (PathC.AbsRel ar) => Path.File ar -> IO (Float, (Path.RelFile, Int))
loadOriginIndex = fmap (mapSnd $ mapFst Path.path) . readIO <=< PathIO.readFile

loadTargetIndex ::
   (PathC.AbsRel ar) => Path.File ar -> IO (Path.RelFile, Int)
loadTargetIndex = fmap (mapFst Path.path) . readIO <=< PathIO.readFile


loadChunk ::
   (PathC.AbsRel ar) =>
   Parameters -> Path.File ar -> Int -> IO (SV.Vector Int16)
loadChunk params path offset =
   PathIO.withFile path IO.ReadMode $ \h -> do
      let chanChunkSize = paramChannels params * paramChunkSize params
      IO.hSeek h IO.AbsoluteSeek $
         fromIntegral (offset * chanChunkSize * sizeOf (0::Int16))
      SV.hGet h chanChunkSize

loadSpectrum ::
   (PathC.AbsRel ar) =>
   Int -> Path.File ar -> Int -> IO (SV.Vector Float)
loadSpectrum chunkSize path offset =
   PathIO.withFile path IO.ReadMode $ \h -> do
      let specSize = div chunkSize 2 + 1
      IO.hSeek h IO.AbsoluteSeek $
         fromIntegral (offset * specSize * sizeOf (0::Float))
      SV.hGet h specSize


norm2 :: SV.Vector Float -> Float
norm2 = sqrt . SV.foldl' (+) 0 . SV.map (\x -> x*x)

createSpectrumMap ::
   (PathC.AbsRel ar) =>
   Parameters -> SplitPath ar -> IO [(SV.Vector Float, (Float, Int))]
createSpectrumMap params poolBucket = do
   files <- loadSpectra params poolBucket
   return $ flip map files $ \(spec, pos) ->
      case normalizeSpectrum spec of
         (normedSpec, norm) -> (normedSpec, (norm, pos))

normalizeSpectrum :: SV.Vector Float -> (SV.Vector Float, Float)
normalizeSpectrum spec =
   let norm = norm2 spec
   in  (SV.map (/norm) spec, norm)

matchSpectrum :: SV.Vector Float -> SV.Vector Float -> Float
matchSpectrum spec dict =
   SV.foldl' (+) 0 $ SV.zipWith (*) spec dict

asVector16 :: SV.Vector Int16 -> SV.Vector Int16
asVector16 = id

clip16 :: Float -> Float
clip16 =
   min (fromIntegral (maxBound::Int16)) .
   max (fromIntegral (minBound::Int16))

copyChunk ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1) =>
   Parameters -> Float -> Float ->
   SplitPath ar0 -> Int -> Path.File ar1 -> IO ()
copyChunk params amp scalProd (SplitPath poolDir poolName) offset dstPath = do
   PathIO.writeFile (dstPath <++> originSuffix) $
      show (scalProd, (Path.toString poolName, offset))
   withFilePath SV.writeFile (dstPath <++> chunkSuffix)
      . asVector16
      . SV.map (Real.roundSimple . clip16 . (*amp) . fromIntegral)
      =<< loadChunk params (poolDir </> poolName <++> chunkSuffix) offset

associateBucket ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1, PathC.AbsRel ar2) =>
   Parameters -> SplitPath ar0 -> SplitPath ar1 -> PathFmt.File ar2 -> IO ()
associateBucket params poolBucket srcBucket@(SplitPath srcDir srcName) dst = do

   dict <- createSpectrumMap params poolBucket
   files <- loadSpectra params srcBucket
   srcIndex <- loadIndex $ srcDir </> srcName <++> indexSuffix
   forM_ (zip files (map snd srcIndex)) $ \((spec, specIx), pos) -> do
      let srcNorm = norm2 spec
      let (scalProd, (poolNorm, matchingOffset)) =
            List.maximumBy (comparing fst) $
            map (mapFst (matchSpectrum spec)) dict
      let dstPath = PathFmt.printf dst pos
      createDirectoriesFor dstPath
      PathIO.writeFile (dstPath <++> targetSuffix) $
         show (Path.toString srcName, specIx)
      copyChunk params (srcNorm/poolNorm)
         scalProd poolBucket matchingOffset dstPath

merge :: (Ord a) => [a] -> [a] -> [(a,a)]
merge =
   let go (x:xs) yt@(y0:ys0) =
          if x <= y0
             then (x,y0) : go xs yt
             else
                case ys0 of
                   [] -> (x,y0) : []
                   y1:_ys1 ->
                      if x < y1
                         then (x,y0) : go xs ys0
                         else go (x:xs) ys0
       go [] _ = []
       go (_:_) [] = error "merge: second list empty"
   in  go

runAssociate ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1, PathC.AbsRel ar2) =>
   Parameters -> Path.Dir ar0 -> Path.Dir ar1 -> PathFmt.File ar2 -> IO ()
runAssociate params poolDir src dst = do
   let getSpectra :: (PathC.AbsRel ar) => Path.Dir ar -> IO [Path.RelFile]
       getSpectra =
         fmap (List.sort . mapMaybe maybeSpectrumSuffix) . getDirectoryContents
   srcDirs <- getSpectra src
   poolDirs <- getSpectra poolDir
   forM_ (merge srcDirs poolDirs) $ \(sdir,pdir) -> do
      associateBucket params (SplitPath poolDir pdir) (SplitPath src sdir) dst


mapUnionsWithKey ::
   (Ord k) => (k -> a -> a -> a) -> [Map k a] -> Map k a
mapUnionsWithKey f =
   foldl (Map.unionWithKey f) Map.empty

mapInverse :: (Ord a, Ord b, Show b) => Map a b -> Map b (Except b a)
mapInverse =
   Map.fromListWithKey keyClash .
   map (mapSnd success . swap) . Map.toList

runAdjacent ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1, PathC.AbsRel ar2) =>
   Verbosity -> Parameters -> Float ->
   Path.Dir ar0 -> Path.Dir ar1 -> Path.Dir ar2 -> IO ()
runAdjacent verbosity params cohesion poolDir srcDir dstDir = do
   originMap <-
      catchKeyClash "unions of origin maps" . mapUnionsWithKey keyClash
         =<< mapM (fmap (fmap success) . loadOriginMap . SplitPath poolDir)
         .   mapMaybe maybeSpectrumSuffix
         =<< getDirectoryContents poolDir

   indexMap <- catchKeyClash "index inversion" $ mapInverse originMap

   chunkNames <-
      fmap (List.sort . mapMaybe (maybeSuffix chunkSuffix)) $
      getDirectoryContents dstDir

   case chunkNames of
      [] -> ioError $ userError "no index file found"
      name0 : remIndices ->
         let chunkSize = paramChunkSize params
             maybeNextBetter srcName srcOffset srcSpec dstName scalProd jx =
                case Map.lookup jx indexMap of
                   Nothing -> return Nothing
                   Just (nextName, nextOffset) -> do
                      (nextSpec, nextNorm) <-
                         fmap normalizeSpectrum $
                         loadSpectrum chunkSize
                            (poolDir </> nextName <++> spectrumSuffix) nextOffset
                      let nextScalProd = matchSpectrum srcSpec nextSpec
                      Shell.debug verbosity $
                         printf "%s: %s+%d %s+%d %f %f\n"
                            (Path.toString dstName)
                            (Path.toString $ srcDir </> srcName) srcOffset
                            (Path.toString $ poolDir </> nextName) nextOffset
                            nextScalProd scalProd
                      return $
                         toMaybe (cohesion*nextScalProd > scalProd)
                            (nextName, nextOffset, nextScalProd, nextNorm)

             lookupOrigin ix =
                case Map.lookup ix originMap of
                   Just n -> return n
                   Nothing ->
                      ioError $ userError $
                         "original chunk for " ++ show ix ++ " not found"

             go _ [] = return ()
             go jx0 (dstName : is) = do
                (scalProd, origIx) <-
                   loadOriginIndex $ dstDir </> dstName <++> originSuffix

                (srcName, srcOffset) <-
                   loadTargetIndex $ dstDir </> dstName <++> targetSuffix

                srcSpec <-
                   loadSpectrum chunkSize
                      (srcDir </> srcName <++> spectrumSuffix) srcOffset

                m <- maybeNextBetter srcName srcOffset srcSpec dstName scalProd jx0
                jx1 <-
                   case m of
                      Nothing -> lookupOrigin origIx
                      Just (nextName, nextOffset, nextScalProd, nextNorm) -> do
                         Shell.info verbosity $ "replace " ++ Path.toString dstName
                         let dstPath = dstDir </> dstName
                         copyChunk params (norm2 srcSpec / nextNorm)
                            nextScalProd (SplitPath poolDir nextName)
                            nextOffset dstPath
                         return jx0
                go (mapSnd succ jx1) is

         in  do (_scalProd, origIx) <-
                   loadOriginIndex $ dstDir </> name0 <++> originSuffix
                ix <- lookupOrigin origIx
                go (mapSnd succ ix) remIndices


runCompose ::
   (PathC.AbsRel ar0, PathC.AbsRel ar1) =>
   Parameters -> Path.Dir ar0 -> Path.File ar1 -> IO ()
runCompose params src dst = do
   let shift = paramShift params
       overlap = paramOverlap params
       chunkSize = shift*overlap
       deinterleave xs =
          SV.deinterleave (div (SV.length xs) chunkSize) xs

       pilPost = pillow chunkSize

   chunks <-
      mapM (withFilePath SV.readFile . (src </>)) . List.sort
         . filter (isSuffixOf chunkSuffix)
         =<< getDirectoryContents src

   withFilePath SVL.writeFile dst $ SVL.interleaveFirstPattern $
      map (compose overlap shift) $ List.transpose $
      map
         (map (SVL.fromChunks . (:[]) . FiltNRG.envelope pilPost) .
          deinterleave . SV.map (Bin.toCanonical :: Int16 -> Float)) $
      chunks
