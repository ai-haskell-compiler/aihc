{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Group
    (groupStringTemplates, addSuperGroup, addSubGroup, setEncoderGroup,
     mergeSTGroups, directoryGroup, directoryGroupExt, optInsertGroup,
     directoryGroupLazy, directoryGroupLazyExt, directoryGroupRecursive,
     directoryGroupRecursiveExt, directoryGroupRecursiveLazy,
     directoryGroupRecursiveLazyExt,
     unsafeVolatileDirectoryGroup, nullGroup
    ) where
import Control.Applicative
import Control.Arrow
import qualified Control.Exception as CE
import Control.Monad
import Data.Monoid
import Data.List
import System.FilePath
import System.Directory
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.IO.Error
import qualified Data.Map as M
import Data.Time

import Text.StringTemplate.Base
import Text.StringTemplate.Classes

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = (<$>) . (<$>)

readFileUTF :: FilePath -> IO String
readFileUTF f = do
   h <- openFile f ReadMode
   hSetEncoding h utf8
   hGetContents h

readFileStrictly :: FilePath -> IO String
readFileStrictly f = do
   x <- readFileUTF f
   length x `seq` return x

groupFromFiles :: Stringable a => (FilePath -> IO String) -> [(FilePath,String)] -> IO (STGroup a)
groupFromFiles rf fs = groupStringTemplates <$> forM fs  (\(f,fname) -> do
     stmp <- newSTMP <$> rf f
     return (fname, stmp))

getTmplsRecursive :: FilePath -> FilePath -> FilePath -> IO [(FilePath, FilePath)]
getTmplsRecursive ext base fp = do
          dirContents <- filter (not . isPrefixOf ".") <$> getDirectoryContents fp
          subDirs <- filterM (doesDirectoryExist . (fp </>)) dirContents
          subs <- concat <$> mapM (\x -> getTmplsRecursive ext (base </> x) (fp </> x)) subDirs
          return $ (map ((fp </>) &&& (\x -> base </> dropExtension x)) $
                    filter ((ext ==) . takeExtension) dirContents)
                   ++ subs

{--------------------------------------------------------------------
  Group API
--------------------------------------------------------------------}

-- | Given a list of named of StringTemplates, returns a group which generates
-- them such that they can call one another.
groupStringTemplates :: [(String,StringTemplate a)] -> STGroup a
groupStringTemplates xs = newGen
    where newGen s = StFirst (M.lookup s ng)
          ng = M.fromList $ map (second $ inSGen (`mappend` newGen)) xs

-- | Given a path, returns a group which generates all files in said directory
-- which have the proper \"st\" extension.
-- This function is strict, with all files read once. As it performs file IO,
-- expect it to throw the usual exceptions.
directoryGroup :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroup = directoryGroupExt ".st"

-- | Given a path, returns a group which generates all files in said directory which have the supplied extension.
directoryGroupExt :: (Stringable a) => FilePath -> FilePath -> IO (STGroup a)
directoryGroupExt ext path =
    groupFromFiles readFileStrictly .
    map ((</>) path &&& takeBaseName) . filter ((ext ==) . takeExtension) =<<
    getDirectoryContents path

-- | Given a path, returns a group which generates all files in said directory
-- which have the proper \"st\" extension.
-- This function is lazy in the same way that readFile is lazy, with all
-- files read on demand, but no more than once. The list of files, however,
-- is generated at the time the function is called. As this performs file IO,
-- expect it to throw the usual exceptions. And, as it is lazy, expect
-- these exceptions in unexpected places.
directoryGroupLazy :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroupLazy = directoryGroupLazyExt ".st"

-- | Given a path, returns a group which generates all files in said directory which have the supplied extension.
directoryGroupLazyExt :: (Stringable a) => FilePath -> FilePath -> IO (STGroup a)
directoryGroupLazyExt ext path =
    groupFromFiles readFileUTF .
    map ((</>) path &&& takeBaseName) . filter ((ext ==) . takeExtension) =<<
    getDirectoryContents path

-- | As with 'directoryGroup', but traverses subdirectories as well. A template named
-- \"foo\/bar.st\" may be referenced by \"foo\/bar\" in the returned group.
directoryGroupRecursive :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroupRecursive = directoryGroupRecursiveExt ".st"

-- | As with 'directoryGroupRecursive', but a template extension is supplied.
directoryGroupRecursiveExt :: (Stringable a) => FilePath -> FilePath -> IO (STGroup a)
directoryGroupRecursiveExt ext path = groupFromFiles readFileStrictly =<< getTmplsRecursive ext "" path

-- | See documentation for 'directoryGroupRecursive'.
directoryGroupRecursiveLazy :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroupRecursiveLazy = directoryGroupRecursiveLazyExt ".st"

-- | As with 'directoryGroupRecursiveLazy', but a template extension is supplied.
directoryGroupRecursiveLazyExt :: (Stringable a) => FilePath -> FilePath -> IO (STGroup a)
directoryGroupRecursiveLazyExt ext path = groupFromFiles readFileUTF =<< getTmplsRecursive ext "" path

-- | Adds a supergroup to any StringTemplate group such that templates from
-- the original group are now able to call ones from the supergroup as well.
addSuperGroup :: STGroup a -> STGroup a -> STGroup a
addSuperGroup f g = inSGen (`mappend` g) <$$> f

-- | Adds a \"subgroup\" to any StringTemplate group such that templates from
-- the original group now have template calls \"shadowed\" by the subgroup.
addSubGroup :: STGroup a -> STGroup a -> STGroup a
addSubGroup f g = inSGen (g `mappend`) <$$> f

-- | Merges two groups into a single group. This function is left-biased,
-- prefering bindings from the first group when there is a conflict.
mergeSTGroups :: STGroup a -> STGroup a -> STGroup a
mergeSTGroups f g = addSuperGroup f g `mappend` addSubGroup g f

-- | Adds a set of global options to a group
optInsertGroup :: [(String, String)] -> STGroup a -> STGroup a
optInsertGroup opts f = (inSGen (optInsertGroup opts) . optInsertTmpl opts) <$$> f

-- | Sets an encoding function of a group that all values are
-- rendered with in each enclosed template
setEncoderGroup :: (Stringable a) => (a -> a) ->  STGroup a -> STGroup a
setEncoderGroup x f = (inSGen (setEncoderGroup x) . setEncoder x) <$$> f

-- | For any requested template, returns a message that the template was
-- unable to be found. Useful to add as a super group for a set of templates
-- under development, to aid in debugging.
nullGroup :: Stringable a => STGroup a
nullGroup x = StFirst . Just . newSTMP $ "Could not find template: " ++ x

-- | Given an integral amount of seconds and a path, returns a group generating
-- all files in said directory and subdirectories with the proper \"st\" extension,
-- cached for that amount of seconds. IO errors are \"swallowed\" by this so
-- that exceptions don't arise in unexpected places.
-- This violates referential transparency, but can be very useful in developing
-- templates for any sort of server application. It should be swapped out for
-- production purposes. The dumpAttribs template is added to the returned group
-- by default, as it should prove useful for debugging and developing templates.
unsafeVolatileDirectoryGroup :: Stringable a => FilePath -> Int -> IO (STGroup a)
unsafeVolatileDirectoryGroup path m = return . flip addSubGroup extraTmpls $ cacheSTGroup stfg
    where stfg = StFirst . Just . newSTMP . unsafePerformIO . flip CE.catch
                       (return . (\e -> "IO Error: " ++ show (ioeGetFileName e) ++ " -- " ++ ioeGetErrorString e))
                 . readFileUTF . (path </>) . (++".st")
          extraTmpls = addSubGroup (groupStringTemplates [("dumpAttribs", dumpAttribs)]) nullGroup
          delayTime :: Double
          delayTime = fromIntegral m
          cacheSTGroup :: STGroup a -> STGroup a
          cacheSTGroup g = unsafePerformIO $ do
                             !ior <- newIORef M.empty
                             return $ \s -> unsafePerformIO $ do
                               mp  <- readIORef ior
                               curtime <- getCurrentTime
                               let udReturn now = do
                                       let st = g s
                                       atomicModifyIORef ior $
                                         flip (,) () . M.insert s (now, st)
                                       return st
                               case M.lookup s mp of
                                 Nothing -> udReturn curtime
                                 Just (t, st) ->
                                     if (realToFrac $
                                               diffUTCTime curtime t) > delayTime
                                       then udReturn curtime
                                       else return st
