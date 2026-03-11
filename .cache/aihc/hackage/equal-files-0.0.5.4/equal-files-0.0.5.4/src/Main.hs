module Main where

import qualified EqualFiles

import qualified System.FilePath.Find as FindFile
import System.FilePath.Find ((==?), )

import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.Exit (exitWith, ExitCode(..), )
import System.IO.Error (isFullError, )
import Control.Exception (try, )

import Control.Monad (liftM, when, )
import Control.Monad.Trans.Class (lift, )

import qualified Control.Monad.Exception.Synchronous as Exc


parseCard :: String -> Exc.Exceptional String Int
parseCard str =
   case reads str of
      [(n,"")] ->
         if n>=0
           then return n
           else Exc.throw $ "negative number: " ++ str
      _ -> Exc.throw $ "could not parse cardinal " ++ show str


elaborateOnExhaustedHandles :: IO a -> Exc.ExceptionalT String IO a
elaborateOnExhaustedHandles action =
   Exc.catchT
      (Exc.fromEitherT $ try $ action)
      (\ioe -> do
          when (isFullError ioe)
             (Exc.throwT $ unlines $
                 show ioe :
                 "--presort option may help against too many open files." :
                 [])
          Exc.throwT (show ioe))


data Flags =
   Flags {
      optHelp      :: Bool,
      optRecursive :: Bool,
      optPreSort   :: Int
   }

options :: [OptDescr (Flags -> Exc.Exceptional String Flags)]
options =
   Option ['h'] ["help"]      (NoArg  (\flags -> return $ flags{optHelp      = True})) "show options" :
   Option ['r'] ["recursive"] (NoArg  (\flags -> return $ flags{optRecursive = True})) "descent recursively into directories" :
   Option ['p'] ["presort"]
      (ReqArg (\str flags -> fmap (\n -> flags{optPreSort = n}) $ Exc.mapException ("presort option:\n"++) $ parseCard str) "N")
      "Sort all files according to their first N bytes in a first go.\nThis reduces the number of simultaneously open files.\nThe suggested size value is 512." :
   []


-- output results in a quoted way that might be accessible to further shell scripts
main :: IO ()
main =
   Exc.resolveT (\e -> putStr $ "Aborted: " ++ e ++ "\n") $ do
      argv <- lift getArgs
      let (opts, filesAndDirs, errors) = getOpt RequireOrder options argv
      when (not (null errors)) $ Exc.throwT $ concat $ errors
      flags <-
         Exc.ExceptionalT $ return $
            foldr (flip (>>=))
               (return $
                Flags {optHelp = False,
                       optRecursive = False,
                       optPreSort = 0})
               opts
      when (optHelp flags)
         (lift $
          getProgName >>= \programName ->
          putStrLn
             (usageInfo ("Usage: " ++ programName ++ " [OPTIONS] FILES-AND-DIRECTORIES ...") options) >>
          exitWith ExitSuccess)

      -- ToDo: speed up by pre-sorting with respect to file size
      -- ToDo: save memory by converting filepaths to Text
      files <-
         if optRecursive flags
           then lift $ liftM concat $
                mapM
                   (FindFile.find FindFile.always
                      (FindFile.fileType ==? FindFile.RegularFile))
                   filesAndDirs
           else return filesAndDirs

      elaborateOnExhaustedHandles $
          mapM_ (putStrLn . unwords . map show) =<<
         let prefixSize = optPreSort flags
         in  if prefixSize>0
               then EqualFiles.clusterWithPreSort prefixSize files
               else EqualFiles.clusterLeaveOpen files
