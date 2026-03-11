module Main where

import qualified System.Process as Proc
import qualified System.IO as IO

import qualified Options.Applicative as OP
import Shell.Utility.Exit (exitFailureMsg)

import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ReadE as ReadE
import qualified Distribution.Package as Pkg
import qualified Distribution.Text as DistText

import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import Control.Monad (when, guard)
import Control.Applicative (pure, (<*>))


main :: IO ()
main = do
   (flags, pgkNames) <- OP.execParser $ info options
   Exc.resolveT (\e -> exitFailureMsg $ "Aborted: " ++ e) $
      mapM_ (Trans.lift . putStrLn) =<<
         (Exc.mapExceptionalT (flip State.evalStateT Set.empty) $
          fmap concat $ mapM (getAllDependencies flags True) pgkNames)


data Flags =
   Flags {
      optVerbosity :: Verbosity.Verbosity,
      optUser, optGlobal :: Bool,
      optShowVersions :: Bool,
      optPkgCmd :: String
   }

info :: OP.Parser a -> OP.ParserInfo (a, [String])
info p =
   OP.info
      (OP.helper <*>
         OP.liftA2 (,) p (OP.many (OP.strArgument (OP.metavar "PKG-NAME"))))
      (OP.fullDesc <>
       OP.progDesc "Find transitive dependences of installed GHC packages.")

options :: OP.Parser Flags
options =
   pure Flags
   <*>
      OP.option
         (OP.eitherReader $ ReadE.runReadE Verbosity.flagToVerbosity)
         (OP.short 'v' <>
          OP.long "verbose" <>
          OP.metavar "N" <>
          OP.value Verbosity.silent <>
          OP.help "verbosity level: 0..3")
   <*>
      OP.switch
         (OP.long "user" <>
          OP.help "query GHC's local user package database")
   <*>
      OP.switch
         (OP.long "global" <>
          OP.help "query GHC's global package database")
   <*>
      OP.switch
         (OP.long "show-versions" <>
          OP.help "show package version numbers in the output")
   <*>
      OP.strOption
         (OP.long "pkg-cmd" <>
          OP.metavar "COMMAND" <>
          OP.value defaultPkgCmd <>
          OP.help
            ("command for querying the package database (default: " ++
             defaultPkgCmd ++ ")"))

defaultPkgCmd :: String
defaultPkgCmd = "ghc-pkg"


type PkgName = String


getAllDependencies ::
   Flags -> Bool -> PkgName ->
   Exc.ExceptionalT String (State.StateT (Set.Set PkgName) IO) [PkgName]
getAllDependencies flags userSuppliedName name = do
   when (optVerbosity flags >= Verbosity.deafening)
      (Trans.lift $
         Trans.lift . print =<< State.get)
   b <- Trans.lift $ State.gets (Set.member name)
   if b
     then return []
     else
       Exc.catchT
          (do {- register this name _before_ calling ghc-pkg,
                 because ghc-pkg may not find the package,
                 and then we do not need to query again -}
              Trans.lift $ State.modify (Set.insert name)
              deps <-
                 Exc.mapExceptionalT Trans.lift $
                 getDirectDependencies flags name
              allDeps <- mapM (getAllDependencies flags False) deps
              let strippedName = fromMaybe name $ do
                     guard $ not $ optShowVersions flags
                     pname <-
                        fmap (Pkg.unPackageName . Pkg.pkgName) $
                        DistText.simpleParse name
                     return pname
              return $ concat allDeps ++ [strippedName])
          (\errTxt ->
             if not userSuppliedName &&
                B.isInfixOf (B.pack "cannot find") (B.pack errTxt)
               then return []
               else Exc.throwT errTxt)


getDirectDependencies ::
   Flags -> PkgName ->
   Exc.ExceptionalT String IO [PkgName]
getDirectDependencies flags name = do
   let cmd = optPkgCmd flags
       args =
          (if optUser   flags then ("--user"   :) else id) $
          (if optGlobal flags then ("--global" :) else id) $
          ["field", name, "depends"]
   when (optVerbosity flags >= Verbosity.verbose)
      (Trans.lift $ putStrLn $ unwords $ cmd : args)
   (inp,out,err,pid) <-
      Trans.lift (Proc.runInteractiveProcess cmd args Nothing Nothing)
   txt    <- Trans.lift $ B.hGetContents out
   errTxt <- Trans.lift $ B.hGetContents err
   when (optVerbosity flags >= Verbosity.normal) $
      Trans.lift $ putStr $ B.unpack errTxt
   Exc.mapExceptionT (\n ->
      "ghc-pkg exited with code " ++ show n ++ "\n" ++ B.unpack errTxt) $
         Exc.fromExitCodeT $
         Proc.waitForProcess pid
   Trans.lift (mapM_ IO.hClose [inp,out,err])
   case words (B.unpack txt) of
      "depends:":names ->
         {-
         If multiple packages are installed,
         then the dependencies of each are preceded by a 'depends:' prefix.

         ghc-pkg lists 'builtin_rts' as dependency of 'base',
         but does not accept this identifier as package name,
         since it contains an underscore.
         -}
         return $
            map stripHashAndPrivateLibName $
            filter (flip notElem ["depends:", "builtin_rts"]) names
      [] -> return []
      _ -> Exc.throwT $
              "unexpected output of ghc-pkg - " ++
              "it should start with 'depends:'" ++
              if optVerbosity flags >= Verbosity.verbose
                then "\n" ++ B.unpack txt
                else ""

stripHashAndPrivateLibName :: PkgName -> PkgName
stripHashAndPrivateLibName =
   (List.intercalate "-" . NonEmpty.flatten .
      NonEmpty.mapTail (ListHT.takeUntil isVersionNumber))
   .
   NonEmpty.chop ('-'==)

isVersionNumber :: String -> Bool
isVersionNumber =
   Fold.all (\component -> not (null component) && all Char.isDigit component) .
   NonEmpty.chop ('.'==)
