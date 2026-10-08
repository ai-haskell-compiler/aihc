-- | Whole-program compilation for @--lto@ builds.
--
-- A @--lto@ install stops each module at System FC. @build@ reads the
-- System FC of every module of the program, from the installed packages and
-- the executable alike, merges it into one program, and lowers that program
-- through GRIN and Lir to one object.
module Aihc.Cli.Lto
  ( compileLtoProgram,
    moduleCorePath,
    demoteToEntry,
    entryName,
    readPrograms,
  )
where

import Aihc.Cli.ArtifactCache (hashChunks, sourceFilesHash)
import Aihc.Cli.Install (FcModule (..), ModuleCompileConfig (..), ModuleOutputPaths (..), backendOptionsKey, compileFcModules, moduleOutputPaths, optimizeFcProgram)
import Aihc.Fc qualified as Fc
import Aihc.Native (NativeTarget, executableEntryParts)
import Aihc.Resolve (PackageId (..))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Control.Monad (forM_, unless, when)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

-- | The System FC file of a module under a package root or a build root.
moduleCorePath :: NativeTarget -> FilePath -> Text -> FilePath
moduleCorePath target root name = outputFcPath (moduleOutputPaths root target name)

-- | Merge the System FC files and compile the merged program to one object
-- under the build root. Returns the object.
--
-- The object follows the System FC files and the backend options. A build
-- whose inputs are the ones the object was built from reuses it.
compileLtoProgram :: ModuleCompileConfig -> FilePath -> [FilePath] -> IO FilePath
compileLtoProgram config buildRoot corePaths = do
  let verbose = compileVerbose config
      paths = moduleOutputPaths (buildRoot </> "lto") (compileTarget config) "program"
      object = outputObjectPath paths
      corePath = outputFcPath paths
      keepCore = compileKeepCore config
      stampPath = buildRoot </> "lto" </> "program.hash"
  forM_ corePaths $ \path -> do
    exists <- doesFileExist path
    unless exists (ioError (userError ("The System FC of a module of the program is absent: " <> path)))
  inputsHash <- sourceFilesHash "" corePaths
  let current = hashChunks (map BS8.pack [backendOptionsKey config, inputsHash])
  previous <- readStamp stampPath
  objectExists <- doesFileExist object
  -- A @--keep-core@ build also needs the merged System FC beside the
  -- object, so a reused object whose core file is gone is built again.
  coreKept <- if keepCore then doesFileExist corePath else pure True
  if objectExists && coreKept && previous == Just current
    then verbose ("Reuse program object: " <> object)
    else do
      programs <- readPrograms corePaths
      -- Nothing outside a whole program can name anything in it but the
      -- entry, so every other declaration is demoted before it is pruned.
      -- Visibility is what the passes downstream read as the roots of
      -- reachability, here and in GRIN alike, so stating it once here is
      -- what makes them sweep a whole program to the entry without a flag
      -- that says a whole program is what they are looking at.
      let merged = Fc.pruneProgram [entryName] (demoteToEntry entryName (Fc.mergePrograms programs))
      verbose ("Merge System FC: " <> show (length programs) <> " modules, " <> show (length (Fc.programDecls merged)) <> " reachable declarations")
      -- The whole program is known here, so the inliner keeps only the
      -- entry and what it reaches.
      optimized <- optimizeFcProgram config verbose (Just [entryName]) "program" merged
      -- Inlining drops the values that its copies made dead, and with them
      -- the last reference to a type or a constructor. Prune again, so that
      -- those constructors emit no info table.
      let pruned = Fc.pruneProgram [entryName] optimized
      verbose ("Prune System FC: " <> show (length (Fc.programDecls optimized)) <> " -> " <> show (length (Fc.programDecls pruned)) <> " declarations")
      when (compileLint config) $ do
        let errors = Fc.lintProgram pruned
        unless (null errors) (ioError (userError ("FC lint failed after pruning the program:\n" <> unlines (map (("    " <>) . show) errors))))
      createDirectoryIfMissing True (takeDirectory object)
      -- The merged program is what the backend compiles, so it is the
      -- System FC that @--keep-core@ keeps for a @--lto@ build.
      when keepCore $ do
        Fc.writeProgramFile corePath pruned
        verbose "Write FC: program"
      _ <- compileFcModules config {compileProfileUnit = compileProfileAllocations config} verbose (const paths) [FcModule "program" pruned]
      writeFile stampPath current
  pure object

-- | Make the entry the one declaration another unit can name.
--
-- A constructor and a type go private with the rest: an info table and a
-- layout of a whole program are named from inside it only.
demoteToEntry :: Fc.Name -> Fc.Program -> Fc.Program
demoteToEntry root program =
  program {Fc.programDecls = map demote (Fc.programDecls program)}
  where
    demote decl =
      case decl of
        Fc.DeclType declaration ->
          Fc.DeclType
            declaration
              { Fc.typeVis = Fc.Private,
                Fc.typeCons = [constructor {Fc.conVis = Fc.Private} | constructor <- Fc.typeCons declaration]
              }
        Fc.DeclSynonym declaration -> Fc.DeclSynonym declaration {Fc.synVis = Fc.Private}
        Fc.DeclAxiom declaration -> Fc.DeclAxiom declaration {Fc.axiomVis = Fc.Private}
        Fc.DeclVal declaration
          | Fc.valName declaration == root -> decl
          | otherwise -> Fc.DeclVal declaration {Fc.valVis = Fc.Private}
        Fc.DeclRule {} -> decl

-- | The global that the entry archive calls: the root of the program.
entryName :: Fc.Name
entryName =
  Fc.Name
    { Fc.nameText = name,
      Fc.nameSort = Fc.SortValue,
      Fc.nameOrigin = Fc.OriginTop (PackageId package) moduleName
    }
  where
    (package, moduleName, name) = executableEntryParts

-- | Read the System FC files of a program. Only one file for each
-- capability is read at a time. The binary format gives a program that
-- is fully evaluated and has one object for each distinct name and type.
readPrograms :: [FilePath] -> IO [Fc.Program]
readPrograms paths = do
  capabilities <- getNumCapabilities
  slots <- newQSem (max 1 capabilities)
  forConcurrently paths (bracket_ (waitQSem slots) (signalQSem slots) . readProgram)

readProgram :: FilePath -> IO Fc.Program
readProgram path = do
  loaded <- Fc.readProgramFile path
  case loaded of
    Left message -> ioError (userError ("Invalid System FC file " <> T.unpack message))
    Right program -> pure program

readStamp :: FilePath -> IO (Maybe String)
readStamp path = do
  exists <- doesFileExist path
  if exists then Just <$> readFile path else pure Nothing
