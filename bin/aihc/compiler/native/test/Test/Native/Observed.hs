{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Observed
  ( ObservedProgram (..),
    renderObservedMetadata,
    snapshotSourcePath,
  )
where

import Aihc.Grin.Syntax
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

data ObservedProgram = ObservedProgram
  { observedObject :: !BL.ByteString,
    observedMetadataSource :: !Text
  }
  deriving (Eq, Show)

renderObservedMetadata ::
  (GrinRep -> error) ->
  (FunctionName -> Either error Text) ->
  (Text -> Text) ->
  (Text -> Text) ->
  GrinProgram ->
  [GrinRep] ->
  Either error Text
renderObservedMetadata unsupportedRep functionLabel constructorLabel cSymbol program resultReps = do
  renderedResultReps <- mapM snapshotRepName resultReps
  constructors <- mapM renderConstructorDescriptor constructorEntries
  functions <- mapM renderFunctionDescriptor functionEntries
  pure . T.unlines $
    [ "#include \"aihc_snapshot.h\"",
      "#include <stddef.h>",
      ""
    ]
      <> map renderFunctionDeclaration functions
      <> [""]
      <> concatMap renderConstructorRepDeclaration constructors
      <> concatMap renderFunctionRepDeclaration functions
      <> renderRepDeclaration "result_reps" renderedResultReps
      <> renderConstructorTable constructors
      <> renderFunctionTable functions
      <> [ "void aihc_snapshot_dump_result(uint64_t count, const AihcSlot *values, const AihcMachine *machine) {",
           "  aihc_snapshot_dump(count, values, " <> pointerOrNull renderedResultReps "result_reps" <> ",",
           "                     aihc_allocation_count(machine),",
           "                     " <> tshow (length constructors) <> ", " <> pointerOrNull constructors "constructors" <> ",",
           "                     " <> tshow (length functions) <> ", " <> pointerOrNull functions "functions" <> ");",
           "}"
         ]
  where
    layouts =
      Map.fromList [(name, concat argumentLayouts) | (name, argumentLayouts) <- grinConstructors program]
    constructorEntries =
      [ (index, name, fields)
      | (index, (name, fields)) <- zip [0 :: Int ..] (Map.toAscList layouts)
      ]
    functionEntries =
      [ (grinFunctionName function, map grinVarRuntimeRep (grinFunctionParameters function))
      | function <- grinFunctions program
      ]

    renderConstructorDescriptor (identifier, name, fields) = do
      reps <- mapM snapshotRepName fields
      pure (identifier, name, reps)

    renderFunctionDescriptor (name, parameters) = do
      label <- functionLabel name
      reps <- mapM snapshotRepName parameters
      pure (name, label, reps)

    renderFunctionDeclaration (_, label, _) =
      "extern void " <> cSymbol label <> "(void);"

    renderConstructorRepDeclaration (identifier, _, reps) =
      renderRepDeclaration ("constructor_reps_" <> tshow identifier) reps

    renderFunctionRepDeclaration (_, label, reps) =
      renderRepDeclaration ("function_reps_" <> cSymbol label) reps

    renderConstructorTable [] = []
    renderConstructorTable constructors =
      ["extern const char " <> cSymbol (constructorLabel name) <> "[];" | (_, name, _) <- constructors]
        <> ["static const AihcSnapshotConstructor constructors[] = {"]
        <> [ "  {"
               <> "(uintptr_t)&"
               <> cSymbol (constructorLabel name)
               <> ", "
               <> cString name
               <> ", "
               <> tshow (length reps)
               <> ", "
               <> pointerOrNull reps ("constructor_reps_" <> tshow identifier)
               <> "},"
           | (identifier, name, reps) <- constructors
           ]
        <> ["};"]

    renderFunctionTable [] = []
    renderFunctionTable functions =
      ["static const AihcSnapshotFunction functions[] = {"]
        <> [ "  {(uintptr_t)&"
               <> cSymbol label
               <> ", "
               <> cString (unFunctionName name)
               <> ", "
               <> tshow (length reps)
               <> ", "
               <> pointerOrNull reps ("function_reps_" <> cSymbol label)
               <> "},"
           | (name, label, reps) <- functions
           ]
        <> ["};"]

    snapshotRepName runtimeRep =
      case runtimeRep of
        BoxedRep {} -> pure "AIHC_SNAPSHOT_POINTER"
        SumRep {} -> pure "AIHC_SNAPSHOT_POINTER"
        IntRep -> pure "AIHC_SNAPSHOT_INT"
        Int8Rep -> pure "AIHC_SNAPSHOT_INT8"
        Int16Rep -> pure "AIHC_SNAPSHOT_INT16"
        Int32Rep -> pure "AIHC_SNAPSHOT_INT32"
        Int64Rep -> pure "AIHC_SNAPSHOT_INT64"
        WordRep -> pure "AIHC_SNAPSHOT_WORD"
        Word8Rep -> pure "AIHC_SNAPSHOT_WORD8"
        Word16Rep -> pure "AIHC_SNAPSHOT_WORD16"
        Word32Rep -> pure "AIHC_SNAPSHOT_WORD32"
        Word64Rep -> pure "AIHC_SNAPSHOT_WORD64"
        AddrRep -> pure "AIHC_SNAPSHOT_ADDR"
        FloatRep -> pure "AIHC_SNAPSHOT_FLOAT"
        DoubleRep -> pure "AIHC_SNAPSHOT_DOUBLE"
        _ -> Left (unsupportedRep runtimeRep)

renderRepDeclaration :: Text -> [Text] -> [Text]
renderRepDeclaration _ [] = []
renderRepDeclaration name reps =
  [ "static const AihcSnapshotRep "
      <> name
      <> "[] = {"
      <> T.intercalate ", " reps
      <> "};"
  ]

pointerOrNull :: [value] -> Text -> Text
pointerOrNull values name
  | null values = "NULL"
  | otherwise = name

cString :: Text -> Text
cString value = "\"" <> T.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape character = T.singleton character

tshow :: (Show value) => value -> Text
tshow = T.pack . show

snapshotSourcePath :: IO FilePath
snapshotSourcePath = getCurrentDirectory >>= findRoot
  where
    findRoot directory = do
      let candidate = directory </> "compiler" </> "native" </> "test" </> "Test" </> "Runtime" </> "aihc_snapshot.c"
      exists <- doesFileExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then ioError (userError "The native snapshot test source is missing.")
            else findRoot parent
