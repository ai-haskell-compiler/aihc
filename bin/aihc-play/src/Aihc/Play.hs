{-# LANGUAGE OverloadedStrings #-}

-- | The commands of the helper.
--
-- Each command takes the source of one module and gives one JSON value.
module Aihc.Play
  ( Command (..),
    parseCommand,
    runCommand,
    Session,
    bundleSession,
    sessionBundleStats,
    SpliceHost (..),
    noSpliceHost,
  )
where

import Aihc.Play.Compile (compileProgram)
import Aihc.Play.Json (Json (..))
import Aihc.Play.Names (occurrenceJson, occurrences)
import Aihc.Play.Session
import Aihc.Play.Splices (SpliceHost (..), expandUserSplices, noSpliceHost)
import Aihc.Play.Tree (abridgedTree, treeJson)
import Aihc.Play.Types (inferredSignatures, signatureJson, withSignatures)
import Aihc.Resolve (Package (..), PackageId (..), ResolvedModule (..))
import Data.Text (Text)

data Command
  = -- | The abridged parse tree.
    CommandParse
  | -- | Every name occurrence with its entity.
    CommandNames
  | -- | The source with the inferred signatures.
    CommandTypes
  | -- | The WASM assembly of an executable.
    CommandCompile
  deriving (Eq, Show)

parseCommand :: Text -> Maybe Command
parseCommand name =
  case name of
    "parse" -> Just CommandParse
    "names" -> Just CommandNames
    "types" -> Just CommandTypes
    "compile" -> Just CommandCompile
    _ -> Nothing

-- | Run a command. Each command apart from @parse@ runs the Template
-- Haskell splices of the module first, with the host.
runCommand :: Session -> SpliceHost -> Command -> Text -> IO Json
runCommand session host command source =
  case command of
    CommandParse -> pure (result [("tree", treeJson (abridgedTree parsed))] parseProblems)
    CommandCompile
      | not (null parseProblems) -> pure (result [] parseProblems)
      | otherwise -> expanded (Package "exe" (PackageId "exe")) [] $ \modu -> compileProgram session modu extensions
    CommandNames
      | not (null parseProblems) -> pure (result [("names", JArray [])] parseProblems)
      | otherwise -> expanded userPackage [("names", JArray [])] $ \modu ->
          let (problems, resolved) = resolveUserModule session modu extensions
              names = concatMap (occurrences . resolvedModuleSyntax) resolved
           in result [("names", JArray (map occurrenceJson names))] problems
    CommandTypes
      | not (null parseProblems) -> pure (result noSignatures parseProblems)
      | otherwise -> expanded userPackage noSignatures $ \modu ->
          let (resolveProblems, resolved) = resolveUserModule session modu extensions
           in if not (null resolveProblems)
                then result noSignatures resolveProblems
                else
                  let (problems, checked) = typecheckUserModule session resolved
                      signatures = concatMap (inferredSignatures (sessionWiring session)) checked
                   in result
                        [ ("source", JString (withSignatures source signatures)),
                          ("signatures", JArray (map signatureJson signatures))
                        ]
                        problems
  where
    (parseProblems, parsed, extensions) = parseUserModule source
    noSignatures = [("source", JString source), ("signatures", JArray [])]
    -- Run the splices, then the command. A problem of a splice gives the
    -- empty fields of the command.
    expanded package empty continue = do
      outcome <- expandUserSplices session host package (withQuoteImports parsed) extensions
      pure $ case outcome of
        Left problems -> result empty problems
        Right modu -> continue modu
    result fields problems =
      JObject (("ok", JBool True) : fields <> [("errors", JArray (map problemJson problems))])
