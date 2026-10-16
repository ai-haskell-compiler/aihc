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
  )
where

import Aihc.Play.Compile (compileProgram)
import Aihc.Play.Json (Json (..))
import Aihc.Play.Names (occurrenceJson, occurrences)
import Aihc.Play.Session
import Aihc.Play.Tree (abridgedTree, treeJson)
import Aihc.Play.Types (inferredSignatures, signatureJson, withSignatures)
import Aihc.Resolve (ModuleUnit (..), ResolvedModule (..))
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

runCommand :: Session -> Command -> Text -> Json
runCommand session command source =
  case command of
    CommandCompile -> compileProgram session source
    CommandParse ->
      let (problems, modu, _) = parseUserModule source
       in result [("tree", treeJson (abridgedTree modu))] problems
    CommandNames ->
      let (parseProblems, modu, extensions) = parseUserModule source
       in if not (null parseProblems)
            then result [("names", JArray [])] parseProblems
            else
              let (problems, resolved) = resolveUserModule session modu extensions
                  names = concatMap (occurrences . moduleUnitAst . resolvedModuleUnit) resolved
               in result [("names", JArray (map occurrenceJson names))] problems
    CommandTypes ->
      let (parseProblems, modu, extensions) = parseUserModule source
       in if not (null parseProblems)
            then result [("source", JString source), ("signatures", JArray [])] parseProblems
            else
              let (resolveProblems, resolved) = resolveUserModule session modu extensions
               in if not (null resolveProblems)
                    then result [("source", JString source), ("signatures", JArray [])] resolveProblems
                    else
                      let (problems, checked) = typecheckUserModule session resolved
                          signatures = concatMap (inferredSignatures (sessionWiring session)) checked
                       in result
                            [ ("source", JString (withSignatures source signatures)),
                              ("signatures", JArray (map signatureJson signatures))
                            ]
                            problems
  where
    result fields problems =
      JObject (("ok", JBool True) : fields <> [("errors", JArray (map problemJson problems))])
