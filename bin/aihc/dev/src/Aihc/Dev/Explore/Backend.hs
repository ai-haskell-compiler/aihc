-- | The backend views of the explorer: the Lir that the backend lowers from
-- GC-GRIN, the assembly of the target, and the LLVM IR.
--
-- The views come from the same functions as the build. The object backends
-- write machine code without assembly text, so the assembly view comes from
-- their text printers, which give the same instructions.
module Aihc.Dev.Explore.Backend
  ( backendDocuments,
  )
where

import Aihc.Amd64.Lir qualified as Amd64
import Aihc.Amd64.Text (renderAmd64Statements)
import Aihc.Arm64.Lir qualified as Arm64
import Aihc.Arm64.Text (renderArm64Statements)
import Aihc.Cli.Backend (lowerTargetFor)
import Aihc.Dev.Explore.Document (Definition (..), DefinitionKind (..), Document, Section (..), Segment (..), Stage (..), documentFromSections)
import Aihc.Dev.Explore.TextMate (Token (..), lirGrammar, tokenizeLines)
import Aihc.Grin qualified as Grin
import Aihc.Lir.Lower qualified as Lower
import Aihc.Lir.Pretty (renderModule)
import Aihc.Lir.Syntax (DataItem (..), Function (..), Global (..), Item (..), Module (..), Symbol (..))
import Aihc.Llvm.Lir qualified as Llvm
import Aihc.Native (NativeTarget (..), renderLinkedFunctionSymbol, renderLinkedGlobalSymbol)
import Aihc.Wasm.Lir qualified as Wasm
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T

-- | The Lir, the assembly and the LLVM IR of a GC-GRIN program. The Lir and
-- the assembly are for the target of the build. The LLVM IR comes from the
-- Lir that the LLVM target lowers.
backendDocuments :: NativeTarget -> Lower.ModuleSettings -> Grin.GcGrinProgram -> Map Stage Document
backendDocuments target settings program =
  Map.fromList
    [ (StageLir, either errorDocument (lirDocument symbols) targetLir),
      (StageAssembly, either errorDocument (assemblyDocument symbols) (targetLir >>= assemblyText target)),
      (StageLlvm, either errorDocument (llvmDocument symbols) (lowerFor Llvm >>= llvmText))
    ]
  where
    symbols = symbolDefinitions (Grin.gcGrinProgram program)
    lowerFor native = either (Left . T.pack . show) Right (Lower.lowerModule (lowerTargetFor native) settings program)
    targetLir = lowerFor target

-- | The definition of each Lir symbol that comes from a GRIN function or a
-- GRIN global. The table uses the same symbol functions as the lowering.
symbolDefinitions :: Grin.GrinProgram -> Map Text Definition
symbolDefinitions program =
  Map.fromList $
    [ ("aihc_f_" <> renderLinkedFunctionSymbol name, definitionOf DefinitionCode name)
    | function <- Grin.grinFunctions program,
      let name = Grin.unFunctionName (Grin.grinFunctionName function)
    ]
      <> [ (renderLinkedGlobalSymbol name, definitionOf DefinitionData name)
         | global <- Grin.grinGlobals program,
           let name = Grin.grinGlobalName global
         ]
  where
    definitionOf kind name =
      case Grin.grinNameScope name of
        Just (scope, baseName) -> Definition (Just (Grin.grinScopeModule scope)) baseName kind
        Nothing -> Definition Nothing name kind

-- | The definition of a symbol. A symbol that no GRIN name gives, such as a
-- helper of the runtime, is a definition with the symbol as its name.
lookupSymbol :: Map Text Definition -> DefinitionKind -> Text -> Definition
lookupSymbol symbols kind symbol =
  Map.findWithDefault (Definition Nothing symbol kind) symbol symbols

errorDocument :: Text -> Document
errorDocument message = documentFromSections (map (\line -> [Segment line "invalid"])) [Section Nothing (T.lines message)]

-- | One section for each Lir item.
lirDocument :: Map Text Definition -> Module -> Document
lirDocument symbols lirModule =
  documentFromSections
    (map (map tokenSegment) . tokenizeLines lirGrammar)
    [Section (itemDefinition item) (T.lines (renderModule (Module [item]))) | item <- moduleItems lirModule]
  where
    itemDefinition item =
      case item of
        ItemFunction function -> Just (lookupSymbol symbols DefinitionCode (unSymbol (functionName function)))
        ItemGlobal global -> Just (lookupSymbol symbols DefinitionData (unSymbol (globalName global)))
        ItemData value -> Just (lookupSymbol symbols DefinitionData (unSymbol (dataName value)))
        _ -> Nothing
    tokenSegment token = Segment (tokenText token) (if null (tokenScopes token) then "" else last (tokenScopes token))

-- | The assembly text of the target. The LLVM target has no assembly of its
-- own: Clang makes the object from the LLVM IR.
assemblyText :: NativeTarget -> Module -> Either Text Text
assemblyText target lirModule =
  case target of
    AppleArm64 -> either (Left . T.pack . show) (Right . renderArm64Statements) (Arm64.compileLirStatements lirModule)
    LinuxAmd64 -> either (Left . T.pack . show) (Right . renderAmd64Statements) (Amd64.compileLirStatements lirModule)
    Wasm32Wasip3 -> either (Left . T.pack . show) Right (Wasm.compileLirModule lirModule)
    Llvm -> Left "The LLVM target has no assembly. Clang compiles the LLVM IR of view 8."

llvmText :: Module -> Either Text Text
llvmText lirModule = either (Left . T.pack . show) Right (Llvm.compileLirModule lirModule)

-- | Split the assembly at the label of each symbol. The directives just
-- before a label, such as @.globl@ and @.p2align@, go with the label.
assemblyDocument :: Map Text Definition -> Text -> Document
assemblyDocument symbols text =
  documentFromSections (map assemblySegments) (splitSections startOf directive (T.lines text))
  where
    startOf line = do
      label <- T.stripSuffix ":" line
      let symbol = T.dropWhile (== '"') (T.dropWhileEnd (== '"') label)
          -- A Darwin symbol has a leading underscore.
          plain = fromMaybe symbol (T.stripPrefix "_" symbol)
      if T.null label || T.any isSpace label
        then Nothing
        else Just $ case (Map.lookup symbol symbols, Map.lookup plain symbols) of
          (Just definition, _) -> definition
          (_, Just definition) -> definition
          _ -> Definition Nothing plain DefinitionCode
    directive line = "\t." `T.isPrefixOf` line || "  ." `T.isPrefixOf` line

-- | Split the LLVM IR at each function definition and each global.
llvmDocument :: Map Text Definition -> Text -> Document
llvmDocument symbols text =
  documentFromSections (map llvmSegments) (splitSections startOf (const False) (T.lines text))
  where
    startOf line
      | "define " `T.isPrefixOf` line = definitionAfter DefinitionCode (snd (T.breakOn "@" line))
      | "@" `T.isPrefixOf` line = definitionAfter DefinitionData line
      | otherwise = Nothing
    definitionAfter kind rest = do
      symbol <- llvmSymbol rest
      pure (lookupSymbol symbols kind symbol)

-- | The name of an LLVM global reference: @\@name@ or @\@"name"@.
llvmSymbol :: Text -> Maybe Text
llvmSymbol text = do
  rest <- T.stripPrefix "@" text
  case T.stripPrefix "\"" rest of
    Just quoted -> Just (T.takeWhile (/= '"') quoted)
    Nothing ->
      let name = T.takeWhile (\character -> isAlphaNum character || character `elem` ("_.$-" :: String)) rest
       in if T.null name then Nothing else Just name

-- | Split lines into sections. A line that starts a section has its
-- definition. The lines just before it that the predicate accepts go with
-- it.
splitSections :: (Text -> Maybe Definition) -> (Text -> Bool) -> [Text] -> [Section]
splitSections startOf attached = finish . foldl step (Section Nothing [], [])
  where
    step (current, done) line =
      case startOf line of
        Nothing -> (current {sectionLines = line : sectionLines current}, done)
        Just definition ->
          let (moved, kept) = span attached (sectionLines current)
              previous = current {sectionLines = kept}
           in (Section (Just definition) (line : moved), previous : done)
    -- The explorer puts one blank line between two sections, so the blank
    -- lines at the ends of a section go.
    finish (current, done) =
      [ section {sectionLines = trimmed}
      | section <- reverse (current : done),
        let trimmed = dropWhile T.null (reverse (dropWhile T.null (sectionLines section))),
        not (null trimmed) || isJust (sectionDefinition section)
      ]

-- | Highlight a line of assembly: labels, directives, mnemonics, numbers,
-- strings and comments.
assemblySegments :: Text -> [Segment]
assemblySegments line
  | Just label <- T.stripSuffix ":" line, not (T.any isSpace label) = [Segment line "entity.name.function"]
  | otherwise =
      let (indentation, rest) = T.span isSpace line
          (first, operands) = T.break isSpace rest
          firstClass = if "." `T.isPrefixOf` first then "keyword.control" else "keyword"
       in [Segment indentation "" | not (T.null indentation)]
            <> [Segment first firstClass | not (T.null first)]
            <> operandSegments operands

-- | Highlight a line of LLVM IR.
llvmSegments :: Text -> [Segment]
llvmSegments = operandSegments

-- | Highlight the words of a line: comments, strings, numbers, symbols,
-- registers and keywords.
operandSegments :: Text -> [Segment]
operandSegments text =
  case T.uncons text of
    Nothing -> []
    Just (character, rest)
      | character == ';' || "//" `T.isPrefixOf` text -> [Segment text "comment"]
      | character == '"' ->
          let (body, after) = T.breakOn "\"" rest
              closed = T.take 1 after
           in Segment (T.cons '"' (body <> closed)) "string" : operandSegments (T.drop 1 after)
      | character == '@' || character == '%' ->
          let (name, after) = symbolSpan rest
           in Segment (T.cons character name) (if character == '@' then "variable.other.global" else "variable.other") : operandSegments after
      | character == '#' || isDigit character || (character == '-' && maybe False (isDigit . fst) (T.uncons rest)) ->
          let (number, after) = T.span (\value -> isAlphaNum value || value `elem` ("#-." :: String)) text
           in Segment number "constant.numeric" : operandSegments after
      | isAlphaNum character || character `elem` ("_.$" :: String) ->
          let (word, after) = T.span (\value -> isAlphaNum value || value `elem` ("_.$" :: String)) text
           in Segment word (wordClass word) : operandSegments after
      | otherwise ->
          let (other, after) = T.break (\value -> isAlphaNum value || value `elem` ("\"@%#;_.$-" :: String)) rest
           in Segment (T.cons character other) "" : operandSegments after
  where
    symbolSpan rest =
      case T.uncons rest of
        Just ('"', quoted) ->
          let (body, after) = T.breakOn "\"" quoted
           in ("\"" <> body <> T.take 1 after, T.drop 1 after)
        _ -> T.span (\value -> isAlphaNum value || value `elem` ("_.$-" :: String)) rest
    wordClass word
      | word `elem` llvmKeywords = "keyword"
      | word `elem` llvmTypes || isLlvmIntegerType word = "storage.type"
      | isRegister word = "variable.other"
      | otherwise = ""
    isLlvmIntegerType word = case T.uncons word of
      Just ('i', digits) -> not (T.null digits) && T.all isDigit digits
      _ -> False
    isRegister word = case T.uncons word of
      Just (prefix, digits) | prefix `elem` ("xwdsqv" :: String) -> not (T.null digits) && T.all isDigit digits
      _ -> word `elem` ["sp", "xzr", "wzr", "lr", "fp"]

llvmKeywords :: [Text]
llvmKeywords =
  [ "define",
    "declare",
    "global",
    "constant",
    "private",
    "internal",
    "external",
    "hidden",
    "unnamed_addr",
    "align",
    "ret",
    "br",
    "switch",
    "call",
    "musttail",
    "tail",
    "load",
    "store",
    "getelementptr",
    "inbounds",
    "alloca",
    "phi",
    "select",
    "icmp",
    "fcmp",
    "add",
    "sub",
    "mul",
    "sdiv",
    "udiv",
    "srem",
    "urem",
    "and",
    "or",
    "xor",
    "shl",
    "lshr",
    "ashr",
    "fadd",
    "fsub",
    "fmul",
    "fdiv",
    "trunc",
    "zext",
    "sext",
    "bitcast",
    "ptrtoint",
    "inttoptr",
    "fptosi",
    "sitofp",
    "fpext",
    "fptrunc",
    "unreachable",
    "label",
    "to",
    "nuw",
    "nsw",
    "eq",
    "ne",
    "slt",
    "sle",
    "sgt",
    "sge",
    "ult",
    "ule",
    "ugt",
    "uge",
    "cc",
    "noreturn",
    "nounwind",
    "attributes",
    "target",
    "datalayout",
    "triple",
    "type",
    "zeroinitializer",
    "null",
    "undef",
    "poison",
    "true",
    "false"
  ]

llvmTypes :: [Text]
llvmTypes = ["ptr", "void", "float", "double", "half"]
