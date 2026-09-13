{-# LANGUAGE OverloadedStrings #-}

-- | Subset compatibility checks for interfaces extracted from @.hi@ files.
module Aihc.Dev.ExtractHi.Compare
  ( InterfaceMismatch (..),
    CompatibilityReport (..),
    CoreLibProgressReport (..),
    comparePackageCompatibility,
    comparePackageSubset,
    compatibilityPercent,
    coreLibApiDivergences,
    normalizeSignature,
    coreLibProgressReports,
    renderInterfaceMismatch,
    renderCoreLibProgressReport,
    renderCoreLibProgressReports,
    runCoreLibApiDivergences,
    runCoreLibProgressReports,
  )
where

import Aihc.Dev.ExtractHi (extractPackage, extractSourcePackage)
import Aihc.Dev.ExtractHi.Types
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import Text.Printf (printf)

data InterfaceMismatch = InterfaceMismatch
  { mismatchPath :: !Text,
    mismatchMessage :: !Text
  }
  deriving (Eq, Show)

data CompatibilityReport = CompatibilityReport
  { crMatched :: !Int,
    crTotal :: !Int,
    crExtra :: !Int,
    crMismatches :: ![InterfaceMismatch]
  }
  deriving (Eq, Show)

data CoreLibProgressReport = CoreLibProgressReport
  { clprProgressKey :: !String,
    clprExtraKey :: !String,
    clprReport :: !CompatibilityReport
  }
  deriving (Eq, Show)

comparePackageCompatibility :: PackageInterface -> PackageInterface -> CompatibilityReport
comparePackageCompatibility candidate oracle =
  CompatibilityReport
    { crMatched = matched,
      crTotal = total,
      crExtra = countExtraPackageItems candidate oracle,
      crMismatches = mismatches
    }
  where
    oracleItems = packageItems oracle
    candidateItems = packageItems candidate
    candidateMap = keyed itemKey candidateItems
    results = map (compareCompatibilityItem candidateMap) oracleItems
    matched = length (filter id (map fst results))
    total = length oracleItems
    mismatches = concatMap snd results

compatibilityPercent :: CompatibilityReport -> Double
compatibilityPercent report
  | crTotal report <= 0 = 100
  | otherwise = fromIntegral (crMatched report) * 100.0 / fromIntegral (crTotal report)

coreLibProgressReports :: PackageInterface -> PackageInterface -> PackageInterface -> PackageInterface -> [CoreLibProgressReport]
coreLibProgressReports aihcPrim ghcPrim aihcBase base =
  [ CoreLibProgressReport
      { clprProgressKey = "GHC_PRIM",
        clprExtraKey = "ghc-prim",
        clprReport = comparePackageCompatibility aihcPrim ghcPrim
      },
    CoreLibProgressReport
      { clprProgressKey = "BASE",
        clprExtraKey = "base",
        clprReport = comparePackageCompatibility aihcBase base
      }
  ]

runCoreLibProgressReports :: IO [CoreLibProgressReport]
runCoreLibProgressReports = do
  (aihcPrim, ghcPrim, aihcBase, base) <- extractCoreLibs
  pure (coreLibProgressReports aihcPrim ghcPrim aihcBase base)

-- | Exports of @aihc-prim@ and @aihc-base@ that GHC's @ghc-prim@ and @base@
-- do not provide, restricted to modules that exist in those packages.
-- Modules that only aihc defines may export anything.
runCoreLibApiDivergences :: IO [InterfaceMismatch]
runCoreLibApiDivergences = do
  (aihcPrim, ghcPrim, aihcBase, base) <- extractCoreLibs
  pure (coreLibApiDivergences [ghcPrim, base] aihcPrim <> coreLibApiDivergences [ghcPrim, base] aihcBase)

extractCoreLibs :: IO (PackageInterface, PackageInterface, PackageInterface, PackageInterface)
extractCoreLibs = do
  root <- coreLibsRoot
  ghcPrim <- extractPackage "ghc-prim"
  aihcPrim <- extractSourcePackage (root </> "core-libs" </> "aihc-prim") "aihc-prim"
  base <- extractPackage "base"
  aihcBase <- extractSourcePackage (root </> "core-libs" </> "aihc-base") "aihc-base"
  pure (aihcPrim, ghcPrim, aihcBase, base)

-- | The directory holding @core-libs/@: @AIHC_CORE_LIBS_ROOT@ when set,
-- otherwise the nearest ancestor of the working directory that has it.
coreLibsRoot :: IO FilePath
coreLibsRoot = do
  override <- lookupEnv "AIHC_CORE_LIBS_ROOT"
  case override of
    Just root -> pure root
    Nothing -> getCurrentDirectory >>= findRoot
  where
    marker = "core-libs" </> "aihc-base" </> "aihc-base.cabal"
    findRoot dir = do
      exists <- doesFileExist (dir </> marker)
      if exists
        then pure dir
        else do
          let parent = takeDirectory dir
          if parent == dir
            then ioError (userError ("Could not find " <> marker <> " from the current directory"))
            else findRoot parent

-- | Candidate exports that diverge from the oracle packages. Only modules
-- that an oracle package also exports are checked: every name such a module
-- exports must also be exported by the oracle module, and every fixity it
-- declares must be declared identically there, so that code compiled against
-- aihc's core libraries also compiles against GHC's. Candidate-only modules
-- may export anything.
--
-- Names are compared without their namespace or signature: a class method
-- may become a plain function and back, and the two extractors render types
-- differently. Oracle modules without any extracted export are skipped.
coreLibApiDivergences :: [PackageInterface] -> PackageInterface -> [InterfaceMismatch]
coreLibApiDivergences oracles candidate =
  concatMap divergences (piModules candidate)
  where
    oracleModules =
      Map.fromList
        [ (miModule modu, (packageDisplayName (piPackage oracle), modu))
        | oracle <- oracles,
          modu <- piModules oracle,
          not (Set.null (moduleExportNames modu))
        ]

    divergences modu =
      case Map.lookup (miModule modu) oracleModules of
        Nothing -> []
        Just (oraclePackage, oracleModule) ->
          [ mismatch (miModule modu <> "." <> name) ("not exported by " <> oraclePackage)
          | name <- Set.toAscList (moduleExportNames modu `Set.difference` moduleExportNames oracleModule)
          ]
            <> [ mismatch (miModule modu <> ".fixity:" <> fiName fixity) message
               | fixity <- miFixities modu,
                 Just message <- [fixityDivergence oraclePackage oracleFixities fixity]
               ]
          where
            oracleFixities = keyed fiName (miFixities oracleModule)

    fixityDivergence oraclePackage oracleFixities fixity =
      case Map.lookup (fiName fixity) oracleFixities of
        Nothing -> Just ("fixity is not declared by " <> oraclePackage)
        Just oracleFixity
          | fixity /= oracleFixity -> Just ("fixity differs from " <> oraclePackage <> ": " <> T.pack (show oracleFixity))
          | otherwise -> Nothing

-- | Every name a module exports, whatever its namespace: values, types and
-- their constructors and fields, classes and their methods.
moduleExportNames :: ModuleInterface -> Set Text
moduleExportNames iface =
  Set.fromList $
    concat
      [ map evName (miValues iface),
        concatMap (\typ -> etName typ : etConstructors typ) (miTypes iface),
        concatMap (\klass -> ecName klass : map cmName (ecMethods klass)) (miClasses iface)
      ]

-- | The package name of a @ghc-pkg@ package id such as @base-4.21.2.0-fc24@.
packageDisplayName :: Text -> Text
packageDisplayName packageId =
  T.intercalate "-" (takeWhile (not . startsWithDigit) (T.splitOn "-" packageId))
  where
    startsWithDigit segment = maybe False (isDigit . fst) (T.uncons segment)

renderCoreLibProgressReports :: [CoreLibProgressReport] -> String
renderCoreLibProgressReports reports =
  unlines (map renderCoreLibProgressReport reports <> map renderExtraLine reports)

renderCoreLibProgressReport :: CoreLibProgressReport -> String
renderCoreLibProgressReport report =
  printf
    "%s %d %d %.2f"
    (clprProgressKey report)
    (crMatched stats)
    (crTotal stats)
    (compatibilityPercent stats)
  where
    stats = clprReport report

comparePackageSubset :: PackageInterface -> PackageInterface -> [InterfaceMismatch]
comparePackageSubset candidate oracle =
  concatMap compareModule (piModules candidate)
  where
    oracleModules = keyed miModule (piModules oracle)

    compareModule candidateModule =
      case Map.lookup (miModule candidateModule) oracleModules of
        Nothing ->
          [mismatch (miModule candidateModule) "module is not exported by oracle"]
        Just oracleModule ->
          compareValues candidateModule oracleModule
            <> compareTypes candidateModule oracleModule
            <> compareClasses candidateModule oracleModule
            <> compareFixities candidateModule oracleModule

compareValues :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareValues candidate oracle =
  concatMap compareValue (miValues candidate)
  where
    oracleValues = keyed evName (miValues oracle)
    basePath = miModule candidate <> ".value"

    compareValue value =
      case Map.lookup (evName value) oracleValues of
        Nothing -> [mismatch (basePath <> ":" <> evName value) "value is not exported by oracle"]
        Just oracleValue
          | evType value /= evType oracleValue ->
              [mismatch (basePath <> ":" <> evName value <> ".type") "value type differs from oracle"]
          | otherwise -> []

compareTypes :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareTypes candidate oracle =
  concatMap compareType (miTypes candidate)
  where
    oracleTypes = keyed etName (miTypes oracle)
    basePath = miModule candidate <> ".type"

    compareType typ =
      case Map.lookup (etName typ) oracleTypes of
        Nothing -> [mismatch (basePath <> ":" <> etName typ) "type is not exported by oracle"]
        Just oracleType ->
          [ mismatch (basePath <> ":" <> etName typ <> ".kind") "type kind differs from oracle"
          | etKind typ /= etKind oracleType
          ]
            <> [ mismatch (basePath <> ":" <> etName typ <> ".constructors") "constructors differ from oracle"
               | etConstructors typ /= etConstructors oracleType
               ]

compareClasses :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareClasses candidate oracle =
  concatMap compareClass (miClasses candidate)
  where
    oracleClasses = keyed ecName (miClasses oracle)
    basePath = miModule candidate <> ".class"

    compareClass klass =
      case Map.lookup (ecName klass) oracleClasses of
        Nothing -> [mismatch (basePath <> ":" <> ecName klass) "class is not exported by oracle"]
        Just oracleClass -> compareMethods klass oracleClass

    compareMethods klass oracleClass =
      concatMap compareMethod (ecMethods klass)
      where
        oracleMethods = keyed cmName (ecMethods oracleClass)
        methodPath = basePath <> ":" <> ecName klass <> ".method"

        compareMethod method =
          case Map.lookup (cmName method) oracleMethods of
            Nothing -> [mismatch (methodPath <> ":" <> cmName method) "method is not exported by oracle"]
            Just oracleMethod
              | cmType method /= cmType oracleMethod ->
                  [mismatch (methodPath <> ":" <> cmName method <> ".type") "method type differs from oracle"]
              | otherwise -> []

compareFixities :: ModuleInterface -> ModuleInterface -> [InterfaceMismatch]
compareFixities candidate oracle =
  concatMap compareFixity (miFixities candidate)
  where
    oracleFixities = keyed fiName (miFixities oracle)
    basePath = miModule candidate <> ".fixity"

    compareFixity fixity =
      case Map.lookup (fiName fixity) oracleFixities of
        Nothing -> [mismatch (basePath <> ":" <> fiName fixity) "fixity is not exported by oracle"]
        Just oracleFixity
          | fixity /= oracleFixity ->
              [mismatch (basePath <> ":" <> fiName fixity) "fixity differs from oracle"]
          | otherwise -> []

keyed :: (Ord k) => (a -> k) -> [a] -> Map k a
keyed key = Map.fromList . map (\value -> (key value, value))

mismatch :: Text -> Text -> InterfaceMismatch
mismatch = InterfaceMismatch

renderInterfaceMismatch :: InterfaceMismatch -> String
renderInterfaceMismatch item =
  T.unpack (mismatchPath item <> ": " <> mismatchMessage item)

renderExtraLine :: CoreLibProgressReport -> String
renderExtraLine report =
  printf "EXTRA %s %d" (clprExtraKey report) (crExtra (clprReport report))

data InterfaceItem = InterfaceItem
  { itemKey :: !Text,
    itemPath :: !Text,
    -- | The normalized signature, or 'Nothing' when the interface does not
    -- state one, which is compatible with any signature.
    itemSignature :: !(Maybe Text)
  }
  deriving (Eq, Show)

compareCompatibilityItem :: Map Text InterfaceItem -> InterfaceItem -> (Bool, [InterfaceMismatch])
compareCompatibilityItem candidateItems oracleItem =
  case Map.lookup (itemKey oracleItem) candidateItems of
    Nothing -> (False, [mismatch (itemPath oracleItem) "export is missing from candidate"])
    Just candidateItem
      | Just candidateSignature <- itemSignature candidateItem,
        Just oracleSignature <- itemSignature oracleItem,
        candidateSignature /= oracleSignature ->
          (False, [mismatch (itemPath oracleItem) "export signature differs from candidate"])
      | otherwise -> (True, [])

-- | Bring a type or kind rendered by GHC's interface printer and one rendered
-- from aihc source into the same shape. Whitespace is collapsed first, since
-- GHC wraps long signatures over several lines. The @forall@ prefix goes:
-- GHC prints inferred binders as @forall {r :: RuntimeRep}@ while source
-- spells them out or leaves them implicit. Multiplicities (@a %1 -> b@) go
-- because source signatures never write them, promotion ticks (@'IntRep@)
-- because they are optional, and the kind synonyms of @GHC.Types@ are
-- expanded because either side may use the synonym or its definition. A
-- single-constraint context loses its parentheses, module qualifiers are
-- dropped, and type variables are renamed in order of first occurrence so
-- that alpha-equivalent signatures compare equal.
normalizeSignature :: Text -> Text
normalizeSignature =
  renameTypeVariables
    . normalizeContext
    . T.unwords
    . map expandKindSynonym
    . dropMultiplicities
    . T.words
    . dropTicks
    . dropForall
    . T.unwords
    . T.words
  where
    dropForall text =
      case T.stripPrefix "forall" text of
        Just rest
          | Just (sep, _) <- T.uncons rest,
            isSpace sep || sep == '{' || sep == '(' ->
              let (_, body) = T.breakOn ". " rest
               in if T.null body then text else T.drop 2 body
        _ -> text
    dropMultiplicities (mult : arrow : rest)
      | "%" `T.isPrefixOf` mult, arrow == "->" = arrow : dropMultiplicities rest
    dropMultiplicities (token : rest) = token : dropMultiplicities rest
    dropMultiplicities [] = []
    -- A tick that follows an identifier character belongs to a name such
    -- as @foldl'@; any other tick promotes a constructor.
    dropTicks text = T.pack (go ' ' (T.unpack text))
      where
        go previous (c : rest)
          | c == '\'' && not (isIdentifierChar previous || previous == '\'') = go c rest
          | otherwise = c : go c rest
        go _ [] = []
    -- Tokens carry their surrounding brackets, so only the core is looked up.
    expandKindSynonym token =
      let (open, rest) = T.span (`elem` ("([" :: String)) token
          (core, close) = T.break (`elem` (")],." :: String)) rest
       in case Map.lookup core kindSynonyms of
            Just expansion -> open <> expansion <> close
            Nothing -> token
    kindSynonyms =
      Map.fromList
        [ ("Type", "TYPE (BoxedRep Lifted)"),
          ("UnliftedType", "TYPE (BoxedRep Unlifted)"),
          ("ZeroBitType", "TYPE (TupleRep [])"),
          ("LiftedRep", "(BoxedRep Lifted)"),
          ("UnliftedRep", "(BoxedRep Unlifted)"),
          ("ZeroBitRep", "(TupleRep [])")
        ]
    -- @(Monad m) => a@ and @Monad m => a@ are the same context.
    normalizeContext text =
      case T.breakOn " => " text of
        (context, rest)
          | not (T.null rest),
            Just inner <- T.stripPrefix "(" context >>= T.stripSuffix ")",
            not (T.any (== ',') inner) ->
              inner <> rest
        _ -> text

-- | Rename every type variable to its index of first occurrence and drop
-- module qualifiers, so @Strict.ST s a -> ST s a@ becomes @ST t1 t2 -> ST t1 t2@.
renameTypeVariables :: Text -> Text
renameTypeVariables text = T.pack (go Map.empty (T.unpack text))
  where
    go _ [] = []
    go seen input@(c : rest)
      | isIdentifierStart c =
          let (identifier, afterIdentifier) = span isIdentifierChar input
           in case afterIdentifier of
                '.' : next : _
                  | isUpper c,
                    isIdentifierStart next ->
                      go seen (drop 1 afterIdentifier)
                _
                  | isUpper c -> identifier <> go seen afterIdentifier
                  | otherwise ->
                      let (name, seen') = case Map.lookup identifier seen of
                            Just known -> (known, seen)
                            Nothing ->
                              let fresh = "t" <> show (Map.size seen + 1)
                               in (fresh, Map.insert identifier fresh seen)
                       in name <> go seen' afterIdentifier
      | otherwise = c : go seen rest
    isIdentifierStart ch = isAlpha ch || ch == '_'

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_' || ch == '\'' || ch == '#'

countExtraPackageItems :: PackageInterface -> PackageInterface -> Int
countExtraPackageItems candidate oracle =
  length
    [ ()
    | candidateItem <- packageItems candidate,
      Map.notMember (itemKey candidateItem) oracleItems
    ]
  where
    oracleItems = keyed itemKey (packageItems oracle)

packageItems :: PackageInterface -> [InterfaceItem]
packageItems pkg =
  concatMap moduleItems (piModules pkg)

moduleItems :: ModuleInterface -> [InterfaceItem]
moduleItems iface =
  concat
    [ map (valueItem moduleName) (miValues iface),
      concatMap (typeItems moduleName) (miTypes iface),
      concatMap (classItems moduleName) (miClasses iface),
      map (fixityItem moduleName) (miFixities iface)
    ]
  where
    moduleName = miModule iface

valueItem :: Text -> ExportedValue -> InterfaceItem
valueItem moduleName value =
  InterfaceItem
    { itemKey = moduleName <> ".value:" <> evName value,
      itemPath = moduleName <> ".value:" <> evName value,
      itemSignature = Just (normalizeSignature (evType value))
    }

typeItems :: Text -> ExportedType -> [InterfaceItem]
typeItems moduleName typ =
  InterfaceItem
    { itemKey = typeKey,
      itemPath = typeKey,
      itemSignature =
        if etKind typ == unspecifiedSourceKind
          then Nothing
          else Just (normalizeSignature (etKind typ))
    }
    : [ InterfaceItem
          { itemKey = typeKey <> ".constructor:" <> ctor,
            itemPath = typeKey <> ".constructor:" <> ctor,
            itemSignature = Nothing
          }
      | ctor <- etConstructors typ
      ]
  where
    typeKey = moduleName <> ".type:" <> etName typ

classItems :: Text -> ExportedClass -> [InterfaceItem]
classItems moduleName klass =
  InterfaceItem
    { itemKey = classKey,
      itemPath = classKey,
      itemSignature = Nothing
    }
    : [ InterfaceItem
          { itemKey = classKey <> ".method:" <> cmName method,
            itemPath = classKey <> ".method:" <> cmName method,
            itemSignature = Just (normalizeSignature (cmType method))
          }
      | method <- ecMethods klass
      ]
  where
    classKey = moduleName <> ".class:" <> ecName klass

fixityItem :: Text -> FixityInfo -> InterfaceItem
fixityItem moduleName fixity =
  InterfaceItem
    { itemKey = moduleName <> ".fixity:" <> fiName fixity,
      itemPath = moduleName <> ".fixity:" <> fiName fixity,
      itemSignature = Just (T.pack (show fixity))
    }
