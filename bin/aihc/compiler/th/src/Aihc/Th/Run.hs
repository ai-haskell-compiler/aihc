{-# LANGUAGE OverloadedStrings #-}

-- | Run one splice in an engine and convert its result.
--
-- This module does not know where the engine, the image, and the shared
-- module come from. The caller starts the engine, loads the shared module
-- of the splice module, and gives the facts of the names that are not
-- local to the module. Thus, the command line and a different host can
-- use the same code.
--
-- The body of a splice can use the modules of its own package. The splice
-- package has a copy of these modules, and the names of the copies have
-- the package id of the splice package. Thus, the compiler changes that
-- package id to the package id of the original package in each message
-- from the splice, and back in each reply.
module Aihc.Th.Run
  ( SpliceEnvironment (..),
    runSpliceClosure,
    spliceAt,
    convertResult,
    resultTrees,
    renamePackageId,
    describeFailure,
  )
where

import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve (OperatorFixity)
import Aihc.Resolve.Types (GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Convert (declarationsFromTree, expressionFromTree, patternResultFromTree, typeResultFromTree)
import Aihc.Th.Reify (fixityTree, reifyConStrictness, reifyInfo, reifyRoles)
import Aihc.Th.Request (Request (..), RequestName (..), lookupNameReply, reportedFailure, requestFromTree)
import Aihc.Th.Splices (FactsAnswer, LocalFacts, LocalFixity, NameLookup, Splice (..), SpliceRequest (..), SpliceResult (..), spliceClosureName, spliceModuleName, splicePackageId)
import Aihc.Th.Wire (Tree (..), decodeTree, encodeTree)
import Aihc.Wasm.Engine (Engine, runClosure)
import Control.Exception (IOException, try)
import Control.Monad (forM_, join)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isUpper)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO.Error (ioeGetErrorString, isUserError)

-- | The facts that the splices of one request use, apart from the facts
-- of the module itself.
data SpliceEnvironment = SpliceEnvironment
  { -- | The request of the module.
    spliceEnvironmentRequest :: !SpliceRequest,
    -- | The package id of the splice package, then the package id of the
    -- original package.
    spliceEnvironmentPackageIds :: !(Text, Text),
    -- | The package id of the @aihc-prim@ package, if the splice can use
    -- it.
    spliceEnvironmentPrimPackage :: !(Maybe PackageId),
    -- | The facts of a name that the local facts do not have.
    spliceEnvironmentFacts :: !(FactsAnswer -> GlobalName -> IO (Either Text Tree)),
    -- | The fixity of a name that the local fixities do not have.
    spliceEnvironmentFixity :: !(GlobalName -> IO (Either Text (Maybe OperatorFixity))),
    -- | Show a warning that a splice reported.
    spliceEnvironmentWarn :: !(Text -> IO ())
  }

-- | Run the closure of the splice at a position of the request, in an
-- engine that has the shared module of the splice module. The function
-- answers the requests of the splice. The result is the tree that the
-- splice sent, with the package ids of the original package, and its
-- conversion to the surface syntax.
runSpliceClosure :: Engine -> SpliceEnvironment -> Int -> NameLookup -> LocalFacts -> LocalFixity -> IO (Tree, SpliceResult)
runSpliceClosure engine environment index lookupName localFacts localFixity = do
  splice <- spliceAt request index
  let reify answer global = localFacts answer global >>= maybe (spliceEnvironmentFacts environment answer global) pure
      fixity global = localFixity global >>= maybe (spliceEnvironmentFixity environment global) (pure . Right)
  requests <- newIORef []
  let location = spliceLocation request (join (lookup index (zip [0 ..] (spliceRequestSpans request))))
  outcome <- try (runClosure engine (answerRequest fromGuest toGuest lookupName reify fixity location requests) (splicePackageId request) spliceModuleName (spliceClosureName index))
  made <- reverse <$> readIORef requests
  forM_ [message | RequestReport False message <- made] (spliceEnvironmentWarn environment)
  -- A report of an error is the cause of a failure of the closure, so the
  -- report comes before the failure itself.
  case (reportedFailure made, outcome) of
    (Just message, _) -> ioError (userError (T.unpack message))
    (Nothing, Left failure) -> ioError (userError ("splice " <> show index <> describeRequests made <> ": " <> describeFailure failure))
    (Nothing, Right [message]) -> do
      tree <- either (ioError . userError . T.unpack) (pure . fromGuest) (decodeTree message)
      result <- either (ioError . userError . T.unpack) pure (convertResult splice tree)
      pure (tree, result)
    (Nothing, Right messages) -> ioError (userError ("a splice sent " <> show (length messages) <> " messages"))
  where
    request = spliceEnvironmentRequest environment
    packageIds = spliceEnvironmentPackageIds environment
    fromGuest = fromSplice packageIds (spliceEnvironmentPrimPackage environment)
    toGuest = toSplice packageIds

-- | The splice at a position of the request.
spliceAt :: SpliceRequest -> Int -> IO Splice
spliceAt request index =
  maybe (ioError (userError ("The request has no splice " <> show index <> "."))) pure (lookup index (zip [0 ..] (spliceRequestSplices request)))

-- | The result of a splice from its tree.
convertResult :: Splice -> Tree -> Either Text SpliceResult
convertResult splice tree =
  case splice of
    ExpressionSplice _ -> ExpressionResult <$> expressionFromTree tree
    TypeSplice _ -> TypeResult <$> typeResultFromTree tree
    PatternSplice _ -> PatternResult <$> patternResultFromTree tree
    DeclarationSplice _ -> DeclarationResult <$> declarationsFromTree tree

-- | The trees of a file of splice results, by the position of the splice.
resultTrees :: Tree -> Either Text (Map Int Tree)
resultTrees tree =
  case tree of
    TreeNode "Results" entries -> Map.fromList <$> mapM entry entries
    _ -> Left "The file of splice results is not correct."
  where
    entry value =
      case value of
        TreeNode "Result" [TreeInteger index, result] -> Right (fromInteger index, result)
        _ -> Left "The file of splice results is not correct."

-- | Change the package id of the splice package to the package id of the
-- original package, in each name of a tree from the splice. The first id
-- of the pair is the id of the splice package.
--
-- The library gives the package @ghc-prim@ to some built-in names, as GHC
-- does. An example is the name that @tupleTypeName@ gives. The function
-- changes that package to the given id of the @aihc-prim@ package, which
-- has these names.
fromSplice :: (Text, Text) -> Maybe PackageId -> Tree -> Tree
fromSplice (splice, original) prim =
  maybe id (renamePackageId "ghc-prim" . packageIdText) prim . renamePackageId splice original

-- | Change the package id of the original package to the package id of the
-- splice package, in each name of a tree to the splice.
toSplice :: (Text, Text) -> Tree -> Tree
toSplice (splice, original) = renamePackageId original splice

-- | Change a package id to a different package id, in each name of a tree.
renamePackageId :: Text -> Text -> Tree -> Tree
renamePackageId from to = go
  where
    go tree =
      case tree of
        TreeNode "PkgName" [TreeString identity] | identity == from -> TreeNode "PkgName" [TreeString to]
        TreeNode constructor fields -> TreeNode constructor (map go fields)
        _ -> tree

-- | The message of a failure. A failure of a file operation also names
-- the file and the operation.
describeFailure :: IOException -> String
describeFailure failure
  | isUserError failure = ioeGetErrorString failure
  | otherwise = show failure

-- | The names that a splice asked @reify@ for, for a failure message.
describeRequests :: [Request] -> String
describeRequests made =
  case [name | request <- made, Just name <- [reifiedName request]] of
    [] -> ""
    names -> " (after reify of " <> intercalate ", " (map T.unpack names) <> ")"
  where
    reifiedName request =
      case request of
        RequestReify name _ -> Just name
        RequestReifyRoles name _ -> Just name
        RequestReifyConStrictness name _ -> Just name
        RequestReifyFixity name _ -> Just name
        RequestReport {} -> Nothing
        RequestLocation -> Nothing
        RequestLookupName {} -> Nothing

-- | Do the request of a splice and give the bytes of the reply. Each
-- request goes to the list. The reply to a report has no bytes. The reply
-- to @reify@ is an @Either String Info@ value. The reply to @reifyRoles@ is
-- an @Either String [Role]@ value. The reply to @reifyConStrictness@ is an
-- @Either String [DecidedStrictness]@ value. The reply to @reifyFixity@ is
-- an @Either String (Maybe Fixity)@ value. A name that the compiler cannot
-- find has no fixity declaration that the compiler knows, so it gives
-- 'Nothing'. The reply to @location@ is the @Loc@ value of the splice.
answerRequest :: (Tree -> Tree) -> (Tree -> Tree) -> NameLookup -> (FactsAnswer -> GlobalName -> IO (Either Text Tree)) -> (GlobalName -> IO (Either Text (Maybe OperatorFixity))) -> Tree -> IORef [Request] -> ByteString -> IO ByteString
answerRequest fromGuest toGuest lookupName reify fixity location requests bytes =
  case decodeTree bytes >>= requestFromTree . fromGuest of
    Right made -> do
      modifyIORef' requests (made :)
      case made of
        RequestReport {} -> pure BS.empty
        RequestReify name global -> facts reifyInfo name global
        RequestReifyRoles name global -> facts reifyRoles name global
        RequestReifyConStrictness name global -> facts reifyConStrictness name global
        RequestReifyFixity name requested -> do
          answer <- case requested of
            RequestGlobal global -> fixity global
            RequestScoped scoped -> maybe (pure (Right Nothing)) fixity (scopedName name scoped)
            RequestUnknown -> pure (Right Nothing)
          pure (encodeReply (fixityTree <$> answer))
        RequestLocation -> pure (encodeTree location)
        RequestLookupName isType scoped ->
          let namespace = if isType then ResolutionNamespaceType else ResolutionNamespaceTerm
           in pure (encodeTree (toGuest (lookupNameReply (lookupName namespace scoped))))
    Left message -> ioError (userError (T.unpack message))
  where
    facts question name requested =
      case requested of
        RequestGlobal global -> reply (reify question global)
        RequestScoped scoped ->
          case scopedName name scoped of
            Just global -> reply (reify question global)
            Nothing -> reply (pure (Left (name <> " is not in scope at the splice.")))
        RequestUnknown -> reply (pure (Left ("The compiler can only give the facts of a name with a package and a module, or of a name in scope at the splice, and " <> name <> " is not such a name.")))
    scopedName name scoped = listToMaybe (mapMaybe (`lookupName` scoped) (guessedNamespaces name))
    reply ask = encodeReply <$> ask
    encodeReply answer =
      encodeTree . toGuest $ case answer of
        Right value -> TreeNode "Right" [value]
        Left message -> TreeNode "Left" [TreeString message]

-- | The namespaces where the compiler looks for a name with no package, in
-- order. As with GHC, a name that starts with an upper-case letter or a
-- colon is first a type constructor or a class, and then a data
-- constructor.
guessedNamespaces :: Text -> [ResolutionNamespace]
guessedNamespaces name =
  case T.uncons name of
    Just (first, _)
      | isUpper first || first == ':' -> [ResolutionNamespaceType, ResolutionNamespaceTerm]
    _ -> [ResolutionNamespaceTerm]

-- | The @Loc@ value of a splice: the source file, the package id, the
-- module, and the start and the end of the splice. A splice with no source
-- span gets the position 1:1. GHC gives the column after the last
-- character as the end, as the source span does.
spliceLocation :: SpliceRequest -> Maybe Syntax.SourceSpan -> Tree
spliceLocation request span' =
  TreeNode
    "Loc"
    [ TreeString (T.pack (spliceRequestSourceFile request)),
      TreeString (spliceRequestPackageId request),
      TreeString (spliceRequestModule request),
      position start,
      position end
    ]
  where
    (start, end) =
      case span' of
        Just (Syntax.SourceSpan _ startLine startColumn endLine endColumn _ _) -> ((startLine, startColumn), (endLine, endColumn))
        Nothing -> ((1, 1), (1, 1))
    position (line, column) = TreeNode "(,)" [TreeInteger (toInteger line), TreeInteger (toInteger column)]
