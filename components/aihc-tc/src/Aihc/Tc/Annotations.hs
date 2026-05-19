{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Type checker annotations for AST nodes.
--
-- Following the pattern established by @aihc-resolve@, the type checker
-- attaches its results as 'Annotation' values on AST nodes using the
-- existing 'DeclAnn'/'EAnn'/'PAnn'/'TAnn' wrappers.
module Aihc.Tc.Annotations
  ( -- * Annotation type
    TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),

    -- * Pattern synonyms for extracting annotations
    pattern ETcAnn,
    pattern DTcAnn,
    pattern PTcAnn,
    pattern TTcAnn,

    -- * Helpers
    annotateExpr,
    annotateDecl,

    -- * Pretty-printing
    renderTcType,
    renderTcAnnotations,
    renderAnnotatedTcModules,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ClassDecl (..),
    ClassDeclItem (..),
    Decl (..),
    Expr (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    fromAnnotation,
    mkAnnotation,
    moduleName,
  )
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Applicative ((<|>))
import Data.Data (Data, cast, gmapQ)
import Data.List (intercalate, partition, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

-- | Annotation attached to AST nodes by the type checker.
--
-- Not every field is populated for every node. A variable reference gets
-- a type; a top-level binding gets the generalized scheme, etc.
data TcAnnotation = TcAnnotation
  { -- | The inferred/checked type of this node.
    tcAnnType :: !TcType,
    -- | Type arguments made explicit at this occurrence.
    tcAnnTypeArgs :: ![TcType],
    -- | Evidence terms whose dictionaries must be passed at this occurrence.
    tcAnnEvidenceTerms :: ![EvTerm],
    -- | Term argument types made explicit for lambda-like binders.
    tcAnnTermArgTypes :: ![TcType]
  }
  deriving (Eq, Show)

data TcDictBinderAnnotation = TcDictBinderAnnotation
  { tcDictBinderClassName :: !Text,
    tcDictBinderArgs :: ![TcType],
    tcDictBinderType :: !TcType
  }
  deriving (Eq, Show)

data TcClassMethodAnnotation = TcClassMethodAnnotation
  { tcClassMethodName :: !Text,
    tcClassMethodType :: !TcType,
    tcClassMethodTyVars :: ![TyVarId],
    tcClassMethodDictType :: !TcType,
    tcClassMethodIndex :: !Int
  }
  deriving (Eq, Show)

newtype TcClassAnnotation = TcClassAnnotation
  { tcClassMethods :: [TcClassMethodAnnotation]
  }
  deriving (Eq, Show)

data TcInstanceAnnotation = TcInstanceAnnotation
  { tcInstanceDictName :: !Text,
    tcInstanceDictType :: !TcType,
    tcInstanceTyVars :: ![TyVarId],
    tcInstanceHeadTypes :: ![TcType],
    tcInstanceContextDicts :: ![TcDictBinderAnnotation],
    tcInstanceMethodOrder :: ![Text]
  }
  deriving (Eq, Show)

data TcInstanceMethodAnnotation = TcInstanceMethodAnnotation
  { tcInstanceMethodName :: !Text,
    tcInstanceMethodType :: !TcType
  }
  deriving (Eq, Show)

-- | Extract a 'TcAnnotation' from an 'Expr'.
pattern ETcAnn :: TcAnnotation -> Expr -> Expr
pattern ETcAnn ann inner <- EAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Decl'.
pattern DTcAnn :: TcAnnotation -> Decl -> Decl
pattern DTcAnn ann inner <- DeclAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Pattern'.
pattern PTcAnn :: TcAnnotation -> Pattern -> Pattern
pattern PTcAnn ann inner <- PAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Type'.
pattern TTcAnn :: TcAnnotation -> Type -> Type
pattern TTcAnn ann inner <- TAnn (fromAnnotation -> Just ann) inner

-- | Wrap an expression with a type annotation.
annotateExpr :: TcAnnotation -> Expr -> Expr
annotateExpr ann = EAnn (mkAnnotation ann)

-- | Wrap a declaration with a type annotation.
annotateDecl :: TcAnnotation -> Decl -> Decl
annotateDecl ann = DeclAnn (mkAnnotation ann)

-- | Render a 'TcType' as a human-readable string.
--
-- Uses a precedence level to decide when to insert parentheses:
--   0 = no parens needed (top level or right of ->)
--   1 = parens needed for function types (left of ->)
--   2 = parens needed for function types and type applications (inside type con args)
renderTcType :: TcType -> String
renderTcType = go 0
  where
    go :: Int -> TcType -> String
    go _ (TcTyVar tv) = T.unpack (tvName tv)
    go _ (TcMetaTv (Unique u)) = "?" ++ show u
    go _ (TcTyCon tc []) = T.unpack (tyConName tc)
    go _ (TcTyCon (TyCon name 1) [arg])
      | name == T.pack "[]" = "[" ++ go 0 arg ++ "]"
    go p (TcTyCon tc args) =
      parenIf (p >= 2) $
        unwords (T.unpack (tyConName tc) : map (go 2) args)
    go p (TcFunTy a b) =
      parenIf (p >= 1) $
        go 1 a ++ " -> " ++ go 0 b
    go p (TcForAllTy tv body) =
      let (tvs, inner) = collectForAlls body
       in parenIf (p >= 1) $
            "forall " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ go 0 inner
    go p (TcQualTy [] body) = go p body
    go p (TcQualTy preds body) =
      parenIf (p >= 1) $
        "(" ++ unwords (map showPred preds) ++ ") => " ++ go 0 body
    go p (TcAppTy f a) =
      parenIf (p >= 2) $
        go 1 f ++ " " ++ go 2 a

    showPred (ClassPred cls args) =
      T.unpack cls ++ " " ++ unwords (map (go 2) args)
    showPred (EqPred t1 t2) =
      go 2 t1 ++ " ~ " ++ go 2 t2

    parenIf False s = s
    parenIf True s = "(" ++ s ++ ")"

-- | Collect nested forall binders into a list.
collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)

data TcLocatedAnnotation = TcLocatedAnnotation
  { locatedSpan :: !SourceSpan,
    locatedKind :: !String,
    locatedSummary :: !String,
    locatedLabel :: !String
  }
  deriving (Eq, Show)

-- | Render all type-checker annotations attached to a list of modules.
renderTcAnnotations :: [Module] -> String
renderTcAnnotations modules =
  intercalate "\n" (map renderModule (sortOn fst (map collectModuleTcAnnotations modules)))
  where
    renderModule (moduleNameText, annotations) =
      T.unpack moduleNameText
        <> ":\n"
        <> intercalate "\n" (map (("  " <>) . renderLocatedAnnotation) (sortOn annotationKey annotations))

-- | Pretty-print module source text with type-checker annotations interleaved.
renderAnnotatedTcModules :: [Text] -> [Module] -> [String]
renderAnnotatedTcModules sources modules =
  let moduleSourcePairs = zipWith (\source modu -> (moduleDisplayName modu, source)) sources modules
      annotationMap = Map.fromList (map collectModuleTcAnnotations modules)
   in map
        ( \(name, source) ->
            renderAnnotatedTcSource source (Map.findWithDefault [] name annotationMap)
        )
        (sortOn fst moduleSourcePairs)

collectModuleTcAnnotations :: Module -> (Text, [TcLocatedAnnotation])
collectModuleTcAnnotations modu =
  (moduleDisplayName modu, collectTcAnnotations NoSourceSpan modu)

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe (T.pack "<unnamed>") (moduleName modu)

collectTcAnnotations :: (Data a) => SourceSpan -> a -> [TcLocatedAnnotation]
collectTcAnnotations ambient node =
  fromMaybe (concat (gmapQ (collectTcAnnotations ambient) node)) $
    collectDecl <$> cast node
      <|> collectExpr <$> cast node
      <|> collectPattern <$> cast node
      <|> collectType <$> cast node
      <|> collectClassItem <$> cast node
      <|> collectInstanceItem <$> cast node
  where
    collectDecl (DeclAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationDeclTcItems ambient' inner ann <> collectTcAnnotations ambient' inner
    collectDecl other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectExpr (EAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "expr" ambient' ann <> collectTcAnnotations ambient' inner
    collectExpr other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectPattern (PAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "pattern" ambient' ann <> collectTcAnnotations ambient' inner
    collectPattern other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectType (TAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "type" ambient' ann <> collectTcAnnotations ambient' inner
    collectType other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectClassItem (ClassItemAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "class-item" ambient' ann <> collectTcAnnotations ambient' inner
    collectClassItem other = concat (gmapQ (collectTcAnnotations ambient) other)

    collectInstanceItem (InstanceItemAnn ann inner) =
      let ambient' = annotationSourceSpan ambient ann
       in annotationTcItems "instance-item" ambient' ann <> collectTcAnnotations ambient' inner
    collectInstanceItem other = concat (gmapQ (collectTcAnnotations ambient) other)

annotationSourceSpan :: SourceSpan -> Annotation -> SourceSpan
annotationSourceSpan ambient ann =
  fromMaybe ambient (fromAnnotation @SourceSpan ann)

annotationTcItems :: String -> SourceSpan -> Annotation -> [TcLocatedAnnotation]
annotationTcItems kind span' ann =
  concat
    [ map (locatedTcAnnotation kind span') (maybeToListAnn @TcAnnotation ann),
      map (locatedTcClassAnnotation span') (maybeToListAnn @TcClassAnnotation ann),
      map (locatedTcInstanceAnnotation span') (maybeToListAnn @TcInstanceAnnotation ann),
      map (locatedTcInstanceMethodAnnotation span') (maybeToListAnn @TcInstanceMethodAnnotation ann)
    ]

annotationDeclTcItems :: SourceSpan -> Decl -> Annotation -> [TcLocatedAnnotation]
annotationDeclTcItems span' inner ann =
  concat
    [ map (locatedTcAnnotation "decl" span') (maybeToListAnn @TcAnnotation ann),
      map (locatedTcClassAnnotation (refineDeclSpan span' inner)) (maybeToListAnn @TcClassAnnotation ann),
      map (locatedTcInstanceAnnotation (refineDeclSpan span' inner)) (maybeToListAnn @TcInstanceAnnotation ann),
      map (locatedTcInstanceMethodAnnotation span') (maybeToListAnn @TcInstanceMethodAnnotation ann)
    ]

refineDeclSpan :: SourceSpan -> Decl -> SourceSpan
refineDeclSpan span' decl =
  case decl of
    DeclClass classDecl -> spanThroughItems span' (concatMap classItemSourceSpans (classDeclItems classDecl))
    DeclInstance instanceDecl -> spanThroughItems span' (concatMap instanceItemSourceSpans (instanceDeclItems instanceDecl))
    _ -> span'

classItemSourceSpans :: ClassDeclItem -> [SourceSpan]
classItemSourceSpans item =
  case item of
    ClassItemAnn ann inner -> annotationSourceSpan NoSourceSpan ann : classItemSourceSpans inner
    _ -> []

instanceItemSourceSpans :: InstanceDeclItem -> [SourceSpan]
instanceItemSourceSpans item =
  case item of
    InstanceItemAnn ann inner -> annotationSourceSpan NoSourceSpan ann : instanceItemSourceSpans inner
    _ -> []

spanThroughItems :: SourceSpan -> [SourceSpan] -> SourceSpan
spanThroughItems span' itemSpans =
  case latestSourceSpan itemSpans of
    Nothing -> span'
    Just itemSpan -> replaceSpanEnd span' itemSpan

latestSourceSpan :: [SourceSpan] -> Maybe SourceSpan
latestSourceSpan spans =
  case filter hasSourceSpan spans of
    [] -> Nothing
    realSpans -> Just (maximumByEnd realSpans)

maximumByEnd :: [SourceSpan] -> SourceSpan
maximumByEnd = foldr1 maxEnd
  where
    maxEnd left right
      | sourceSpanEndKey left >= sourceSpanEndKey right = left
      | otherwise = right

sourceSpanEndKey :: SourceSpan -> (Int, Int)
sourceSpanEndKey span' =
  case span' of
    SourceSpan _ _ _ endLine endCol _ _ -> (endLine, endCol)
    NoSourceSpan -> (minBound, minBound)

hasSourceSpan :: SourceSpan -> Bool
hasSourceSpan SourceSpan {} = True
hasSourceSpan NoSourceSpan = False

replaceSpanEnd :: SourceSpan -> SourceSpan -> SourceSpan
replaceSpanEnd startSpan endSpan =
  case (startSpan, endSpan) of
    (SourceSpan source startLine startCol _ _ startOffset _, SourceSpan _ _ _ endLine endCol _ endOffset) ->
      SourceSpan source startLine startCol endLine endCol startOffset endOffset
    _ -> startSpan

maybeToListAnn :: forall a. (Typeable a) => Annotation -> [a]
maybeToListAnn ann = maybe [] pure (fromAnnotation @a ann)

locatedTcAnnotation :: String -> SourceSpan -> TcAnnotation -> TcLocatedAnnotation
locatedTcAnnotation kind span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = kind,
      locatedSummary = renderTcAnnotation ann,
      locatedLabel = renderTcAnnotationLabel ann
    }

locatedTcClassAnnotation :: SourceSpan -> TcClassAnnotation -> TcLocatedAnnotation
locatedTcClassAnnotation span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = "class",
      locatedSummary = renderTcClassAnnotation ann,
      locatedLabel = "class"
    }

locatedTcInstanceAnnotation :: SourceSpan -> TcInstanceAnnotation -> TcLocatedAnnotation
locatedTcInstanceAnnotation span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = "instance",
      locatedSummary = renderTcInstanceAnnotation ann,
      locatedLabel = T.unpack (tcInstanceDictName ann)
    }

locatedTcInstanceMethodAnnotation :: SourceSpan -> TcInstanceMethodAnnotation -> TcLocatedAnnotation
locatedTcInstanceMethodAnnotation span' ann =
  TcLocatedAnnotation
    { locatedSpan = span',
      locatedKind = "instance-method",
      locatedSummary = renderTcInstanceMethodAnnotation ann,
      locatedLabel = T.unpack (tcInstanceMethodName ann)
    }

renderLocatedAnnotation :: TcLocatedAnnotation -> String
renderLocatedAnnotation ann =
  renderSourceSpan (locatedSpan ann)
    <> " "
    <> locatedKind ann
    <> " => "
    <> locatedSummary ann

annotationKey :: TcLocatedAnnotation -> (Int, Int, Int, Int, String, String)
annotationKey ann =
  case locatedSpan ann of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      (startLine, startCol, endLine, endCol, locatedKind ann, locatedSummary ann)
    NoSourceSpan ->
      (maxBound, maxBound, maxBound, maxBound, locatedKind ann, locatedSummary ann)

renderSourceSpan :: SourceSpan -> String
renderSourceSpan span' =
  case span' of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      show startLine
        <> ":"
        <> show startCol
        <> "-"
        <> show endLine
        <> ":"
        <> show endCol
    NoSourceSpan -> "<no-source>"

renderTcAnnotation :: TcAnnotation -> String
renderTcAnnotation ann =
  "type="
    <> renderTcType (tcAnnType ann)
    <> renderListField " typeArgs" renderTcType (tcAnnTypeArgs ann)
    <> renderListField " evidence" renderEvTerm (tcAnnEvidenceTerms ann)
    <> renderListField " termArgs" renderTcType (tcAnnTermArgTypes ann)

renderTcAnnotationLabel :: TcAnnotation -> String
renderTcAnnotationLabel ann =
  unwords $
    map renderTypeApplication (tcAnnTypeArgs ann)
      <> map renderEvTerm (tcAnnEvidenceTerms ann)

renderTypeApplication :: TcType -> String
renderTypeApplication ty = "@" <> renderTcType ty

renderTcClassAnnotation :: TcClassAnnotation -> String
renderTcClassAnnotation ann =
  "methods=[" <> intercalate ", " (map renderTcClassMethodAnnotation (tcClassMethods ann)) <> "]"

renderTcClassMethodAnnotation :: TcClassMethodAnnotation -> String
renderTcClassMethodAnnotation method =
  T.unpack (tcClassMethodName method)
    <> "{index="
    <> show (tcClassMethodIndex method)
    <> ", type="
    <> renderTcType (tcClassMethodType method)
    <> ", tyVars="
    <> renderTyVarList (tcClassMethodTyVars method)
    <> ", dict="
    <> renderTcType (tcClassMethodDictType method)
    <> "}"

renderTcInstanceAnnotation :: TcInstanceAnnotation -> String
renderTcInstanceAnnotation ann =
  "dict="
    <> T.unpack (tcInstanceDictName ann)
    <> " type="
    <> renderTcType (tcInstanceDictType ann)
    <> renderListField " tyVars" renderTyVar (tcInstanceTyVars ann)
    <> renderListField " headTypes" renderTcType (tcInstanceHeadTypes ann)
    <> renderListField " context" renderTcDictBinderAnnotation (tcInstanceContextDicts ann)
    <> renderListField " methods" T.unpack (tcInstanceMethodOrder ann)

renderTcDictBinderAnnotation :: TcDictBinderAnnotation -> String
renderTcDictBinderAnnotation ann =
  T.unpack (tcDictBinderClassName ann)
    <> " "
    <> unwords (map renderTcType (tcDictBinderArgs ann))
    <> " :: "
    <> renderTcType (tcDictBinderType ann)

renderTcInstanceMethodAnnotation :: TcInstanceMethodAnnotation -> String
renderTcInstanceMethodAnnotation ann =
  T.unpack (tcInstanceMethodName ann) <> " type=" <> renderTcType (tcInstanceMethodType ann)

renderTyVarList :: [TyVarId] -> String
renderTyVarList = renderList renderTyVar

renderTyVar :: TyVarId -> String
renderTyVar = T.unpack . tvName

renderListField :: String -> (a -> String) -> [a] -> String
renderListField _ _ [] = ""
renderListField label renderItem xs = label <> "=" <> renderList renderItem xs

renderList :: (a -> String) -> [a] -> String
renderList renderItem xs = "[" <> intercalate ", " (map renderItem xs) <> "]"

renderEvTerm :: EvTerm -> String
renderEvTerm ev =
  case ev of
    EvVarTerm var -> renderEvVar var
    EvGiven pred' -> "given(" <> renderPred pred' <> ")"
    EvDict name typeArgs evidence ->
      "dict "
        <> T.unpack name
        <> renderListField " typeArgs" renderTcType typeArgs
        <> renderListField " evidence" renderEvTerm evidence
    EvCoercion coercion -> "coercion(" <> renderCoercion coercion <> ")"
    EvSuperClass inner index -> "super[" <> show index <> "](" <> renderEvTerm inner <> ")"
    EvCast inner coercion -> "cast(" <> renderEvTerm inner <> ", " <> renderCoercion coercion <> ")"

renderEvVar :: EvVar -> String
renderEvVar (EvVar (Unique unique)) = "$ev" <> show unique

renderCoercion :: Coercion -> String
renderCoercion coercion =
  case coercion of
    CoVar var -> renderEvVar var
    Refl ty -> "refl " <> renderTcType ty
    Sym inner -> "sym(" <> renderCoercion inner <> ")"
    Trans left right -> "trans(" <> renderCoercion left <> ", " <> renderCoercion right <> ")"
    TyConAppCo tc coercions -> T.unpack (tyConName tc) <> renderListField " coercions" renderCoercion coercions
    AxiomInstCo name types -> "axiom " <> T.unpack name <> renderListField " types" renderTcType types

renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred cls args -> T.unpack cls <> " " <> unwords (map renderTcType args)
    EqPred left right -> renderTcType left <> " ~ " <> renderTcType right

renderAnnotatedTcSource :: Text -> [TcLocatedAnnotation] -> String
renderAnnotatedTcSource source annotations =
  let sourceLines = T.lines source
      sorted = sortOn annotationKey annotations
      grouped = groupByLine sorted
   in intercalate "\n" (concatMap (renderSourceLine grouped) (zip [1 ..] sourceLines))

groupByLine :: [TcLocatedAnnotation] -> Map.Map Int [TcLocatedAnnotation]
groupByLine = foldr insertAnn Map.empty
  where
    insertAnn ann acc =
      case locatedSpan ann of
        SourceSpan _ startLine _ _ _ _ _ ->
          Map.insertWith (<>) startLine [ann] acc
        NoSourceSpan -> acc

renderSourceLine :: Map.Map Int [TcLocatedAnnotation] -> (Int, Text) -> [String]
renderSourceLine grouped (lineNum, sourceLine) =
  let annotations = maybe [] (sortOn annotationStartCol) (Map.lookup lineNum grouped)
   in T.unpack sourceLine : renderAnnotationLines annotations

annotationStartCol :: TcLocatedAnnotation -> Int
annotationStartCol ann =
  case locatedSpan ann of
    SourceSpan _ _ startCol _ _ _ _ -> startCol
    NoSourceSpan -> maxBound

renderAnnotationLines :: [TcLocatedAnnotation] -> [String]
renderAnnotationLines [] = []
renderAnnotationLines annotations =
  let items = [(annotationStartCol ann - 1, locatedLabel ann) | ann <- annotations]
      (markerItems, labeledItems) = partition (null . snd) items
      markerCol =
        case labeledItems of
          [] -> minimum (map fst markerItems)
          _ -> minimum (map fst labeledItems)
      markerLines = replicate (length markerItems) (replicate markerCol ' ' <> "\x2502")
   in markerLines <> layoutAnnotationLines labeledItems

type AnnotationItem = (Int, String)

layoutAnnotationLines :: [AnnotationItem] -> [String]
layoutAnnotationLines [] = []
layoutAnnotationLines items =
  let reversed = sortOn (Down . fst) items
      (currentLine, deferred) = packLine reversed
   in renderAnnotationLine currentLine deferred : layoutAnnotationLines deferred

packLine :: [AnnotationItem] -> ([AnnotationItem], [AnnotationItem])
packLine [] = ([], [])
packLine (rightmost : rest) =
  let go _ [] fitted deferred = (sortOn fst fitted, sortOn fst deferred)
      go minCol (item@(col, label) : remaining) fitted deferred =
        let annotEnd = col + 3 + length label
            fitsBeforePlaced = annotEnd < minCol
            crossesDeferred = any ((\d -> d > col && d < annotEnd) . fst) deferred
         in if fitsBeforePlaced && not crossesDeferred
              then go col remaining (item : fitted) deferred
              else go minCol remaining fitted (item : deferred)
   in go (fst rightmost) rest [rightmost] []

renderAnnotationLine :: [AnnotationItem] -> [AnnotationItem] -> String
renderAnnotationLine placedOnLine deferredItems =
  let deferredCols = map fst deferredItems
      buildLine _ [] [] = ""
      buildLine pos placed deferred =
        case (placed, deferred) of
          ((col, label) : restPlaced, _)
            | pos == col ->
                "\x2514\x2500 " <> label <> buildLine (pos + 3 + length label) restPlaced (filter (>= pos + 3 + length label) deferred)
          (_, d : restDeferred)
            | pos == d ->
                "\x2502" <> buildLine (pos + 1) placed restDeferred
          _ ->
            let nextPos = case (placed, deferred) of
                  ((col, _) : _, d : _) -> min col d
                  ((col, _) : _, []) -> col
                  ([], d : _) -> d
                padding = nextPos - pos
             in replicate padding ' ' <> buildLine nextPos placed deferred
   in buildLine 0 (sortOn fst placedOnLine) (sortOn id deferredCols)
