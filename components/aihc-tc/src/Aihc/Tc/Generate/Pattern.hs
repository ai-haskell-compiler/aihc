{-# LANGUAGE OverloadedStrings #-}

-- | Shared type-checking support for term patterns.
module Aihc.Tc.Generate.Pattern
  ( PatternCheck (..),
    annotatePatternBindings,
    checkPattern,
    checkPatterns,
    checkPatternsWithGivens,
    checkedPattern,
    withPatternBindings,
  )
where

import Aihc.Parser.Syntax
  ( Literal (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    RecordField (..),
    SourceSpan (..),
    TupleFlavor (..),
    UnqualifiedName (..),
    fromAnnotation,
    mkAnnotation,
    nameText,
    peelLiteralAnn,
    peelPatternAnn,
  )
import Aihc.Resolve (ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Annotations (PendingTcAnnotation, pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Instantiate (Instantiation (..), instantiate, instantiateWithArgs)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data PatternCheck = PatternCheck
  { pcBindings :: ![(UnqualifiedName, TcType)],
    pcWantedCts :: ![Ct],
    pcGivenCts :: ![Ct],
    pcPatterns :: ![Pattern]
  }
  deriving (Show)

instance Semigroup PatternCheck where
  left <> right =
    PatternCheck
      { pcBindings = pcBindings left <> pcBindings right,
        pcWantedCts = pcWantedCts left <> pcWantedCts right,
        pcGivenCts = pcGivenCts left <> pcGivenCts right,
        pcPatterns = pcPatterns left <> pcPatterns right
      }

instance Monoid PatternCheck where
  mempty = PatternCheck [] [] [] []

data GadtHandling
  = GadtAsWanted
  | GadtAsGiven
  deriving (Eq)

checkPatterns :: SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatterns = checkPatternsWith GadtAsWanted

checkPatternsWithGivens :: SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatternsWithGivens = checkPatternsWith GadtAsGiven

checkPatternsWith :: GadtHandling -> SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatternsWith gadtHandling sp = fmap mconcat . mapM (uncurry (checkPatternWith gadtHandling sp))

checkPattern :: SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPattern = checkPatternWith GadtAsWanted

checkPatternWith :: GadtHandling -> SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternWith gadtHandling sp pat scrutTy =
  case overloadedIntegerPatternLiteral pat of
    Just isNegative -> checkOverloadedIntegerPattern sp pat isNegative scrutTy
    Nothing -> checkPatternCore gadtHandling sp pat scrutTy

checkPatternCore :: GadtHandling -> SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternCore gadtHandling sp pat scrutTy =
  case pat of
    PVar name ->
      pure (checkedOnly pat) {pcBindings = [(name, scrutTy)]}
    PAnn ann inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
    PParen inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PParen (checkedPattern innerCheck)]}
    PWildcard {} -> pure (checkedOnly pat)
    PLit {} -> pure (checkedOnly pat)
    PNegLit {} -> pure (checkedOnly pat)
    PAs name inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcBindings = (name, scrutTy) : pcBindings innerCheck, pcPatterns = [PAs name (checkedPattern innerCheck)]}
    PStrict inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PStrict (checkedPattern innerCheck)]}
    PIrrefutable inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcPatterns = [PIrrefutable (checkedPattern innerCheck)]}
    PCon name _typeArgs subPats ->
      checkConPattern gadtHandling sp pat name subPats scrutTy
    PInfix lhs op rhs ->
      checkConPattern gadtHandling sp pat op [lhs, rhs] scrutTy
    PList items -> do
      elemTy <- freshMetaTv
      let listTy = listType elemTy
      eqCt <- wantedEq sp scrutTy listTy
      itemChecks <- checkPatternsWith gadtHandling sp [(item, elemTy) | item <- items]
      pure itemChecks {pcWantedCts = eqCt : pcWantedCts itemChecks, pcPatterns = [PList (pcPatterns itemChecks)]}
    PTuple flavor items -> do
      elemTys <- mapM (const freshMetaTv) items
      let tupleTy = TcTyCon (TyCon (tupleConText flavor (length items)) (length items)) elemTys
      eqCt <- wantedEq sp scrutTy tupleTy
      itemChecks <- checkPatternsWith gadtHandling sp (zip items elemTys)
      pure itemChecks {pcWantedCts = eqCt : pcWantedCts itemChecks, pcPatterns = [PTuple flavor (pcPatterns itemChecks)]}
    _ -> pure (checkedOnly pat)

checkedOnly :: Pattern -> PatternCheck
checkedOnly pat = mempty {pcPatterns = [pat]}

checkedPattern :: PatternCheck -> Pattern
checkedPattern check =
  case pcPatterns check of
    [pat] -> pat
    _ -> error "checkedPattern: expected exactly one checked pattern"

overloadedIntegerPatternLiteral :: Pattern -> Maybe Bool
overloadedIntegerPatternLiteral pat =
  case peelPatternAnn pat of
    PLit lit
      | isOverloadedIntegerLiteral lit -> Just False
    PNegLit lit
      | isOverloadedIntegerLiteral lit -> Just True
    _ -> Nothing

isOverloadedIntegerLiteral :: Literal -> Bool
isOverloadedIntegerLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ TInteger _ -> True
    _ -> False

checkOverloadedIntegerPattern :: SourceSpan -> Pattern -> Bool -> TcType -> TcM PatternCheck
checkOverloadedIntegerPattern sp pat isNegative scrutTy = do
  (fromIntegerPending, fromIntegerCts) <- checkPatternMethod sp pat "fromInteger" scrutTy (TcFunTy integerTy scrutTy)
  negateCheck <-
    if isNegative
      then Just <$> checkPatternMethod sp pat "negate" scrutTy (TcFunTy scrutTy scrutTy)
      else pure Nothing
  let eqTy = TcFunTy scrutTy (TcFunTy scrutTy boolTy)
  (eqPending, eqCts) <- checkPatternMethod sp pat "==" eqTy eqTy
  let methodAnnotations =
        [("fromInteger", fromIntegerPending)]
          <> maybe [] (\(pending, _) -> [("negate", pending)]) negateCheck
          <> [("==", eqPending)]
      pat' = foldr (uncurry attachPendingPatternAnnotation) pat methodAnnotations
      negateCts = maybe [] snd negateCheck
  pure
    PatternCheck
      { pcBindings = [],
        pcWantedCts = fromIntegerCts <> negateCts <> eqCts,
        pcGivenCts = [],
        pcPatterns = [pat']
      }

checkPatternMethod :: SourceSpan -> Pattern -> Text -> TcType -> TcType -> TcM (PendingTcAnnotation, [Ct])
checkPatternMethod sp pat name annotationTy expectedTy = do
  resolution <- requiredPatternResolution name pat
  (actualTy, typeArgs, methodCts) <- inferResolvedPatternMethod sp name resolution
  methodEq <- wantedMethodEq sp name actualTy expectedTy
  pure
    ( pendingAnnotation
        annotationTy
        typeArgs
        (map ctEvVar methodCts)
        [],
      methodCts <> [methodEq]
    )

integerTy :: TcType
integerTy = TcTyCon (TyCon "Integer" 0) []

boolTy :: TcType
boolTy = TcTyCon (TyCon "Bool" 0) []

wantedMethodEq :: SourceSpan -> Text -> TcType -> TcType -> TcM Ct
wantedMethodEq sp method actual expected = do
  ev <- freshEvVar
  pure $
    mkWantedEqCt
      TypeTrace
        { typeTraceType = actual,
          typeTraceRole = ActualType,
          typeTraceOrigin = ConstraintTypeOrigin (OccurrenceOf method)
        }
      TypeTrace
        { typeTraceType = expected,
          typeTraceRole = ExpectedType,
          typeTraceOrigin = ConstraintTypeOrigin (LitOrigin sp)
        }
      ev
      (LitOrigin sp)
      sp

inferResolvedPatternMethod :: SourceSpan -> Text -> ResolutionAnnotation -> TcM (TcType, [TcType], [Ct])
inferResolvedPatternMethod sp displayName resolution = do
  mBinder <- lookupResolvedTerm displayName (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp displayName) (instPreds inst)
      pure (instType inst, instTypeArgs inst, cts)
    Just (TcMonoIdBinder ty) ->
      pure (ty, [], [])
    Nothing ->
      abortTc ("resolved " <> T.unpack displayName <> " missing from type environment: " <> show (resolutionTarget resolution))

predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name pred' = do
  ev <- freshEvVar
  pure (mkWantedCt pred' ev (OccurrenceOf name) sp)

requiredPatternResolution :: Text -> Pattern -> TcM ResolutionAnnotation
requiredPatternResolution name pat =
  case [resolution | resolution <- patternResolutions pat, resolutionName resolution == name, resolutionNamespace resolution == ResolutionNamespaceTerm] of
    resolution : _ -> pure resolution
    [] -> do
      emitError NoSourceSpan (OtherError ("missing resolver annotation for overloaded pattern method " <> T.unpack name))
      abortTc ("missing resolver annotation for overloaded pattern method " <> T.unpack name)

patternResolutions :: Pattern -> [ResolutionAnnotation]
patternResolutions pat =
  case pat of
    PAnn ann inner -> mapMaybe fromAnnotation [ann] <> patternResolutions inner
    PParen inner -> patternResolutions inner
    PStrict inner -> patternResolutions inner
    PIrrefutable inner -> patternResolutions inner
    PAs _ inner -> patternResolutions inner
    PTypeSig inner _ -> patternResolutions inner
    _ -> []

attachPendingPatternAnnotation :: Text -> PendingTcAnnotation -> Pattern -> Pattern
attachPendingPatternAnnotation target pending pat =
  case pat of
    PAnn ann inner ->
      case fromAnnotation ann of
        Just resolution
          | resolutionName resolution == target,
            resolutionNamespace resolution == ResolutionNamespaceTerm ->
              PAnn (mkAnnotation pending) (PAnn ann inner)
        _ -> PAnn ann (attachPendingPatternAnnotation target pending inner)
    PParen inner -> PParen (attachPendingPatternAnnotation target pending inner)
    PStrict inner -> PStrict (attachPendingPatternAnnotation target pending inner)
    PIrrefutable inner -> PIrrefutable (attachPendingPatternAnnotation target pending inner)
    PAs name inner -> PAs name (attachPendingPatternAnnotation target pending inner)
    PTypeSig inner ty -> PTypeSig (attachPendingPatternAnnotation target pending inner) ty
    _ -> pat

annotatePatternBindings :: [(UnqualifiedName, TcType)] -> Pattern -> Pattern
annotatePatternBindings bindings =
  go
  where
    go pat =
      case pat of
        PAnn ann inner -> PAnn ann (go inner)
        PVar name -> PVar (annotateBinderName bindings name)
        PParen inner -> PParen (go inner)
        PAs name inner -> PAs (annotateBinderName bindings name) (go inner)
        PStrict inner -> PStrict (go inner)
        PIrrefutable inner -> PIrrefutable (go inner)
        PList items -> PList (map go items)
        PTuple flavor items -> PTuple flavor (map go items)
        PUnboxedSum alt arity inner -> PUnboxedSum alt arity (go inner)
        PInfix lhs op rhs -> PInfix (go lhs) op (go rhs)
        PView expr inner -> PView expr (go inner)
        PCon name typeArgs subPats -> PCon name typeArgs (map go subPats)
        PRecord name fields wildcard -> PRecord name (map annotateRecordField fields) wildcard
        PTypeSig inner type' -> PTypeSig (go inner) type'
        PSplice expr -> PSplice expr
        _ -> pat

    annotateRecordField :: RecordField Pattern -> RecordField Pattern
    annotateRecordField field =
      field {recordFieldValue = go (recordFieldValue field)}

annotateBinderName :: [(UnqualifiedName, TcType)] -> UnqualifiedName -> UnqualifiedName
annotateBinderName bindings name =
  case lookup name bindings of
    Nothing -> name
    Just ty -> name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation (pendingAnnotation ty [] [] [])]}

checkConPattern :: GadtHandling -> SourceSpan -> Pattern -> Name -> [Pattern] -> TcType -> TcM PatternCheck
checkConPattern gadtHandling sp originalPat conSyntax subPats scrutTy = do
  let conName = patternNameText conSyntax
  target <- resolvedTermTarget conSyntax
  mBinder <- lookupResolvedTerm conName target
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      (conTy, _preds) <- instantiate scheme
      (argTys, conResTy) <- splitConTy (length subPats) conTy
      scrutCt <- constructorScrutineeCt gadtHandling sp conName scrutTy conResTy
      subCheck <- checkPatternsWith gadtHandling sp (zip subPats argTys)
      pure
        subCheck
          { pcWantedCts = fst scrutCt <> pcWantedCts subCheck,
            pcGivenCts = snd scrutCt <> pcGivenCts subCheck,
            pcPatterns = [replaceConstructorSubpatterns originalPat (pcPatterns subCheck)]
          }
    Just other ->
      abortTc ("resolved constructor is not an identifier binder: " <> show conName <> " resolved as " <> show target <> " with binder " <> show other)
    Nothing ->
      abortTc ("resolved constructor missing from type environment: " <> show conName <> " resolved as " <> show target)

replaceConstructorSubpatterns :: Pattern -> [Pattern] -> Pattern
replaceConstructorSubpatterns pat subPats =
  case pat of
    PCon name typeArgs _ -> PCon name typeArgs subPats
    PInfix _ op _ ->
      case subPats of
        [lhs, rhs] -> PInfix lhs op rhs
        _ -> pat
    _ -> pat

constructorScrutineeCt :: GadtHandling -> SourceSpan -> Text -> TcType -> TcType -> TcM ([Ct], [Ct])
constructorScrutineeCt gadtHandling sp conName scrutTy conResTy = do
  ev <- freshEvVar
  gadtCon <- isGadtCon conName
  if gadtHandling == GadtAsGiven && gadtCon
    then
      pure
        ( [],
          [ Ct
              { ctPred = EqPred scrutTy conResTy,
                ctFlavor = Given,
                ctEvVar = ev,
                ctOrigin = AppOrigin sp,
                ctProvenance = FromCtOrigin (AppOrigin sp),
                ctLoc = sp
              }
          ]
        )
    else do
      let wantedCt = mkWantedCt (EqPred scrutTy conResTy) ev (AppOrigin sp) sp
      pure ([wantedCt], [])

splitConTy :: Int -> TcType -> TcM ([TcType], TcType)
splitConTy 0 ty = pure ([], ty)
splitConTy n (TcFunTy arg rest) = do
  (args, result) <- splitConTy (n - 1) rest
  pure (arg : args, result)
splitConTy n result = do
  missingArgs <- mapM (const freshMetaTv) [1 .. n]
  pure (missingArgs, result)

wantedEq :: SourceSpan -> TcType -> TcType -> TcM Ct
wantedEq sp left right = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred left right) ev (AppOrigin sp) sp)

withPatternBindings :: [(UnqualifiedName, TcType)] -> TcM a -> TcM a
withPatternBindings [] action = action
withPatternBindings ((name, ty) : rest) action =
  extendResolvedTermEnv name (TcMonoIdBinder ty) (withPatternBindings rest action)

listType :: TcType -> TcType
listType elemTy = TcTyCon (TyCon "[]" 1) [elemTy]

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor arity =
  case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = T.replicate (n - 1) ","

patternNameText :: Name -> Text
patternNameText name =
  case nameQualifier name of
    Nothing -> nameText name
    Just qualifier -> qualifier <> "." <> nameText name
