{-# LANGUAGE OverloadedStrings #-}

-- | Shared type-checking support for term patterns.
module Aihc.Tc.Generate.Pattern
  ( PatternCheck (..),
    checkPattern,
    checkPatterns,
    checkPatternsWithGivens,
    withPatternBindings,
  )
where

import Aihc.Parser.Syntax
  ( Name (..),
    Pattern (..),
    SourceSpan,
    nameText,
    unqualifiedNameText,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Instantiate (instantiate)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Text (Text)

data PatternCheck = PatternCheck
  { pcBindings :: ![(Text, TcType)],
    pcWantedCts :: ![Ct],
    pcGivenCts :: ![Ct]
  }
  deriving (Show)

instance Semigroup PatternCheck where
  left <> right =
    PatternCheck
      { pcBindings = pcBindings left <> pcBindings right,
        pcWantedCts = pcWantedCts left <> pcWantedCts right,
        pcGivenCts = pcGivenCts left <> pcGivenCts right
      }

instance Monoid PatternCheck where
  mempty = PatternCheck [] [] []

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
  case pat of
    PVar name ->
      pure mempty {pcBindings = [(unqualifiedNameText name, scrutTy)]}
    PAnn _ann inner -> checkPatternWith gadtHandling sp inner scrutTy
    PParen inner -> checkPatternWith gadtHandling sp inner scrutTy
    PWildcard {} -> pure mempty
    PLit {} -> pure mempty
    PNegLit {} -> pure mempty
    PAs name inner -> do
      innerCheck <- checkPatternWith gadtHandling sp inner scrutTy
      pure innerCheck {pcBindings = (unqualifiedNameText name, scrutTy) : pcBindings innerCheck}
    PStrict inner -> checkPatternWith gadtHandling sp inner scrutTy
    PIrrefutable inner -> checkPatternWith gadtHandling sp inner scrutTy
    PCon name _typeArgs subPats ->
      checkConPattern gadtHandling sp (patternNameText name) subPats scrutTy
    PInfix lhs op rhs ->
      checkConPattern gadtHandling sp (patternNameText op) [lhs, rhs] scrutTy
    PList items -> do
      elemTy <- freshMetaTv
      let listTy = listType elemTy
      eqCt <- wantedEq sp scrutTy listTy
      itemChecks <- checkPatternsWith gadtHandling sp [(item, elemTy) | item <- items]
      pure itemChecks {pcWantedCts = eqCt : pcWantedCts itemChecks}
    _ -> pure mempty

checkConPattern :: GadtHandling -> SourceSpan -> Text -> [Pattern] -> TcType -> TcM PatternCheck
checkConPattern gadtHandling sp conName subPats scrutTy = do
  mBinder <- lookupTerm conName
  case mBinder of
    Just (TcIdBinder _ scheme _) -> do
      (conTy, _preds) <- instantiate scheme
      (argTys, conResTy) <- splitConTy (length subPats) conTy
      scrutCt <- constructorScrutineeCt gadtHandling sp conName scrutTy conResTy
      subCheck <- checkPatternsWith gadtHandling sp (zip subPats argTys)
      pure
        subCheck
          { pcWantedCts = fst scrutCt <> pcWantedCts subCheck,
            pcGivenCts = snd scrutCt <> pcGivenCts subCheck
          }
    _ -> pure mempty

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

withPatternBindings :: [(Text, TcType)] -> TcM a -> TcM a
withPatternBindings [] action = action
withPatternBindings ((name, ty) : rest) action =
  extendTermEnv name (TcMonoIdBinder name ty) (withPatternBindings rest action)

listType :: TcType -> TcType
listType elemTy = TcTyCon (TyCon "[]" 1) [elemTy]

patternNameText :: Name -> Text
patternNameText name =
  case nameQualifier name of
    Nothing -> nameText name
    Just qualifier -> qualifier <> "." <> nameText name
