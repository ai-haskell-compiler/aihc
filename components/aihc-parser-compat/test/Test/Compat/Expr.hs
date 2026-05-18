{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Compat.Expr
  ( comparisonDump,
    exprCompatTests,
    renderGhc,
    sameStructural,
  )
where

import Aihc.Parser qualified as Aihc
import Aihc.Parser.Compat (toGhcHsExpr)
import Aihc.Parser.Compat.Internal.Ghc
  ( compatGhcExtensions,
    normalizeGhcAst,
    parseGhcLocatedExpr,
  )
import Aihc.Parser.Parens (addExprParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax hiding (Match, RecordCon)
import Data.ByteString (ByteString)
import Data.Data (Data, gmapQ, showConstr, toConstr)
import Data.List (isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (cast)
import GHC.Data.FastString (FastString)
import GHC.Hs
  ( ArithSeqInfo (..),
    DotFieldOcc (..),
    FieldOcc (..),
    GRHS (..),
    GRHSs (..),
    GhcPs,
    HsCmd (..),
    HsCmdTop (..),
    HsExpr (..),
    HsFieldBind (..),
    HsLocalBindsLR (..),
    HsQuote (..),
    HsRecFields (..),
    HsTupArg (..),
    HsTypedSplice (..),
    HsUntypedSplice (..),
    HsValBindsLR (..),
    LGRHS,
    LHsCmd,
    LHsCmdTop,
    LHsExpr,
    LHsRecUpdFields (..),
    LMatch,
    Match (..),
    MatchGroup (..),
    ParStmtBlock (..),
    StmtLR (..),
  )
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated, unLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Language.Haskell.Syntax.Basic (FieldLabelString)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Expr ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

exprCompatTests :: TestTree
exprCompatTests =
  testGroup
    "expr"
    [ testGroup
        "examples"
        [ example "variable" (EVar "x"),
          example "integer" (EInt 42 TInteger "42"),
          example "empty list" (EList []),
          example "unit tuple" (ETuple Boxed []),
          example "unboxed unit tuple" (ETuple Unboxed []),
          example "tuple section" (ETuple Boxed [Just (EVar "x"), Nothing]),
          example "unboxed sum" (EUnboxedSum 1 3 (EVar "x")),
          example "infix application" (EInfix (EVar "x") "+" (EVar "y")),
          example "left section" (ESectionL (EVar "x") "+"),
          example "right section" (ESectionR "+" (EVar "x")),
          example "lambda" (ELambdaPats [PVar "x"] (EVar "x")),
          example "lambda-case" (ELambdaCase [CaseAlt [] (PVar "x") (UnguardedRhs [] (EVar "x") Nothing)]),
          example "do" (EDo [DoExpr (EVar "x")] DoPlain),
          example "mdo" (EDo [DoExpr (EVar "x")] DoMdo),
          example "qualified do" (EDo [DoExpr (EVar "x")] (DoQualified "M")),
          example "record construction" (ERecordCon "C" [RecordField "x" (EVar "y") False] False),
          example "record update" (ERecordUpd (EVar "r") [RecordField "x" (EVar "y") False]),
          example "type application" (ETypeApp (EVar "f") (TCon "Int" Unpromoted)),
          example "type signature" (ETypeSig (EVar "x") (TCon "Int" Unpromoted)),
          example "TH expression quote" (ETHExpQuote (EVar "x")),
          example "TH splice" (ETHSplice (EParen (EVar "x"))),
          example "record dot" (EGetField (EVar "r") "x"),
          example "record projection" (EGetFieldProjection ["x", "y"]),
          testCase "repeated infix application matches GHC" repeatedInfixParser
        ],
      QC.testProperty "generated expr converts to normalized GHC parsed AST" prop_exprCompat
    ]

repeatedInfixParser :: Assertion
repeatedInfixParser = do
  let source = "1 + 2 + 3"
  aihcExpr <-
    case Aihc.parseExpr Aihc.defaultConfig source of
      Aihc.ParseErr err -> assertFailure (Aihc.formatParseErrors "<compat-test>" (Just source) err)
      Aihc.ParseOk expr -> pure expr
  ghcExpr <-
    case parseGhcLocatedExpr compatGhcExtensions source of
      Left err -> assertFailure (T.unpack err)
      Right expr -> pure (normalizeGhcAst (unLoc expr))
  let convertedExpr = toGhcHsExpr aihcExpr
  assertBool
    ("expected: " <> renderGhc ghcExpr <> "\nactual: " <> renderGhc convertedExpr)
    (eqHsExprIgnoringAnnotations ghcExpr convertedExpr)

prop_exprCompat :: Expr -> QC.Property
prop_exprCompat expr =
  let parenthesized = addExprParens expr
      source = renderExpr parenthesized
   in QC.counterexample (T.unpack source) $
        case parseGhcLocatedExpr compatGhcExtensions source of
          Left err -> QC.counterexample (T.unpack err) False
          Right parsed ->
            let expected = normalizeGhcAst (unLoc parsed)
                actual = toGhcHsExpr parenthesized
             in QC.counterexample (exprMismatch expected actual) $
                  eqHsExprIgnoringAnnotations expected actual

example :: TestName -> Expr -> TestTree
example label expr =
  testCase label $ do
    let parenthesized = addExprParens expr
        source = renderExpr parenthesized
    case parseGhcLocatedExpr compatGhcExtensions source of
      Left err -> assertFailure (T.unpack err <> "\nsource:\n" <> T.unpack source)
      Right parsed -> do
        let expected = normalizeGhcAst (unLoc parsed)
            actual = toGhcHsExpr parenthesized
        assertBool (T.unpack source <> "\n" <> exprMismatch expected actual) $
          eqHsExprIgnoringAnnotations expected actual

renderExpr :: Expr -> Text
renderExpr = renderStrict . layoutPretty defaultLayoutOptions . pretty

exprMismatch :: HsExpr GhcPs -> HsExpr GhcPs -> String
exprMismatch expected actual =
  unlines
    [ "expected pretty: " <> renderGhc expected,
      "actual pretty: " <> renderGhc actual,
      "expected comparison tree:",
      comparisonDump expected,
      "actual comparison tree:",
      comparisonDump actual
    ]

eqHsExprIgnoringAnnotations :: HsExpr GhcPs -> HsExpr GhcPs -> Bool
eqHsExprIgnoringAnnotations left right =
  case (left, right) of
    (HsVar _ a, HsVar _ b) -> sameStructural (unLoc a) (unLoc b)
    (HsOverLabel _ a, HsOverLabel _ b) -> a == b
    (HsIPVar _ a, HsIPVar _ b) -> a == b
    (HsOverLit _ a, HsOverLit _ b) -> sameStructural a b
    (HsLit _ a, HsLit _ b) -> sameStructural a b
    (HsLam _ variantA matchesA, HsLam _ variantB matchesB) ->
      variantA == variantB && eqMatchGroup matchesA matchesB
    (HsApp _ funA argA, HsApp _ funB argB) ->
      eqLocatedHsExpr funA funB && eqLocatedHsExpr argA argB
    (HsAppType _ funA tyA, HsAppType _ funB tyB) ->
      eqLocatedHsExpr funA funB && sameStructural tyA tyB
    (OpApp _ lhsA opA rhsA, OpApp _ lhsB opB rhsB) ->
      eqLocatedHsExpr lhsA lhsB && eqLocatedHsExpr opA opB && eqLocatedHsExpr rhsA rhsB
    (NegApp _ exprA syntaxA, NegApp _ exprB syntaxB) ->
      eqLocatedHsExpr exprA exprB && sameStructural syntaxA syntaxB
    (HsPar _ exprA, HsPar _ exprB) ->
      eqLocatedHsExpr exprA exprB
    (HsPar _ exprA, exprB) ->
      eqHsExprIgnoringAnnotations (unLoc exprA) exprB
    (exprA, HsPar _ exprB) ->
      eqHsExprIgnoringAnnotations exprA (unLoc exprB)
    (SectionL _ exprA opA, SectionL _ exprB opB) ->
      eqLocatedHsExpr exprA exprB && eqLocatedHsExpr opA opB
    (SectionR _ opA exprA, SectionR _ opB exprB) ->
      eqLocatedHsExpr opA opB && eqLocatedHsExpr exprA exprB
    (ExplicitTuple _ argsA boxityA, ExplicitTuple _ argsB boxityB) ->
      boxityA == boxityB && eqList eqHsTupArg argsA argsB
    (ExplicitSum _ tagA arityA exprA, ExplicitSum _ tagB arityB exprB) ->
      tagA == tagB && arityA == arityB && eqLocatedHsExpr exprA exprB
    (HsCase _ scrutA matchesA, HsCase _ scrutB matchesB) ->
      eqLocatedHsExpr scrutA scrutB && eqMatchGroup matchesA matchesB
    (HsIf _ condA yesA noA, HsIf _ condB yesB noB) ->
      eqLocatedHsExpr condA condB && eqLocatedHsExpr yesA yesB && eqLocatedHsExpr noA noB
    (HsMultiIf _ rhssA, HsMultiIf _ rhssB) ->
      eqNonEmpty eqLocatedGRHS rhssA rhssB
    (HsLet _ bindsA exprA, HsLet _ bindsB exprB) ->
      eqLocalBinds bindsA bindsB && eqLocatedHsExpr exprA exprB
    (HsDo _ flavorA stmtsA, HsDo _ flavorB stmtsB) ->
      flavorA == flavorB && eqList eqLocatedStmt (unLoc stmtsA) (unLoc stmtsB)
    (ExplicitList _ exprsA, ExplicitList _ exprsB) ->
      eqList eqLocatedHsExpr exprsA exprsB
    (RecordCon {rcon_con = conA, rcon_flds = fieldsA}, RecordCon {rcon_con = conB, rcon_flds = fieldsB}) ->
      sameStructural (unLoc conA) (unLoc conB) && eqHsRecFields fieldsA fieldsB
    (RecordUpd {rupd_expr = exprA, rupd_flds = fieldsA}, RecordUpd {rupd_expr = exprB, rupd_flds = fieldsB}) ->
      eqLocatedHsExpr exprA exprB && eqHsRecUpdFields fieldsA fieldsB
    (HsGetField {gf_expr = exprA, gf_field = fieldA}, HsGetField {gf_expr = exprB, gf_field = fieldB}) ->
      eqLocatedHsExpr exprA exprB && eqDotFieldOcc (unLoc fieldA) (unLoc fieldB)
    (HsProjection {proj_flds = fieldsA}, HsProjection {proj_flds = fieldsB}) ->
      eqList eqDotFieldOcc (NE.toList fieldsA) (NE.toList fieldsB)
    (ExprWithTySig _ exprA tyA, ExprWithTySig _ exprB tyB) ->
      eqLocatedHsExpr exprA exprB && sameStructural tyA tyB
    (ArithSeq _ witnessA seqA, ArithSeq _ witnessB seqB) ->
      sameStructural witnessA witnessB && eqArithSeqInfo seqA seqB
    (HsTypedBracket _ exprA, HsTypedBracket _ exprB) ->
      eqLocatedHsExpr exprA exprB
    (HsUntypedBracket _ quoteA, HsUntypedBracket _ quoteB) ->
      eqHsQuote quoteA quoteB
    (HsTypedSplice _ spliceA, HsTypedSplice _ spliceB) ->
      eqHsTypedSplice spliceA spliceB
    (HsUntypedSplice _ spliceA, HsUntypedSplice _ spliceB) ->
      eqHsUntypedSplice spliceA spliceB
    (HsProc _ patA cmdA, HsProc _ patB cmdB) ->
      sameStructural patA patB && eqLocatedHsCmdTop cmdA cmdB
    (HsStatic _ exprA, HsStatic _ exprB) ->
      eqLocatedHsExpr exprA exprB
    (HsPragE _ pragA exprA, HsPragE _ pragB exprB) ->
      sameStructural pragA pragB && eqLocatedHsExpr exprA exprB
    (HsEmbTy _ tyA, HsEmbTy _ tyB) ->
      sameStructural tyA tyB
    (HsHole a, HsHole b) ->
      sameConstructor a b
    (HsForAll _ telescopeA exprA, HsForAll _ telescopeB exprB) ->
      sameStructural telescopeA telescopeB && eqLocatedHsExpr exprA exprB
    (HsQual _ contextA exprA, HsQual _ contextB exprB) ->
      eqList eqLocatedHsExpr (unLoc contextA) (unLoc contextB) && eqLocatedHsExpr exprA exprB
    (HsFunArr _ multA argA resA, HsFunArr _ multB argB resB) ->
      sameStructural multA multB && eqLocatedHsExpr argA argB && eqLocatedHsExpr resA resB
    _ -> False

eqLocatedHsExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
eqLocatedHsExpr left right = eqHsExprIgnoringAnnotations (unLoc left) (unLoc right)

eqHsTupArg :: HsTupArg GhcPs -> HsTupArg GhcPs -> Bool
eqHsTupArg left right =
  case (left, right) of
    (Present _ exprA, Present _ exprB) -> eqLocatedHsExpr exprA exprB
    (Missing _, Missing _) -> True
    _ -> False

eqMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs) -> Bool
eqMatchGroup left right =
  case (left, right) of
    (MG _ matchesA, MG _ matchesB) -> eqList eqLocatedMatch (unLoc matchesA) (unLoc matchesB)

eqLocatedMatch :: LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs) -> Bool
eqLocatedMatch left right =
  case (unLoc left, unLoc right) of
    (Match _ contextA patsA grhssA, Match _ contextB patsB grhssB) ->
      sameStructural contextA contextB && sameStructural patsA patsB && eqGRHSs grhssA grhssB

eqGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> GRHSs GhcPs (LHsExpr GhcPs) -> Bool
eqGRHSs left right =
  case (left, right) of
    (GRHSs _ grhssA bindsA, GRHSs _ grhssB bindsB) ->
      eqNonEmpty eqLocatedGRHS grhssA grhssB && eqLocalBinds bindsA bindsB

eqLocatedGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs) -> Bool
eqLocatedGRHS left right =
  case (unLoc left, unLoc right) of
    (GRHS _ guardsA bodyA, GRHS _ guardsB bodyB) ->
      eqList eqLocatedStmt guardsA guardsB && eqLocatedHsExpr bodyA bodyB

eqLocatedStmt :: GenLocated location (StmtLR GhcPs GhcPs (LHsExpr GhcPs)) -> GenLocated location' (StmtLR GhcPs GhcPs (LHsExpr GhcPs)) -> Bool
eqLocatedStmt left right =
  case (unLoc left, unLoc right) of
    (LastStmt _ bodyA _ _, LastStmt _ bodyB _ _) ->
      eqLocatedHsExpr bodyA bodyB
    (LastStmt _ bodyA _ _, BodyStmt _ bodyB _ _) ->
      eqLocatedHsExpr bodyA bodyB
    (BodyStmt _ bodyA _ _, LastStmt _ bodyB _ _) ->
      eqLocatedHsExpr bodyA bodyB
    (BindStmt _ patA bodyA, BindStmt _ patB bodyB) ->
      sameStructural patA patB && eqLocatedHsExpr bodyA bodyB
    (BodyStmt _ bodyA _ _, BodyStmt _ bodyB _ _) ->
      eqLocatedHsExpr bodyA bodyB
    (LetStmt _ bindsA, LetStmt _ bindsB) ->
      eqLocalBinds bindsA bindsB
    (ParStmt _ blocksA _ _, ParStmt _ blocksB _ _) ->
      eqNonEmpty eqParStmtBlock blocksA blocksB
    (TransStmt {trS_form = formA, trS_using = usingA, trS_by = byA}, TransStmt {trS_form = formB, trS_using = usingB, trS_by = byB}) ->
      sameConstructor formA formB && eqLocatedHsExpr usingA usingB && eqMaybe eqLocatedHsExpr byA byB
    (RecStmt {recS_stmts = stmtsA}, RecStmt {recS_stmts = stmtsB}) ->
      eqList eqLocatedStmt (unLoc stmtsA) (unLoc stmtsB)
    _ -> False

eqLocatedHsCmdTop :: LHsCmdTop GhcPs -> LHsCmdTop GhcPs -> Bool
eqLocatedHsCmdTop left right =
  case (unLoc left, unLoc right) of
    (HsCmdTop _ cmdA, HsCmdTop _ cmdB) -> eqLocatedHsCmd cmdA cmdB

eqLocatedHsCmd :: LHsCmd GhcPs -> LHsCmd GhcPs -> Bool
eqLocatedHsCmd left right =
  eqHsCmdIgnoringAnnotations (unLoc left) (unLoc right)

eqHsCmdIgnoringAnnotations :: HsCmd GhcPs -> HsCmd GhcPs -> Bool
eqHsCmdIgnoringAnnotations left right =
  case (left, right) of
    (HsCmdArrApp _ lhsA rhsA appTyA _, HsCmdArrApp _ lhsB rhsB appTyB _) ->
      eqLocatedHsExpr lhsA lhsB && eqLocatedHsExpr rhsA rhsB && sameConstructor appTyA appTyB
    (HsCmdArrForm _ opA fixityA argsA, HsCmdArrForm _ opB fixityB argsB) ->
      eqLocatedHsExpr opA opB && fixityA == fixityB && eqList eqLocatedHsCmdTop argsA argsB
    (HsCmdApp _ funA argA, HsCmdApp _ funB argB) ->
      eqLocatedHsCmd funA funB && eqLocatedHsExpr argA argB
    (HsCmdLam _ variantA matchesA, HsCmdLam _ variantB matchesB) ->
      variantA == variantB && sameStructural matchesA matchesB
    (HsCmdPar _ cmdA, HsCmdPar _ cmdB) ->
      eqLocatedHsCmd cmdA cmdB
    (HsCmdCase _ scrutA matchesA, HsCmdCase _ scrutB matchesB) ->
      eqLocatedHsExpr scrutA scrutB && sameStructural matchesA matchesB
    (HsCmdIf _ _ condA yesA noA, HsCmdIf _ _ condB yesB noB) ->
      eqLocatedHsExpr condA condB && eqLocatedHsCmd yesA yesB && eqLocatedHsCmd noA noB
    (HsCmdLet _ bindsA cmdA, HsCmdLet _ bindsB cmdB) ->
      eqLocalBinds bindsA bindsB && eqLocatedHsCmd cmdA cmdB
    (HsCmdDo _ stmtsA, HsCmdDo _ stmtsB) ->
      eqList eqLocatedCmdStmt (unLoc stmtsA) (unLoc stmtsB)
    _ -> False

eqLocatedCmdStmt :: GenLocated location (StmtLR GhcPs GhcPs (LHsCmd GhcPs)) -> GenLocated location' (StmtLR GhcPs GhcPs (LHsCmd GhcPs)) -> Bool
eqLocatedCmdStmt left right =
  case (unLoc left, unLoc right) of
    (LastStmt _ bodyA _ _, LastStmt _ bodyB _ _) ->
      eqLocatedHsCmd bodyA bodyB
    (LastStmt _ bodyA _ _, BodyStmt _ bodyB _ _) ->
      eqLocatedHsCmd bodyA bodyB
    (BodyStmt _ bodyA _ _, LastStmt _ bodyB _ _) ->
      eqLocatedHsCmd bodyA bodyB
    (BindStmt _ patA bodyA, BindStmt _ patB bodyB) ->
      sameStructural patA patB && eqLocatedHsCmd bodyA bodyB
    (BodyStmt _ bodyA _ _, BodyStmt _ bodyB _ _) ->
      eqLocatedHsCmd bodyA bodyB
    (LetStmt _ bindsA, LetStmt _ bindsB) ->
      eqLocalBinds bindsA bindsB
    (RecStmt {recS_stmts = stmtsA}, RecStmt {recS_stmts = stmtsB}) ->
      eqList eqLocatedCmdStmt (unLoc stmtsA) (unLoc stmtsB)
    _ -> False

eqParStmtBlock :: ParStmtBlock GhcPs GhcPs -> ParStmtBlock GhcPs GhcPs -> Bool
eqParStmtBlock (ParStmtBlock _ stmtsA _ _) (ParStmtBlock _ stmtsB _ _) =
  eqList eqLocatedStmt stmtsA stmtsB

eqMaybe :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
eqMaybe eq left right =
  case (left, right) of
    (Nothing, Nothing) -> True
    (Just a, Just b) -> eq a b
    _ -> False

eqLocalBinds :: HsLocalBindsLR GhcPs GhcPs -> HsLocalBindsLR GhcPs GhcPs -> Bool
eqLocalBinds left right =
  sameStructural left right || bothEmpty left right
  where
    bothEmpty left' right' = emptyLocalBindsLike left' && emptyLocalBindsLike right'

eqHsRecFields :: HsRecFields GhcPs (LHsExpr GhcPs) -> HsRecFields GhcPs (LHsExpr GhcPs) -> Bool
eqHsRecFields left right =
  case (left, right) of
    (HsRecFields _ fieldsA dotdotA, HsRecFields _ fieldsB dotdotB) ->
      eqList eqLocatedFieldBind fieldsA fieldsB && sameConstructor dotdotA dotdotB

eqHsRecUpdFields :: LHsRecUpdFields GhcPs -> LHsRecUpdFields GhcPs -> Bool
eqHsRecUpdFields left right =
  case (left, right) of
    (RegularRecUpdFields _ fieldsA, RegularRecUpdFields _ fieldsB) ->
      eqList eqLocatedFieldBind fieldsA fieldsB
    (OverloadedRecUpdFields _ fieldsA, OverloadedRecUpdFields _ fieldsB) ->
      eqList eqLocatedFieldBind fieldsA fieldsB
    (RegularRecUpdFields _ [], OverloadedRecUpdFields _ []) ->
      True
    (OverloadedRecUpdFields _ [], RegularRecUpdFields _ []) ->
      True
    _ -> False

eqDotFieldOcc :: DotFieldOcc GhcPs -> DotFieldOcc GhcPs -> Bool
eqDotFieldOcc (DotFieldOcc _ labelA) (DotFieldOcc _ labelB) =
  unLoc labelA == unLoc labelB

eqLocatedFieldBind :: (Data lhs) => GenLocated location (HsFieldBind lhs (LHsExpr GhcPs)) -> GenLocated location (HsFieldBind lhs (LHsExpr GhcPs)) -> Bool
eqLocatedFieldBind left right =
  let HsFieldBind _ lhsA rhsA punA = unLoc left
      HsFieldBind _ lhsB rhsB punB = unLoc right
   in sameFieldLhs lhsA lhsB && eqLocatedHsExpr rhsA rhsB && punA == punB

sameFieldLhs :: (Data a, Data b) => a -> b -> Bool
sameFieldLhs left right =
  case (cast left, cast right) of
    (Just (FieldOcc _ labelA :: FieldOcc GhcPs), Just (FieldOcc _ labelB :: FieldOcc GhcPs)) ->
      sameStructural (unLoc labelA) (unLoc labelB)
    _ -> sameStructural left right

eqArithSeqInfo :: ArithSeqInfo GhcPs -> ArithSeqInfo GhcPs -> Bool
eqArithSeqInfo left right =
  case (left, right) of
    (From fromA, From fromB) ->
      eqLocatedHsExpr fromA fromB
    (FromThen fromA thenA, FromThen fromB thenB) ->
      eqLocatedHsExpr fromA fromB && eqLocatedHsExpr thenA thenB
    (FromTo fromA toA, FromTo fromB toB) ->
      eqLocatedHsExpr fromA fromB && eqLocatedHsExpr toA toB
    (FromThenTo fromA thenA toA, FromThenTo fromB thenB toB) ->
      eqLocatedHsExpr fromA fromB && eqLocatedHsExpr thenA thenB && eqLocatedHsExpr toA toB
    _ -> False

eqHsQuote :: HsQuote GhcPs -> HsQuote GhcPs -> Bool
eqHsQuote left right =
  case (left, right) of
    (ExpBr _ exprA, ExpBr _ exprB) -> eqLocatedHsExpr exprA exprB
    (PatBr _ patA, PatBr _ patB) -> sameStructural patA patB
    (DecBrL _ declsA, DecBrL _ declsB) -> sameStructural declsA declsB
    (DecBrG _ groupA, DecBrG _ groupB) -> sameStructural groupA groupB
    (TypBr _ tyA, TypBr _ tyB) -> sameStructural tyA tyB
    (VarBr _ namespaceA nameA, VarBr _ namespaceB nameB) -> namespaceA == namespaceB && sameStructural nameA nameB
    _ -> False

eqHsTypedSplice :: HsTypedSplice GhcPs -> HsTypedSplice GhcPs -> Bool
eqHsTypedSplice left right =
  case (left, right) of
    (HsTypedSpliceExpr _ exprA, HsTypedSpliceExpr _ exprB) -> eqLocatedHsExpr exprA exprB

eqHsUntypedSplice :: HsUntypedSplice GhcPs -> HsUntypedSplice GhcPs -> Bool
eqHsUntypedSplice left right =
  case (left, right) of
    (HsUntypedSpliceExpr _ exprA, HsUntypedSpliceExpr _ exprB) -> eqLocatedHsExpr exprA exprB
    (HsQuasiQuote _ quoterA bodyA, HsQuasiQuote _ quoterB bodyB) -> sameStructural quoterA quoterB && sameStructural bodyA bodyB
    _ -> False

eqList :: (a -> b -> Bool) -> [a] -> [b] -> Bool
eqList eq left right =
  length left == length right && and (zipWith eq left right)

eqNonEmpty :: (a -> b -> Bool) -> NE.NonEmpty a -> NE.NonEmpty b -> Bool
eqNonEmpty eq left right =
  eqList eq (NE.toList left) (NE.toList right)

sameConstructor :: (Data a, Data b) => a -> b -> Bool
sameConstructor left right =
  showConstr (toConstr (normalizeGhcAst left)) == showConstr (toConstr (normalizeGhcAst right))

data SomeData = forall a. (Data a) => SomeData a

sameStructural :: (Data a, Data b) => a -> b -> Bool
sameStructural left right =
  structuralEq (normalizeGhcAst left) (normalizeGhcAst right)

structuralEq :: (Data a, Data b) => a -> b -> Bool
structuralEq left right =
  primitiveEq left right
    || emptyLocalBindsEq left right
    || stmtEq left right
    || (isIgnoredAnnotationPayload left && isIgnoredAnnotationPayload right)
    || ( sameCon
           && case (conName, leftChildren, rightChildren) of
             ("L", [_, leftPayload], [_, rightPayload]) -> structuralEqSome leftPayload rightPayload
             _ -> eqList structuralEqSome leftChildren rightChildren
       )
  where
    conName = showConstr (toConstr left)
    sameCon = conName == showConstr (toConstr right)
    leftChildren = gmapQ SomeData left
    rightChildren = gmapQ SomeData right

emptyLocalBindsEq :: (Data a, Data b) => a -> b -> Bool
emptyLocalBindsEq left right =
  case (cast left, cast right) of
    (Just (left' :: HsLocalBindsLR GhcPs GhcPs), Just (right' :: HsLocalBindsLR GhcPs GhcPs)) ->
      emptyLocalBindsLike left' && emptyLocalBindsLike right'
    _ -> False

stmtEq :: (Data a, Data b) => a -> b -> Bool
stmtEq left right =
  exprStmtEq || cmdStmtEq
  where
    exprStmtEq =
      case (cast left, cast right) of
        (Just (left' :: StmtLR GhcPs GhcPs (LHsExpr GhcPs)), Just (right' :: StmtLR GhcPs GhcPs (LHsExpr GhcPs))) ->
          eqStmtBodies eqLocatedHsExpr left' right'
        _ -> False
    cmdStmtEq =
      case (cast left, cast right) of
        (Just (left' :: StmtLR GhcPs GhcPs (LHsCmd GhcPs)), Just (right' :: StmtLR GhcPs GhcPs (LHsCmd GhcPs))) ->
          eqStmtBodies eqLocatedHsCmd left' right'
        _ -> False

eqStmtBodies :: (body -> body -> Bool) -> StmtLR GhcPs GhcPs body -> StmtLR GhcPs GhcPs body -> Bool
eqStmtBodies eqBody left right =
  case (left, right) of
    (LastStmt _ bodyA _ _, LastStmt _ bodyB _ _) -> eqBody bodyA bodyB
    (LastStmt _ bodyA _ _, BodyStmt _ bodyB _ _) -> eqBody bodyA bodyB
    (BodyStmt _ bodyA _ _, LastStmt _ bodyB _ _) -> eqBody bodyA bodyB
    (BodyStmt _ bodyA _ _, BodyStmt _ bodyB _ _) -> eqBody bodyA bodyB
    _ -> False

emptyLocalBindsLike :: HsLocalBindsLR GhcPs GhcPs -> Bool
emptyLocalBindsLike binds =
  case binds of
    EmptyLocalBinds _ -> True
    HsValBinds _ (ValBinds _ [] []) -> True
    _ -> False

isIgnoredAnnotationPayload :: (Data a) => a -> Bool
isIgnoredAnnotationPayload value =
  case (showConstr (toConstr value), gmapQ SomeData value) of
    (name, _) | isIgnoredAnnotationCon name -> True
    ("Nothing", []) -> True
    ("Just", [payload]) -> isIgnoredAnnotationSome payload
    ("[]", []) -> True
    (":", [headPayload, tailPayload]) -> isIgnoredAnnotationSome headPayload && isIgnoredAnnotationSome tailPayload
    ("(,)", payloads) -> all isIgnoredAnnotationSome payloads
    _ -> False

isIgnoredAnnotationCon :: String -> Bool
isIgnoredAnnotationCon name =
  any (`isPrefixOf` name) ["Ann", "Ep", "Epa", "NameAnn", "NoEp"]
    || name
      `elem` [ "Anchor",
               "NameSquare",
               "NameParens",
               "NameBackquotes",
               "NoComments",
               "NormalSyntax",
               "UnicodeSyntax",
               "NoSourceText",
               "SourceText"
             ]

structuralEqSome :: SomeData -> SomeData -> Bool
structuralEqSome (SomeData left) (SomeData right) =
  structuralEq left right

isIgnoredAnnotationSome :: SomeData -> Bool
isIgnoredAnnotationSome (SomeData value) =
  isIgnoredAnnotationPayload value

primitiveEq :: (Data a, Data b) => a -> b -> Bool
primitiveEq left right =
  primRdrName
    || prim (Proxy @Bool)
    || prim (Proxy @Char)
    || prim (Proxy @Int)
    || prim (Proxy @Integer)
    || prim (Proxy @Rational)
    || prim (Proxy @String)
    || prim (Proxy @Text)
    || prim (Proxy @ByteString)
    || prim (Proxy @FastString)
    || prim (Proxy @FieldLabelString)
  where
    primRdrName =
      case (cast left, cast right) of
        (Just (left' :: RdrName), Just (right' :: RdrName)) -> rdrNameOcc left' == rdrNameOcc right'
        _ -> False

    prim :: forall c. (Eq c, Data c) => Proxy c -> Bool
    prim _ =
      case (cast left, cast right) of
        (Just (left' :: c), Just (right' :: c)) -> left' == right'
        _ -> False

comparisonDump :: (Data a) => a -> String
comparisonDump = unlines . dumpLines 0 . normalizeGhcAst

dumpLines :: (Data a) => Int -> a -> [String]
dumpLines depth value =
  let conName = showConstr (toConstr value)
      children = gmapQ SomeData value
   in if isIgnoredDumpPayload value
        then []
        else case (primitiveValue value, conName, children) of
          (Just leaf, _, _) -> [indent depth <> leaf]
          (_, "L", [_, payload]) -> dumpSome depth payload
          _ ->
            let childLines = concatMap (dumpSome (depth + 1)) children
             in if null childLines
                  then [indent depth <> conName]
                  else (indent depth <> conName) : childLines

dumpSome :: Int -> SomeData -> [String]
dumpSome depth (SomeData value) =
  dumpLines depth value

isIgnoredDumpPayload :: (Data a) => a -> Bool
isIgnoredDumpPayload value =
  isIgnoredAnnotationPayload value
    || showConstr (toConstr value)
      `elem` [ "NoExtField",
               "NoExtCon",
               "NoAnnSortKey"
             ]

primitiveValue :: (Data a) => a -> Maybe String
primitiveValue value =
  prim (Proxy @Bool)
    <> prim (Proxy @Char)
    <> prim (Proxy @Int)
    <> prim (Proxy @Integer)
    <> prim (Proxy @Rational)
    <> prim (Proxy @String)
    <> prim (Proxy @Text)
    <> prim (Proxy @ByteString)
    <> prim (Proxy @FastString)
    <> primRdrName
  where
    primRdrName =
      case cast value of
        Just (value' :: RdrName) -> Just (renderGhc (rdrNameOcc value'))
        Nothing -> Nothing

    prim :: forall c. (Show c, Data c) => Proxy c -> Maybe String
    prim _ =
      case cast value of
        Just (value' :: c) -> Just (show value')
        Nothing -> Nothing

indent :: Int -> String
indent depth = replicate (depth * 2) ' '

renderGhc :: (Outputable a) => a -> String
renderGhc = showSDocUnsafe . ppr
