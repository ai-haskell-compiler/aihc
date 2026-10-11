{-# LANGUAGE OverloadedStrings #-}

module Test.Fc.Properties
  ( fcPropertyTests,
  )
where

import Aihc.Fc
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, annotate, failure, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

fcPropertyTests :: TestTree
fcPropertyTests =
  testGroup
    "SystemFC properties"
    [ testProperty "parseProgram . renderProgram = id" prop_programRoundTrip,
      testProperty "tidyProgram is idempotent" prop_tidyIdempotent,
      testProperty "tidyProgram output round trips" prop_tidyRoundTrip,
      testProperty "t prefix stores Bool" prop_prefixStrip,
      testProperty "uses have no type suffix" prop_noUseTypes,
      testProperty "readable style is short" prop_readableStyle
    ]

prop_readableStyle :: Property
prop_readableStyle = withTests 1 $ property $ do
  let printed = renderProgramWith RenderReadable readableProgram
  annotate (T.unpack printed)
  T.lines printed === readableExpected

-- | The readable text of 'readableProgram'. Only the used scopes show, the
-- literals and the unboxed tuples are short, the arrows have no
-- representations, the representation synonym folds, and the unused
-- pattern binders show as @_@.
readableExpected :: [Text]
readableExpected =
  [ "scope 1 = \"\" Test",
    "scope 2 = \"aihc-prim\" GHC.Types",
    "scope 4 = \"aihc-prim\" GHC.Prim",
    "",
    "pub type 2.sLiftedRep :: 2.tRuntimeRep =",
    "  2.cBoxedRep 2.cLifted",
    "",
    "pub type 2.tTuple2# (r1 : 2.tRuntimeRep) (r2 : 2.tRuntimeRep) (a1 : 2.tTYPE r1) (a2 : 2.tTYPE r2) :: 2.tTYPE (2.cTupleRep '[r1, r2]) {",
    "  pub unboxed-tuple 2.cTuple2# :: ∀(r1 : 2.tRuntimeRep) (r2 : 2.tRuntimeRep) (a1 : 2.tTYPE r1) (a2 : 2.tTYPE r2). a1 → a2 → (# a1, a2 #)",
    "}",
    "",
    "val 1.vpair :: 4.tInt# → 4.tInt# → (# 4.tInt#, 4.tInt# #)",
    "  = λ(x : 4.tInt#) (_ : 4.tInt#).",
    "    (# 1#, x #)",
    "",
    "val 1.vsecond :: (# 4.tInt#, 4.tInt# #) → 4.tInt#",
    "  = λ(p : (# 4.tInt#, 4.tInt# #)).",
    "    let! (# _, b : 4.tInt# #) = p;",
    "    case b of {",
    "      -1# →",
    "        3#Word8;",
    "      _ →",
    "        2##",
    "    }",
    "",
    "val 1.vgreeting :: 2.t[] 2.tChar",
    "  = \"hi\\n\""
  ]

-- | A program that uses each short form of the readable style.
readableProgram :: Program
readableProgram =
  Program
    { programScopes = insertScope 4 primPackage "GHC.Prim" (insertScope 3 testPackage "Unused" (insertScope 5 primPackage "GHC.CString" scopes)),
      programImports = Imports mempty mempty mempty mempty mempty mempty mempty,
      programDecls =
        [ DeclSynonym (SynonymDecl Pub liftedRep [] (TyCon (wiredType "RuntimeRep")) boxedLifted),
          DeclType
            TypeDecl
              { typeVis = Pub,
                typeName = tupleType,
                typeBinders = tupleBinders,
                typeResult = TyApp (TyCon (wiredType "TYPE")) (TyApp (TyCon (wiredCon "TupleRep")) (promoted [TyVar r1, TyVar r2])),
                typeRoles = [],
                typeCons = [ConDecl Pub tupleCon (foldr (TyForAll . uncurry Binder) (lifted (TyVar a1) (TyFun (TyVar r2) (TyApp (TyCon (wiredCon "TupleRep")) (promoted [TyVar r1, TyVar r2])) (TyVar a2) (tuple (TyVar r1) (TyVar r2) (TyVar a1) (TyVar a2)))) [(name, binderType binder) | binder@(Binder name _) <- tupleBinders]) UnboxedTupleConstructor []]
              },
          DeclVal
            ValDecl
              { valVis = Private,
                valInline = InlineDefault,
                valName = valueNameTop "pair",
                valType = lifted intType (lifted intType intPair),
                valBody =
                  ExLam
                    (Binder (localValue "x") intType)
                    ( ExLam
                        (Binder (localValueWith 3 "_pat") intType)
                        (ExApp (ExApp (foldl ExTyApp (ExVar tupleCon) [intRep, intRep, intType, intType]) (intLiteral 1)) (ExVar (localValue "x")))
                    )
              },
          DeclVal
            ValDecl
              { valVis = Private,
                valInline = InlineDefault,
                valName = valueNameTop "second",
                valType = lifted intPair intType,
                valBody =
                  ExLam
                    (Binder (localValue "p") intPair)
                    ( ExCase
                        (ExVar (localValue "p"))
                        (Just (Binder (localValue "_scrut") intPair))
                        ( Alt (AltData tupleCon) [] [Binder (localValueWith 9 "_pat") intType, Binder (localValue "b") intType] (ExCase (ExVar (localValue "b")) Nothing (Alt (AltLit (LitInt intRepCon (-1))) [] [] (ExLit (LitInt (TyCon (wiredCon "Word8Rep")) 3) (TyCon (primType "Word8#"))) NE.:| [Alt AltDefault [] [] (ExLit (LitInt (TyCon (wiredCon "WordRep")) 2) (TyCon (primType "Word#")))]))
                            NE.:| []
                        )
                    )
              },
          DeclVal
            ValDecl
              { valVis = Private,
                valInline = InlineDefault,
                valName = valueNameTop "greeting",
                valType = TyApp (TyCon (wiredType "[]")) (TyCon (wiredType "Char")),
                valBody = ExApp (ExApp (ExVar (Name "unpackLatin1From" SortValue (OriginTop primPackage "GHC.CString"))) (ExLit (LitAddr (TyCon (wiredCon "AddrRep")) "hi\n") (TyCon (primType "Addr#")))) (intLiteral 0)
              }
        ]
    }
  where
    liftedRep = Name "LiftedRep" SortSynonym (OriginTop primPackage "GHC.Types")
    boxedLifted = TyApp (TyCon (wiredCon "BoxedRep")) (TyCon (wiredCon "Lifted"))
    lifted = TyFun boxedLifted boxedLifted
    tupleType = wiredType "Tuple2#"
    tupleCon = wiredCon "Tuple2#"
    r1 = localType "r1"
    r2 = localType "r2"
    a1 = localType "a1"
    a2 = localType "a2"
    tupleBinders =
      [ Binder r1 (TyCon (wiredType "RuntimeRep")),
        Binder r2 (TyCon (wiredType "RuntimeRep")),
        Binder a1 (TyApp (TyCon (wiredType "TYPE")) (TyVar r1)),
        Binder a2 (TyApp (TyCon (wiredType "TYPE")) (TyVar r2))
      ]
    tuple w x y z = foldl TyApp (TyCon tupleType) [w, x, y, z]
    promoted = foldr (\element rest -> foldl TyApp (TyCon (wiredCon ":")) [TyCon (wiredType "RuntimeRep"), element, rest]) (TyApp (TyCon (wiredCon "[]")) (TyCon (wiredType "RuntimeRep")))
    intRepCon = TyCon (wiredCon "IntRep")
    intRep = intRepCon
    intType = TyCon (primType "Int#")
    intPair = tuple intRep intRep intType intType
    intLiteral value = ExLit (LitInt intRepCon value) intType
    wiredType text = Name text SortTypeConstructor (OriginTop primPackage "GHC.Types")
    wiredCon text = Name text SortDataConstructor (OriginTop primPackage "GHC.Types")
    primType text = Name text SortTypeConstructor (OriginTop primPackage "GHC.Prim")

prop_programRoundTrip :: Property
prop_programRoundTrip = property $ do
  program <- forAll genProgram
  let printed = renderProgram program
  annotate (T.unpack printed)
  case parseProgram printed of
    Left parseError -> do
      annotate (renderParseError parseError)
      failure
    Right parsed -> parsed === program

prop_tidyIdempotent :: Property
prop_tidyIdempotent = property $ do
  program <- forAll genTidyProgram
  tidyProgram (tidyProgram program) === tidyProgram program

prop_tidyRoundTrip :: Property
prop_tidyRoundTrip = property $ do
  program <- tidyProgram <$> forAll genTidyProgram
  let printed = renderProgram program
  annotate (T.unpack printed)
  case parseProgram printed of
    Left parseError -> do
      annotate (renderParseError parseError)
      failure
    Right parsed -> parsed === program

prop_prefixStrip :: Property
prop_prefixStrip = property $ do
  let printed = renderProgram boolProgram
  annotate (T.unpack printed)
  T.isInfixOf "1.tBool" printed === True
  nameText (typeName (boolDecl boolProgram)) === "Bool"

prop_noUseTypes :: Property
prop_noUseTypes = property $ do
  let printed = renderProgram boolProgram
  annotate (T.unpack printed)
  T.isInfixOf " : tycon" printed === False
  T.isInfixOf ") : " printed === False

boolDecl :: Program -> TypeDecl
boolDecl program =
  case programDecls program of
    DeclType declaration : _ -> declaration
    _ -> error "expected Bool type"

boolProgram :: Program
boolProgram = identityProgram

primPackage :: PackageId
primPackage = PackageId "aihc-prim"

testPackage :: PackageId
testPackage = PackageId ""

scopes :: ScopeTable
scopes =
  insertScope 2 primPackage "GHC.Types" (insertScope 1 testPackage "Test" emptyScopeTable)

typeNameTop :: Text -> Name
typeNameTop text = Name text SortTypeConstructor (OriginTop testPackage "Test")

valueNameTop :: Text -> Name
valueNameTop text = Name text SortValue (OriginTop testPackage "Test")

dataNameTop :: Text -> Name
dataNameTop text = Name text SortDataConstructor (OriginTop testPackage "Test")

synonymNameTop :: Text -> Name
synonymNameTop text = Name text SortSynonym (OriginTop testPackage "Test")

axiomNameTop :: Text -> Name
axiomNameTop text = Name text SortAxiom (OriginTop testPackage "Test")

typeWired :: Text -> Name
typeWired text = Name text SortSynonym (OriginTop primPackage "GHC.Types")

localType :: Text -> Name
localType text = Name text SortTypeVariable (OriginLocal (Unique 0))

localValue :: Text -> Name
localValue text = Name text SortValue (OriginLocal (Unique 0))

localTypeWith :: Int -> Text -> Name
localTypeWith unique text = Name text SortTypeVariable (OriginLocal (Unique unique))

localValueWith :: Int -> Text -> Name
localValueWith unique text = Name text SortValue (OriginLocal (Unique unique))

identityProgram :: Program
identityProgram =
  Program
    { programScopes = scopes,
      programImports = Imports mempty mempty mempty mempty mempty mempty mempty,
      programDecls =
        [ DeclType
            TypeDecl
              { typeVis = Pub,
                typeName = typeNameTop "Bool",
                typeBinders = [],
                typeResult = TyCon (typeWired "Type"),
                typeRoles = [],
                typeCons =
                  [ ConDecl Pub (dataNameTop "False") (TyCon (typeNameTop "Bool")) HeapConstructor [],
                    ConDecl Pub (dataNameTop "True") (TyCon (typeNameTop "Bool")) HeapConstructor []
                  ]
              },
          DeclVal
            ValDecl
              { valVis = Private,
                valInline = InlineDefault,
                valName = valueNameTop "id",
                valType =
                  TyForAll
                    (Binder (localType "a") (TyCon (typeWired "Type")))
                    ( TyFun
                        (TyCon (typeWired "LiftedRep"))
                        (TyCon (typeWired "LiftedRep"))
                        (TyVar (localType "a"))
                        (TyVar (localType "a"))
                    ),
                valBody =
                  ExTyLam
                    (Binder (localType "a") (TyCon (typeWired "Type")))
                    ( ExLam
                        (Binder (localValue "x") (TyVar (localType "a")))
                        (ExVar (localValue "x"))
                    )
              }
        ]
    }

genProgram :: Gen Program
genProgram = Program scopes <$> genImports <*> Gen.list (Range.linear 0 10) genDecl

genImports :: Gen Imports
genImports = do
  headers <- genMap genHeaderName genType
  synonyms <- genMap (synonymNameTop . ("ImportedS" <>) <$> genSuffix) genType
  axioms <- Map.fromList <$> Gen.list (Range.linear 0 5) genImportedAxiom
  binders <- genMap genLocalName genType
  strictFields <- Map.fromList <$> (traverse (\name -> (,) name <$> genStrictFields) =<< Gen.subsequence (Map.keys headers))
  dataCons <- Map.fromList <$> (traverse (\name -> (,) name <$> Gen.list (Range.linear 0 3) (dataNameTop . ("ImportedC" <>) <$> genSuffix)) =<< Gen.subsequence [name | name <- Map.keys headers, nameSort name == SortTypeConstructor])
  pure (Imports headers synonyms axioms binders mempty (Map.filter (not . null) strictFields) dataCons)
  where
    genMap makeName makeValue = Map.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> makeName <*> makeValue)
    genHeaderName = do
      suffix <- genSuffix
      Gen.element
        [ typeNameTop ("ImportedT" <> suffix),
          dataNameTop ("ImportedC" <> suffix),
          synonymNameTop ("ImportedS" <> suffix),
          valueNameTop ("importedV" <> suffix)
        ]
    genLocalName = Gen.choice [genLocalTypeName, genLocalValueName]
    genImportedAxiom = do
      name <- axiomNameTop . ("importedAxiom" <>) <$> genSuffix
      declaration <- AxiomDecl Private name <$> Gen.list (Range.linear 0 3) genTypeBinder <*> genRole <*> genType <*> genType
      pure (name, declaration)

genDecl :: Gen Decl
genDecl =
  Gen.choice
    [ DeclType <$> genTypeDecl,
      DeclSynonym <$> genSynonymDecl,
      DeclAxiom <$> genAxiomDecl,
      DeclVal <$> genValDecl,
      DeclRule <$> genRuleDecl
    ]

genTypeDecl :: Gen TypeDecl
genTypeDecl = do
  name <- typeNameTop . ("T" <>) <$> genSuffix
  binders <- Gen.list (Range.linear 0 3) genTypeBinder
  roles <- traverse (const genRole) binders
  TypeDecl
    <$> genVis
    <*> pure name
    <*> pure binders
    <*> genType
    <*> pure roles
    <*> Gen.list (Range.linear 0 3) (genConDecl name)

genConDecl :: Name -> Gen ConDecl
genConDecl typeName =
  ConDecl
    <$> genVis
    <*> (dataNameTop . ("C" <>) <$> genSuffix)
    <*> Gen.choice [pure (TyCon typeName), genType]
    <*> pure HeapConstructor
    <*> genStrictFields

genStrictFields :: Gen [Int]
genStrictFields = Set.toAscList . Set.fromList <$> Gen.list (Range.linear 0 3) (Gen.int (Range.linear 0 5))

genSynonymDecl :: Gen SynonymDecl
genSynonymDecl =
  SynonymDecl
    <$> genVis
    <*> (synonymNameTop . ("S" <>) <$> genSuffix)
    <*> Gen.list (Range.linear 0 3) genTypeBinder
    <*> genType
    <*> genType

genAxiomDecl :: Gen AxiomDecl
genAxiomDecl =
  AxiomDecl
    <$> genVis
    <*> (axiomNameTop . ("axiom" <>) <$> genSuffix)
    <*> Gen.list (Range.linear 0 3) genTypeBinder
    <*> genRole
    <*> genType
    <*> genType

genValDecl :: Gen ValDecl
genValDecl =
  ValDecl
    <$> genVis
    <*> (valueNameTop . ("f" <>) <$> genSuffix)
    <*> genType
    <*> Gen.choice [ExVar <$> genLocalValueName, genForeignCallExpr]
    <*> genInlineSpec

genInlineSpec :: Gen InlineSpec
genInlineSpec =
  Gen.choice
    [ pure InlineDefault,
      InlineAlways <$> genActivation,
      InlineWhenUseful <$> genActivation,
      -- A NOINLINE that allows every phase is what no pragma says, and the
      -- text form leaves the activation of a plain noinline unsaid.
      InlineNever <$> Gen.filter (/= AlwaysActive) genActivation
    ]

genActivation :: Gen RuleActivation
genActivation = Gen.choice [pure AlwaysActive, ActiveAfter <$> phase, ActiveBefore <$> phase, pure NeverActive]
  where
    phase = Gen.int (Range.linear 0 3)

genRuleDecl :: Gen RuleDecl
genRuleDecl =
  RuleDecl . ("rule" <>)
    <$> genSuffix
    <*> genActivation
    <*> Gen.list (Range.linear 0 2) genTypeBinder
    <*> Gen.list (Range.linear 0 2) (Binder <$> genLocalValueName <*> genType)
    <*> genType
    <*> (ExVar <$> genLocalValueName)
    <*> (ExVar <$> genLocalValueName)

genForeignCallExpr :: Gen Expr
genForeignCallExpr =
  ExForeignCall
    <$> genForeignCall
    <*> Gen.list (Range.linear 0 2) genType
    <*> Gen.list (Range.linear 0 3) (ExVar <$> genLocalValueName)

genForeignCall :: Gen ForeignCall
genForeignCall =
  (ForeignCall . valueNameTop . ("foreign" <>) <$> genSuffix)
    <*> genCallingConvention
    <*> Gen.list (Range.linear 0 4) genForeignImportDependency
    <*> genType

genForeignImportDependency :: Gen ForeignImportDependency
genForeignImportDependency =
  Gen.choice
    [ ForeignAxiom . axiomNameTop . ("foreignAxiom" <>) <$> genSuffix,
      ForeignConstructor . dataNameTop . ("ForeignConstructor" <>) <$> genSuffix
    ]

genCallingConvention :: Gen CallingConvention
genCallingConvention =
  Gen.choice
    [ pure Prim,
      CCall <$> genCCallSpec
    ]

genCCallSpec :: Gen CCallSpec
genCCallSpec =
  CCallSpec
    <$> genForeignSymbol
    <*> genCCallTarget
    <*> genForeignSafety
    <*> Gen.list (Range.linear 0 6) genCAbiType
    <*> genCAbiType
    <*> genForeignEffect

genCCallTarget :: Gen CCallTarget
genCCallTarget = Gen.element [CCallFunction, CCallAddress, CCallDynamic, CCallWrapper]

genForeignSymbol :: Gen Text
genForeignSymbol =
  Gen.text
    (Range.linear 0 16)
    ( Gen.frequency
        [ (12, Gen.alphaNum),
          (1, Gen.element ['_', '.', '-', ' ', '\'', '"', '\\', '\n'])
        ]
    )

genCAbiType :: Gen CAbiType
genCAbiType = Gen.element [minBound .. maxBound]

genForeignSafety :: Gen ForeignSafety
genForeignSafety = Gen.element [ForeignUnsafe, ForeignSafe, ForeignInterruptible]

genForeignEffect :: Gen ForeignEffect
genForeignEffect = Gen.element [ForeignPure, ForeignRealWorld]

genType :: Gen Type
genType =
  Gen.recursive
    Gen.choice
    [ TyVar <$> genLocalTypeName,
      TyCon <$> genTypeName,
      TyLit <$> genTypeName <*> genTyLit
    ]
    [ TyApp <$> genType <*> genType,
      TyFun <$> genType <*> genType <*> genType <*> genType,
      TyForAll <$> genTypeBinder <*> genType,
      TyEq <$> genType <*> genType
    ]

genTyLit :: Gen TyLit
genTyLit =
  Gen.choice
    [ TyLitNat . fromIntegral <$> Gen.int (Range.linear 0 100000),
      TyLitSymbol <$> Gen.text (Range.linear 0 8) Gen.unicode,
      TyLitChar <$> Gen.unicode
    ]

genTypeName :: Gen Name
genTypeName =
  Gen.choice
    [ pure (typeWired "Type"),
      pure (typeWired "LiftedRep"),
      pure (typeWired "UnliftedRep"),
      typeNameTop . ("T" <>) <$> genSuffix
    ]

genTypeBinder :: Gen Binder
genTypeBinder =
  Binder
    <$> genLocalTypeName
    <*> pure (TyCon (typeWired "Type"))

genLocalTypeName :: Gen Name
genLocalTypeName =
  localTypeWith
    <$> Gen.int (Range.linear 0 10000)
    <*> (("tv" <>) <$> genSuffix)

genLocalValueName :: Gen Name
genLocalValueName =
  localValueWith
    <$> Gen.int (Range.linear 0 10000)
    <*> (("x" <>) <$> genSuffix)

genVis :: Gen Vis
genVis = Gen.element [Pub, Private]

genRole :: Gen Role
genRole = Gen.element [Nominal, Representational, Phantom]

genSuffix :: Gen Text
genSuffix = Gen.text (Range.linear 1 4) Gen.lower

genTidyProgram :: Gen Program
genTidyProgram = do
  typeUnique <- Gen.int (Range.linear 0 10000)
  outerUnique <- Gen.int (Range.linear 0 10000)
  innerOffset <- Gen.int (Range.linear 1 10000)
  let typeVar = localTypeWith typeUnique "a"
      outer = localValueWith outerUnique "a"
      inner = localValueWith (outerUnique + innerOffset) "a"
      kind = TyCon (typeWired "Type")
      valueType = TyVar typeVar
      lifted = TyCon (typeWired "LiftedRep")
      functionType = TyFun lifted lifted valueType valueType
  pure
    Program
      { programScopes = scopes,
        programImports = Imports mempty mempty mempty mempty mempty mempty mempty,
        programDecls =
          [ DeclVal
              ValDecl
                { valVis = Pub,
                  valInline = InlineDefault,
                  valName = valueNameTop "shadow",
                  valType = TyForAll (Binder typeVar kind) functionType,
                  valBody =
                    ExTyLam
                      (Binder typeVar kind)
                      ( ExLam
                          (Binder outer valueType)
                          ( ExApp
                              (ExLam (Binder inner valueType) (ExVar inner))
                              (ExVar outer)
                          )
                      )
                }
          ]
      }
