{-# LANGUAGE OverloadedStrings #-}

module Test.Fc.Properties
  ( fcPropertyTests,
  )
where

import Aihc.Fc
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon (..),
    TyVarId (..),
    Unique (..),
    VecCount (..),
    VecElem (..),
    liftedRuntimeRep,
    mkTyCon,
    mkTyConWithOrigin,
    setTyVarKind,
  )
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isAsciiUpper)
import Data.List (nubBy)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, PropertyT, annotate, cover, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

fcPropertyTests :: TestTree
fcPropertyTests =
  testGroup
    "System FC properties"
    [ testProperty "parseProgram . renderProgram = id" prop_programRoundTrip,
      testProperty "constructor names roundtrip" prop_constructorNameRoundTrip,
      testProperty "parseExpr . renderExpr = id" prop_exprRoundTrip,
      testProperty "parseType . renderType = id" prop_typeRoundTrip,
      testProperty "type variable equality includes all fields" prop_tyVarEquality,
      testProperty "type constructor equality includes all fields" prop_tyConEquality,
      testProperty "programs print type constructor kind signatures" prop_tyConKindSignature,
      testProperty "roundtrips repeated type constructor identities" prop_repeatedTyConIdentity,
      testProperty "constructors always print full identities" prop_constructorIdentity,
      testProperty "dependent runtime representations use their binder" prop_dependentRuntimeRep,
      testProperty "package origins distinguish equal symbol names" prop_packageOrigins,
      testProperty "external signatures occur once" prop_externalSignatures,
      testProperty "non-primitive bindings do not define built-in symbols" prop_nonPrimitiveBuiltinOrigin,
      testProperty "local definition signatures serve local occurrences" prop_localSignatures,
      testProperty "undeclared external occurrences are rejected" prop_undeclaredExternal,
      testProperty "duplicate external declarations are rejected" prop_duplicateExternal
    ]

prop_programRoundTrip :: Property
prop_programRoundTrip = property $ do
  value <- forAll genProgram
  let topBinds = fcTopBinds value
  cover 2 "empty program" (null topBinds)
  cover 2 "external declaration" (any isExternal topBinds)
  cover 2 "data declaration" (any isData topBinds)
  cover 2 "axiom declaration" (any isAxiom topBinds)
  cover 2 "newtype declaration" (any isNewtype topBinds)
  cover 2 "primitive declaration" (any isPrimitive topBinds)
  cover 20 "foreign import" (any isForeignImport topBinds)
  cover 2 "value binding" (any isValueBinding topBinds)
  cover 20 "foreign call" (any topBindHasForeignCall topBinds)
  roundTrip renderProgram parseProgram value

prop_constructorNameRoundTrip :: Property
prop_constructorNameRoundTrip = property $ do
  constructorName <- forAll genConstructorName
  cover 10 "ordinary constructor" (isOrdinaryConstructorName constructorName)
  cover 10 "operator constructor" (isOperatorConstructorName constructorName)
  cover 10 "parenthesized constructor" (hasDelimiters '(' ')' constructorName)
  cover 10 "bracketed constructor" (hasDelimiters '[' ']' constructorName)
  constructorOrigin <- forAll (genConstructorFor constructorName)
  dataOrigin <- forAll (genSymbolOriginFor "T")
  moduleId <- forAll genModuleId
  let declaration = FcDataDecl dataOrigin "T" [] KType [FcDataConDecl constructorOrigin constructorName []]
  roundTrip renderProgram parseProgram (FcProgram moduleId [FcData declaration])

prop_exprRoundTrip :: Property
prop_exprRoundTrip = property $ do
  value <- forAll genExpr
  roundTrip renderExpr parseExpr value

prop_typeRoundTrip :: Property
prop_typeRoundTrip = property $ do
  value <- forAll genType
  roundTrip renderType parseType value

isExternal :: FcTopBind -> Bool
isExternal FcExternal {} = True
isExternal _ = False

isData :: FcTopBind -> Bool
isData FcData {} = True
isData _ = False

isAxiom :: FcTopBind -> Bool
isAxiom FcAxiom {} = True
isAxiom _ = False

isNewtype :: FcTopBind -> Bool
isNewtype FcNewtype {} = True
isNewtype _ = False

isPrimitive :: FcTopBind -> Bool
isPrimitive FcPrimitive {} = True
isPrimitive _ = False

isForeignImport :: FcTopBind -> Bool
isForeignImport FcForeignImport {} = True
isForeignImport _ = False

isValueBinding :: FcTopBind -> Bool
isValueBinding FcTopBind {} = True
isValueBinding _ = False

topBindHasForeignCall :: FcTopBind -> Bool
topBindHasForeignCall (FcTopBind binding) = bindHasForeignCall binding
topBindHasForeignCall _ = False

bindHasForeignCall :: FcBind -> Bool
bindHasForeignCall (FcNonRec _ expression) = exprHasForeignCall expression
bindHasForeignCall (FcRec bindings) = any (exprHasForeignCall . snd) bindings

exprHasForeignCall :: FcExpr -> Bool
exprHasForeignCall expression =
  case expression of
    FcVar {} -> False
    FcLit {} -> False
    FcApp function argument -> exprHasForeignCall function || exprHasForeignCall argument
    FcTyApp function _ -> exprHasForeignCall function
    FcLam _ body -> exprHasForeignCall body
    FcTyLam _ body -> exprHasForeignCall body
    FcLet binding body -> bindHasForeignCall binding || exprHasForeignCall body
    FcCase scrutinee _ alternatives -> exprHasForeignCall scrutinee || any (exprHasForeignCall . altRhs) alternatives
    FcCast body _ -> exprHasForeignCall body
    FcCallForeign {} -> True

prop_tyVarEquality :: Property
prop_tyVarEquality = property $ do
  let unique = Unique 1
      left = setTyVarKind KType (TyVarId "a" unique)
      right = setTyVarKind KRuntimeRep (TyVarId "b" unique)
  (left == right) === False

prop_tyConEquality :: Property
prop_tyConEquality = property $ do
  let left = mkTyCon "T" 0 KType
      right = mkTyCon "T" 0 KRuntimeRep
  (left == right) === False

prop_tyConKindSignature :: Property
prop_tyConKindSignature = property $ do
  let kind = KFun KType (KFun KType (KTYPE (BoxedRep Unlifted)))
      tyCon = mkTyConWithOrigin (PackageId "pkg") "GHC.Prim" "MutVar#" 2 kind
      ty = TcTyCon tyCon []
      program = FcProgram (FcModuleId "test" "Test") [FcExternal (FcBuiltinOrigin "value") ty]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.isInfixOf "external builtin.value : tycon \"pkg\" \"GHC.Prim\" MutVar#/2 { :: Type → Type → TYPE UnliftedRep }" rendered === True
  T.count "\n\ntycon " rendered === 0
  roundTrip renderProgram parseProgram program

prop_repeatedTyConIdentity :: Property
prop_repeatedTyConIdentity = property $ do
  let leftTyCon = mkTyConWithOrigin (PackageId "pkg") "Module" "T" 0 KType
      rightTyCon = mkTyConWithOrigin (PackageId "pkg") "Module" "T" 0 KRuntimeRep
      left = FcVar (Var "left" (Unique 1) (TcTyCon leftTyCon []))
      right = FcVar (Var "right" (Unique 2) (TcTyCon rightTyCon []))
      expression = FcApp left right
      rendered = T.pack (renderExpr expression)
  annotate (T.unpack rendered)
  T.isPrefixOf "(left{unique 1} :" rendered === True
  T.count "tycon \"pkg\" \"Module\" T/0" rendered === 2
  roundTrip renderExpr parseExpr expression

prop_constructorIdentity :: Property
prop_constructorIdentity = property $ do
  let constructorOrigin = FcConstructorId "test" "Test" "J"
      declaration = FcDataDecl (FcTopLevelOrigin "test" "Test" "A") "A" [] KType [FcDataConDecl constructorOrigin "J" []]
      program = FcProgram (FcModuleId "test" "Test") [FcData declaration]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.isInfixOf "= \"test\" Test.J" rendered === True
  roundTrip renderProgram parseProgram program

prop_dependentRuntimeRep :: Property
prop_dependentRuntimeRep = property $ do
  let rep = setTyVarKind KRuntimeRep (TyVarId "rep" (Unique 1))
      value = setTyVarKind (KTYPE (RuntimeRepVar (tvUnique rep))) (TyVarId "value" (Unique 2))
      ty = TcForAllTy rep (TcForAllTy value (TcTyVar value))
      rendered = T.pack (renderType ty)
  annotate (T.unpack rendered)
  rendered
    === "∀ (rep{unique 1} : RuntimeRep). ∀ (value{unique 2} : TYPE RuntimeRepVar 1). (value{unique 2} : TYPE RuntimeRepVar 1)"
  roundTrip renderType parseType ty

prop_packageOrigins :: Property
prop_packageOrigins = property $ do
  let ty = TcTyCon (TyCon "Identity" 0) []
      occurrence packageName =
        FcVar
          ( (Var "id" (Unique 1) ty)
              { varResolvedName = Just (FcTopLevelOrigin packageName "Module" "id")
              }
          )
      renderedA = T.pack (renderExpr (occurrence "pkgA"))
      renderedB = T.pack (renderExpr (occurrence "pkgB"))
      externalA = fcExternalVar (FcTopLevelOrigin "pkgA" "Module" "id") ty
      externalB = fcExternalVar (FcTopLevelOrigin "pkgB" "Module" "id") ty
  annotate (T.unpack renderedA)
  annotate (T.unpack renderedB)
  when (renderedA == renderedB) failure
  (varUnique externalA == varUnique externalB) === False
  case parseExpr renderedA of
    Left parseError -> annotate (show parseError) >> failure
    Right (FcVar var) -> varResolvedName var === Just (FcTopLevelOrigin "pkgA" "Module" "id")
    Right parsed -> annotate (show parsed) >> failure

prop_externalSignatures :: Property
prop_externalSignatures = property $ do
  let ty = TcForAllTy typeVariable (TcFunTy (TcTyVar typeVariable) (TcTyVar typeVariable))
      origin = FcTopLevelOrigin "pkg" "Module" "id"
      imported = (Var "id" (Unique 1) ty) {varResolvedName = Just origin}
      result = Var "result" (Unique 2) ty
      program = FcProgram (FcModuleId "test" "Test") [FcExternal origin ty, FcTopBind (FcNonRec result (FcVar imported))]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.count "Module.id :" rendered === 1
  T.count "\"pkg\" Module.id" rendered === 2
  roundTrip renderProgram parseProgram program
  where
    typeVariable = setTyVarKind (KTYPE liftedRuntimeRep) (TyVarId "a" (Unique 3))

prop_nonPrimitiveBuiltinOrigin :: Property
prop_nonPrimitiveBuiltinOrigin = property $ do
  let ty = TcTyCon (TyCon "Int" 0) []
      origin = FcBuiltinOrigin "value"
      binder = Var "local" (Unique 1) ty
      occurrence = (Var "value" (Unique 2) ty) {varResolvedName = Just origin}
      program = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcRec [(binder, FcVar occurrence)])]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.count "external builtin.value" rendered === 0
  roundTrip renderProgram parseProgram program

prop_undeclaredExternal :: Property
prop_undeclaredExternal = property $ do
  let source = "module \"test\" Test where\n\nresult : Int =\n  \"pkg\" Module.value"
  annotate (T.unpack source)
  case parseProgram source of
    Left _ -> pure ()
    Right parsed -> annotate (show parsed) >> failure

prop_localSignatures :: Property
prop_localSignatures = property $ do
  let origin = FcConstructorId "pkg" "Module" "True"
      declaration = FcDataDecl (FcTopLevelOrigin "pkg" "Module" "Bool") "Bool" [] KType [FcDataConDecl origin "True" []]
      constructor = (Var "True" (Unique 1) (fcDataResultType declaration)) {varResolvedName = Just (fcConstructorSymbolOrigin origin)}
      result = Var "result" (Unique 2) (TcTyCon (TyCon "Bool" 0) [])
      program =
        FcProgram
          (FcModuleId "pkg" "Module")
          [ FcData declaration,
            FcTopBind (FcNonRec result (FcVar constructor))
          ]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.count "external" rendered === 0
  T.count "True{unique 1}" rendered === 1
  roundTrip renderProgram parseProgram program

prop_duplicateExternal :: Property
prop_duplicateExternal = property $ do
  let source = "module \"test\" Test where\n\nexternal \"pkg\" Module.value : Int\n\nexternal \"pkg\" Module.value : Int"
  annotate (T.unpack source)
  case parseProgram source of
    Left _ -> pure ()
    Right parsed -> annotate (show parsed) >> failure

roundTrip :: (Eq a, Show a, Show error) => (a -> String) -> (Text -> Either error a) -> a -> PropertyT IO ()
roundTrip pretty parse value = do
  let rendered = T.pack (pretty value)
  annotate (T.unpack rendered)
  case parse rendered of
    Left parseError -> annotate (show parseError) >> failure
    Right actual -> actual === value

genProgram :: Gen FcProgram
genProgram = Gen.choice [genGeneralProgram, genForeignProgram]

genGeneralProgram :: Gen FcProgram
genGeneralProgram = FcProgram <$> genModuleId <*> smallList genTopBind

genForeignProgram :: Gen FcProgram
genForeignProgram = do
  moduleId <- genModuleId
  foreignCall <- genForeignCall
  binder <- genBinder
  arguments <- smallList genExpr
  pure
    ( FcProgram
        moduleId
        [ FcForeignImport foreignCall,
          FcTopBind (FcNonRec binder (FcCallForeign foreignCall arguments))
        ]
    )

genModuleId :: Gen FcModuleId
genModuleId = FcModuleId . PackageId <$> genPackageName <*> genModuleName

genTopBind :: Gen FcTopBind
genTopBind =
  Gen.choice
    [ FcExternal <$> genSymbolOrigin <*> genType,
      FcData <$> genDataDecl,
      FcAxiom <$> genAxiomDecl,
      FcNewtype <$> genNewtypeDecl,
      FcPrimitive <$> genBinder <*> genInt,
      FcForeignImport <$> genForeignCall,
      FcTopBind <$> genBindWith genExpr
    ]

genDataDecl :: Gen FcDataDecl
genDataDecl = do
  dataName <- genTypeName
  dataOrigin <- genSymbolOriginFor dataName
  FcDataDecl dataOrigin dataName <$> smallList genTyVar <*> genKind <*> smallList genDataConstructor

genDataConstructor :: Gen FcDataConDecl
genDataConstructor = do
  constructorName <- genConstructorName
  constructorOrigin <- genConstructorFor constructorName
  FcDataConDecl constructorOrigin constructorName <$> smallList genType

genAxiomDecl :: Gen FcAxiomDecl
genAxiomDecl =
  FcAxiomDecl
    <$> genTypeName
    <*> smallList genTyVar
    <*> Gen.element [FcNominal, FcRepresentational]
    <*> genType
    <*> genType

genNewtypeDecl :: Gen FcNewtypeDecl
genNewtypeDecl =
  do
    newtypeName <- genTypeName
    newtypeOrigin <- genSymbolOriginFor newtypeName
    constructorName <- genConstructorName
    constructorOrigin <- genConstructorFor constructorName
    FcNewtypeDecl
      newtypeOrigin
      newtypeName
      <$> smallList genTyVar
      <*> pure constructorOrigin
      <*> pure constructorName
      <*> genType
      <*> genType

genForeignCall :: Gen FcForeignCall
genForeignCall = FcForeignCall <$> genVarName <*> genLiteralText <*> pure (PackageId "aihc-prim") <*> genForeignSignature

genForeignSignature :: Gen FcForeignSignature
genForeignSignature =
  FcForeignSignature <$> smallList genForeignType <*> genForeignType <*> genForeignEffect

genForeignEffect :: Gen FcForeignEffect
genForeignEffect = Gen.element [FcForeignPure, FcForeignRealWorld]

genForeignType :: Gen FcForeignType
genForeignType = Gen.element [FcForeignInt, FcForeignInt32, FcForeignWord64, FcForeignAddr]

genExpr :: Gen FcExpr
genExpr =
  Gen.recursive
    Gen.choice
    [ FcVar <$> genVar,
      FcLit <$> genLiteral <*> genLiteralType
    ]
    [ FcApp <$> genExpr <*> genExpr,
      FcTyApp <$> genExpr <*> genType,
      FcLam <$> genBinder <*> genExpr,
      FcTyLam <$> genTyVar <*> genExpr,
      FcLet <$> genBindWith genExpr <*> genExpr,
      FcCase <$> genExpr <*> genBinder <*> smallList (genAltWith genExpr),
      FcCast <$> genExpr <*> genCoercion,
      FcCallForeign <$> genForeignCall <*> smallList genExpr
    ]

genBindWith :: Gen FcExpr -> Gen FcBind
genBindWith child =
  Gen.choice
    [ FcNonRec <$> genBinder <*> child,
      FcRec . nubBy sameBinder <$> smallList ((,) <$> genBinder <*> child)
    ]
  where
    sameBinder (left, _) (right, _) = varUnique left == varUnique right

genAltWith :: Gen FcExpr -> Gen FcAlt
genAltWith child = FcAlt <$> genAltCon <*> smallList genBinder <*> child

genAltCon :: Gen FcAltCon
genAltCon =
  Gen.choice
    [ do
        constructorName <- genConstructorName
        DataAlt <$> genConstructorFor constructorName,
      LitAlt <$> genLiteral <*> genLiteralType,
      pure DefaultAlt
    ]

genVar :: Gen Var
genVar = do
  identifier <- genUnique
  ty <- genType
  resolvedName <- Gen.maybe genSymbolOrigin
  let name = maybe (generatedName "v" identifier) fcOriginName resolvedName
  pure ((Var name identifier ty) {varResolvedName = resolvedName})

genBinder :: Gen Var
genBinder = do
  identifier <- genUnique
  Var (generatedName "v" identifier) identifier <$> genType

genSymbolOrigin :: Gen FcSymbolOrigin
genSymbolOrigin = genVarName >>= genSymbolOriginFor

genSymbolOriginFor :: Text -> Gen FcSymbolOrigin
genSymbolOriginFor symbolName =
  Gen.choice
    [ FcTopLevelOrigin <$> genPackageName <*> genModuleName <*> pure symbolName,
      pure (FcBuiltinOrigin symbolName)
    ]

genConstructorFor :: Text -> Gen FcConstructorId
genConstructorFor constructorName =
  FcConstructorId . PackageId <$> genPackageName <*> genModuleName <*> pure constructorName

genLiteral :: Gen Literal
genLiteral =
  Gen.choice
    [ LitInt <$> genRuntimeRep <*> genInteger,
      LitChar <$> genRuntimeRep <*> Gen.unicode,
      LitString <$> genLiteralText,
      LitAddr . BS.pack <$> smallList (Gen.word8 Range.constantBounded)
    ]

genLiteralType :: Gen TcType
genLiteralType = TcTyCon <$> genTyCon <*> pure []

genType :: Gen TcType
genType =
  Gen.recursive
    Gen.choice
    [ TcTyVar <$> genTyVar,
      TcMetaTv <$> genUnique,
      TcTyCon <$> genTyCon <*> pure [],
      TcBuiltinTyCon <$> genTypeName <*> Gen.int (Range.linear 0 4) <*> pure []
    ]
    [ TcTyCon <$> genTyCon <*> smallList genType,
      TcBuiltinTyCon <$> genTypeName <*> Gen.int (Range.linear 0 4) <*> smallList genType,
      TcFunTy <$> genType <*> genType,
      TcForAllTy <$> genTyVar <*> genType,
      TcQualTy <$> smallList genPred <*> genType,
      TcAppTy <$> genType <*> genType
    ]

genPred :: Gen Pred
genPred = Gen.choice [ClassPred <$> (TyCon <$> genTypeName <*> Gen.int (Range.linear 0 3)) <*> smallList genType, EqPred <$> genType <*> genType]

genTyVar :: Gen TyVarId
genTyVar = do
  identifier <- genUnique
  kind <- genKind
  pure (setTyVarKind kind (TyVarId (generatedName "a" identifier) identifier))

genTyCon :: Gen TyCon
genTyCon = do
  typeName <- genTypeName
  arity <- Gen.int (Range.linear 0 4)
  kind <- genKind
  Gen.choice
    [ pure (mkTyCon typeName arity kind),
      (mkTyConWithOrigin . PackageId <$> genText) <*> genTypeName <*> pure typeName <*> pure arity <*> pure kind
    ]

genKind :: Gen Kind
genKind =
  Gen.recursive
    Gen.choice
    [ KTYPE <$> genRuntimeRep,
      pure KConstraint,
      pure KRuntimeRep,
      pure KLevity,
      pure KVecCount,
      pure KVecElem,
      KMeta <$> genUnique
    ]
    [KFun <$> genKind <*> genKind]

genRuntimeRep :: Gen RuntimeRep
genRuntimeRep =
  Gen.recursive
    Gen.choice
    [ VecRep <$> genVecCount <*> genVecElem,
      BoxedRep <$> Gen.element [Lifted, Unlifted],
      pure IntRep,
      pure Int8Rep,
      pure Int16Rep,
      pure Int32Rep,
      pure Int64Rep,
      pure WordRep,
      pure Word8Rep,
      pure Word16Rep,
      pure Word32Rep,
      pure Word64Rep,
      pure AddrRep,
      pure FloatRep,
      pure DoubleRep,
      RuntimeRepVar <$> genUnique,
      RuntimeRepMeta <$> genUnique
    ]
    [ TupleRep <$> smallList genRuntimeRep,
      SumRep <$> smallList genRuntimeRep
    ]

genVecCount :: Gen VecCount
genVecCount = Gen.element [Vec2, Vec4, Vec8, Vec16, Vec32, Vec64]

genVecElem :: Gen VecElem
genVecElem =
  Gen.element
    [ Int8ElemRep,
      Int16ElemRep,
      Int32ElemRep,
      Int64ElemRep,
      Word8ElemRep,
      Word16ElemRep,
      Word32ElemRep,
      Word64ElemRep,
      FloatElemRep,
      DoubleElemRep
    ]

genCoercion :: Gen Coercion
genCoercion =
  Gen.recursive
    Gen.choice
    [ CoVar . EvVar <$> genUnique,
      Refl <$> genType,
      AxiomInstCo <$> genTypeName <*> smallList genType
    ]
    [ Sym <$> genCoercion,
      Trans <$> genCoercion <*> genCoercion,
      TyConAppCo <$> genTyCon <*> smallList genCoercion
    ]

genUnique :: Gen Unique
genUnique = Unique <$> genInt

generatedName :: Text -> Unique -> Text
generatedName prefix (Unique identifier)
  | identifier < 0 = prefix <> "_n" <> T.pack (show (abs identifier))
  | otherwise = prefix <> "_" <> T.pack (show identifier)

genInt :: Gen Int
genInt = Gen.int (Range.linear (-1000) 1000)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-100000) 100000)

genText :: Gen Text
genText = do
  first <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> "_$")
  rest <- Gen.list (Range.linear 0 11) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_$#'"))
  pure (T.pack (first : rest))

genVarName :: Gen Text
genVarName = do
  first <- Gen.element (['a' .. 'z'] <> "_$")
  rest <- Gen.list (Range.linear 0 11) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_$#'"))
  pure (T.pack (first : rest))

genTypeName :: Gen Text
genTypeName = do
  first <- Gen.element ['A' .. 'Z']
  rest <- Gen.list (Range.linear 0 11) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_$#'"))
  pure (T.pack (first : rest))

genConstructorName :: Gen Text
genConstructorName =
  Gen.choice
    [ genTypeName,
      genSymbolicConstructorName,
      genDelimitedConstructorName '(' ')',
      genDelimitedConstructorName '[' ']'
    ]

genSymbolicConstructorName :: Gen Text
genSymbolicConstructorName =
  Gen.text (Range.linear 1 12) (Gen.element operatorNameCharacters)

genDelimitedConstructorName :: Char -> Char -> Gen Text
genDelimitedConstructorName opening closing = do
  contents <- Gen.text (Range.linear 0 12) (Gen.element (delimitedNameCharacters opening closing))
  pure (T.cons opening (T.snoc contents closing))

operatorNameCharacters :: [Char]
operatorNameCharacters = "!#$%&*+./<=>?@\\^|-~:"

delimitedNameCharacters :: Char -> Char -> [Char]
delimitedNameCharacters opening closing =
  [ character
  | character <- ['!' .. '~'],
    not (isAlphaNum character),
    character /= opening,
    character /= closing
  ]

isOrdinaryConstructorName :: Text -> Bool
isOrdinaryConstructorName constructorName =
  maybe False (isAsciiUpper . fst) (T.uncons constructorName)

isOperatorConstructorName :: Text -> Bool
isOperatorConstructorName constructorName =
  not (T.null constructorName) && T.all (`elem` operatorNameCharacters) constructorName

hasDelimiters :: Char -> Char -> Text -> Bool
hasDelimiters opening closing value =
  T.length value >= 2 && T.head value == opening && T.last value == closing

genPackageName :: Gen Text
genPackageName = Gen.choice [pure "", genText]

genModuleName :: Gen Text
genModuleName = T.intercalate "." <$> Gen.list (Range.linear 1 3) genTypeName

genLiteralText :: Gen Text
genLiteralText = Gen.text (Range.linear 0 12) Gen.unicode

smallList :: Gen a -> Gen [a]
smallList = Gen.list (Range.linear 0 3)
