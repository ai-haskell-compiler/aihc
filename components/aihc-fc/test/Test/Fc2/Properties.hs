{-# LANGUAGE OverloadedStrings #-}

module Test.Fc2.Properties
  ( fc2PropertyTests,
  )
where

import Aihc.Fc2
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, annotate, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

fc2PropertyTests :: TestTree
fc2PropertyTests =
  testGroup
    "SystemFC2 properties"
    [ testProperty "parseProgram . renderProgram = id" prop_programRoundTrip,
      testProperty "tidyProgram is idempotent" prop_tidyIdempotent,
      testProperty "tidyProgram output round trips" prop_tidyRoundTrip,
      testProperty "t prefix stores Bool" prop_prefixStrip,
      testProperty "uses have no type suffix" prop_noUseTypes
    ]

prop_programRoundTrip :: Property
prop_programRoundTrip = property $ do
  program <- forAll genProgram
  let printed = T.pack (renderProgram program)
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
  let printed = T.pack (renderProgram program)
  annotate (T.unpack printed)
  case parseProgram printed of
    Left parseError -> do
      annotate (renderParseError parseError)
      failure
    Right parsed -> parsed === program

prop_prefixStrip :: Property
prop_prefixStrip = property $ do
  let printed = T.pack (renderProgram boolProgram)
  annotate (T.unpack printed)
  T.isInfixOf "1.tBool" printed === True
  nameText (typeName (boolDecl boolProgram)) === "Bool"

prop_noUseTypes :: Property
prop_noUseTypes = property $ do
  let printed = T.pack (renderProgram boolProgram)
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
    { programModule = ModuleId testPackage "Test",
      programScopes = scopes,
      programDecls =
        [ DeclType
            TypeDecl
              { typeVis = Pub,
                typeName = typeNameTop "Bool",
                typeBinders = [],
                typeResult = TyCon (typeWired "Type"),
                typeRoles = [],
                typeCons =
                  [ ConDecl Pub (dataNameTop "False") (TyCon (typeNameTop "Bool")),
                    ConDecl Pub (dataNameTop "True") (TyCon (typeNameTop "Bool"))
                  ]
              },
          DeclVal
            ValDecl
              { valVis = Private,
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
genProgram = do
  suffix <- Gen.text (Range.linear 1 4) Gen.lower
  let typeName = typeNameTop ("T" <> suffix)
      consName = dataNameTop ("C" <> suffix)
      valName = valueNameTop ("f" <> suffix)
      result = TyCon (typeWired "Type")
  pure
    Program
      { programModule = ModuleId testPackage "Test",
        programScopes = scopes,
        programDecls =
          [ DeclType
              TypeDecl
                { typeVis = Pub,
                  typeName = typeName,
                  typeBinders = [],
                  typeResult = result,
                  typeRoles = [],
                  typeCons = [ConDecl Pub consName (TyCon typeName)]
                },
            DeclVal
              ValDecl
                { valVis = Pub,
                  valName = valName,
                  valType =
                    TyFun
                      (TyCon (typeWired "LiftedRep"))
                      (TyCon (typeWired "LiftedRep"))
                      (TyCon typeName)
                      (TyCon typeName),
                  valBody =
                    ExLam
                      (Binder (localValue "x") (TyCon typeName))
                      (ExVar (localValue "x"))
                }
          ]
      }

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
      { programModule = ModuleId testPackage "Test",
        programScopes = scopes,
        programDecls =
          [ DeclVal
              ValDecl
                { valVis = Pub,
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
