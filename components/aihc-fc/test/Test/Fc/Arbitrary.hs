{-# LANGUAGE OverloadedStrings #-}

module Test.Fc.Arbitrary
  ( prop_fcTextRoundTrip,
  )
where

import Aihc.Fc
import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Tc
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

prop_fcTextRoundTrip :: QC.Property
prop_fcTextRoundTrip =
  QC.forAllShrink genProgram (const []) $ \program ->
    case renderProgramChecked program of
      Left _ -> QC.discard
      Right renderedString ->
        let rendered = T.pack renderedString
         in case parseProgram rendered of
              Left parseError ->
                QC.counterexample
                  ("failed to parse generated System FC:\n" <> T.unpack rendered <> "\n\n" <> parseError)
                  False
              Right reparsed ->
                QC.counterexample
                  ("rendered System FC:\n" <> T.unpack rendered)
                  (renderProgram reparsed QC.=== T.unpack rendered)

genProgram :: QC.Gen FcProgram
genProgram = QC.sized $ \size -> FcProgram <$> smallList (genTopBind (max 1 (size `div` 3)))

genTopBind :: Int -> QC.Gen FcTopBind
genTopBind size =
  QC.oneof
    [ genData,
      FcAxiom <$> genAxiom,
      FcNewtype <$> genNewtype,
      FcPrimitive <$> genBinder <*> QC.chooseInt (0, 4),
      FcForeignImport <$> genForeignCall,
      FcTopBind <$> genBind size
    ]
  where
    genData = do
      name <- genText
      tyVars <- smallList genTyVar
      constructors <- smallList $ do
        constructor <- genText
        fields <- smallList (genType 2)
        pure (FcDataConstructor constructor (filter (`notElem` tyVars) (freeRigidTyVarsOf fields)) fields)
      pure (FcData name tyVars constructors)

genAxiom :: QC.Gen FcAxiomDecl
genAxiom =
  FcAxiomDecl
    <$> genText
    <*> smallList genTyVar
    <*> QC.elements [FcNominal, FcRepresentational]
    <*> genType 2
    <*> genType 2

genNewtype :: QC.Gen FcNewtypeDecl
genNewtype = do
  name <- genText
  tyVars <- smallList genTyVar
  constructor <- genText
  representation <- genType 2
  let result = TcTyCon (TyCon name (length tyVars)) (map TcTyVar tyVars)
  pure (FcNewtypeDecl name tyVars constructor representation result)

genBind :: Int -> QC.Gen FcBind
genBind size =
  QC.oneof
    [ FcNonRec <$> genBinder <*> genExpr (size `div` 2),
      FcRec <$> smallList ((,) <$> genBinder <*> genExpr (size `div` 3))
    ]

genExpr :: Int -> QC.Gen FcExpr
genExpr size
  | size <= 0 = QC.oneof [FcVar <$> genVar, FcLit <$> genLiteral]
  | otherwise =
      QC.frequency
        [ (4, genExpr 0),
          (2, FcApp <$> smaller <*> smaller),
          (1, FcTyApp <$> smaller <*> genType 2),
          (1, FcLam <$> genBinder <*> smaller),
          (1, FcTyLam <$> genTyVar <*> smaller),
          (1, FcLet <$> genBind (size `div` 2) <*> smaller),
          (1, FcCase <$> smaller <*> genBinder <*> smallList (genAlt (size `div` 2))),
          (1, FcCast <$> smaller <*> genCoercion 2)
        ]
  where
    smaller = genExpr (size `div` 2)

genAlt :: Int -> QC.Gen FcAlt
genAlt size = FcAlt <$> genAltCon <*> smallList genBinder <*> genExpr size

genAltCon :: QC.Gen FcAltCon
genAltCon = QC.oneof [DataAlt <$> genText, LitAlt <$> genLiteral, pure DefaultAlt]

genVar :: QC.Gen Var
genVar = do
  name <- genText
  unique <- genUnique
  ty <- genType 2
  resolved <-
    QC.frequency
      [ (3, pure Nothing),
        (1, Just . ("Package.Module." <>) <$> genText)
      ]
  pure ((Var name unique ty) {varResolvedName = resolved})

genBinder :: QC.Gen Var
genBinder = do
  variable <- genVar
  pure variable {varResolvedName = Nothing}

genType :: Int -> QC.Gen TcType
genType size
  | size <= 0 =
      QC.oneof
        [ TcTyVar <$> genTyVar,
          TcMetaTv <$> genUnique,
          (\name arity -> TcTyCon (TyCon name arity) []) <$> genText <*> QC.chooseInt (0, 3)
        ]
  | otherwise =
      QC.frequency
        [ (4, genType 0),
          (2, TcTyCon <$> genTyCon <*> smallList smaller),
          (2, TcFunTy <$> smaller <*> smaller),
          (1, TcForAllTy <$> genTyVar <*> smaller),
          (1, TcQualTy <$> smallList (genPred (size `div` 2)) <*> smaller),
          (1, TcAppTy <$> smaller <*> smaller)
        ]
  where
    smaller = genType (size `div` 2)

genPred :: Int -> QC.Gen Pred
genPred size =
  QC.oneof
    [ ClassPred <$> genText <*> smallList (genType size),
      EqPred <$> genType size <*> genType size
    ]

genTyVar :: QC.Gen TyVarId
genTyVar = TyVarId <$> genText <*> genUnique

genTyCon :: QC.Gen TyCon
genTyCon = TyCon <$> genText <*> QC.chooseInt (0, 4)

genCoercion :: Int -> QC.Gen Coercion
genCoercion size
  | size <= 0 = QC.oneof [CoVar . EvVar <$> genUnique, Refl <$> genType 1]
  | otherwise =
      QC.frequency
        [ (3, genCoercion 0),
          (1, Sym <$> smaller),
          (1, Trans <$> smaller <*> smaller),
          (1, TyConAppCo <$> genTyCon <*> smallList smaller),
          (1, AxiomInstCo <$> genText <*> smallList (genType 1))
        ]
  where
    smaller = genCoercion (size `div` 2)

genLiteral :: QC.Gen Literal
genLiteral =
  QC.oneof
    [ LitInt <$> genRuntimeRep <*> QC.arbitrary,
      LitChar <$> genRuntimeRep <*> QC.arbitrary,
      LitString <$> genText,
      LitAddr . BS.pack <$> smallList QC.arbitrary
    ]

genForeignCall :: QC.Gen FcForeignCall
genForeignCall = FcForeignCall <$> genText <*> genText <*> genForeignSignature

genForeignSignature :: QC.Gen FcForeignSignature
genForeignSignature =
  FcForeignSignature
    <$> smallList genForeignType
    <*> genForeignType
    <*> QC.elements [FcForeignPure, FcForeignRealWorld]

genForeignType :: QC.Gen FcForeignType
genForeignType = QC.elements [FcForeignInt, FcForeignInt32, FcForeignWord64, FcForeignAddr]

genRuntimeRep :: QC.Gen RuntimeRep
genRuntimeRep =
  QC.oneof
    [ pure IntRep,
      pure Int32Rep,
      pure WordRep,
      pure Word64Rep,
      pure AddrRep,
      pure FloatRep,
      pure DoubleRep,
      BoxedRep <$> QC.elements [Lifted, Unlifted],
      RuntimeRepVar <$> genUnique,
      RuntimeRepMeta <$> genUnique
    ]

genUnique :: QC.Gen Unique
-- Duplicate compiler identities make an otherwise generated program malformed.
-- Draw from the full range so independent binders remain independent.
genUnique = Unique <$> QC.chooseInt (minBound + 1, maxBound)

genText :: QC.Gen Text
genText = T.pack <$> QC.elements ["a", "xor", "length", "Data.List.value", "$dict", "(,)", "name with spaces"]

smallList :: QC.Gen a -> QC.Gen [a]
smallList generator = do
  count <- QC.chooseInt (0, 3)
  QC.vectorOf count generator
