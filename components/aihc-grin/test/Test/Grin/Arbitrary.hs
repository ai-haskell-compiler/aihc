{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Arbitrary
  ( prop_grinPrettyRoundTrip,
  )
where

import Aihc.Grin.Parser (parseProgram, renderParseError)
import Aihc.Grin.Pretty (renderProgram)
import Aihc.Grin.Syntax
import Aihc.Tc.Types
  ( Levity (..),
    RuntimeRep (..),
    Unique (..),
    VecCount (..),
    VecElem (..),
  )
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

prop_grinPrettyRoundTrip :: QC.Property
prop_grinPrettyRoundTrip =
  QC.forAllShrink genGrinProgram (const []) $ \program ->
    let rendered = T.pack (renderProgram program)
     in case parseProgram rendered of
          Left err ->
            QC.counterexample
              ("failed to parse generated GRIN:\n" <> T.unpack rendered <> "\n\n" <> renderParseError err)
              False
          Right reparsed ->
            -- GrinVar's semantic Eq instance intentionally ignores runtime
            -- representations, so compare the complete derived structure.
            QC.counterexample
              ("rendered GRIN:\n" <> T.unpack rendered)
              (show reparsed QC.=== show program)

genGrinProgram :: QC.Gen GrinProgram
genGrinProgram =
  QC.sized $ \size ->
    QC.resize (max 1 (size `div` 4)) $
      GrinProgram
        <$> smallList ((,) <$> genText <*> smallList (smallList genRuntimeRep))
        <*> smallList ((,) <$> genVar <*> QC.arbitrary)
        <*> smallList genForeignCall
        <*> smallList genText
        <*> smallList genCodeInfo
        <*> smallList ((,) <$> genVar <*> genNode)
        <*> smallList ((,) <$> genVar <*> genNode)
        <*> smallList genFunction

genCodeInfo :: QC.Gen GrinCodeInfo
genCodeInfo =
  GrinCodeInfo
    <$> genText
    <*> genFunctionName
    <*> smallList (smallList genRuntimeRep)
    <*> genRuntimeRep

genFunction :: QC.Gen GrinFunction
genFunction =
  GrinFunction
    <$> genFunctionName
    <*> QC.oneof [pure Nothing, Just <$> genText]
    <*> smallList genVar
    <*> genRuntimeRep
    <*> genExpr

genExpr :: QC.Gen GrinExpr
genExpr = QC.sized genExprSized

genExprSized :: Int -> QC.Gen GrinExpr
genExprSized size =
  QC.oneof (atomicExpressions <> recursiveExpressions)
  where
    smaller = QC.resize (size `div` 2) (genExprSized (size `div` 2))
    atomicExpressions =
      [ GrinConstant <$> smallList genValue,
        GrinStore <$> genNode,
        GrinEnsureHeap <$> QC.arbitrary <*> smallList genValue,
        GrinStoreUnchecked <$> genNode,
        GrinFetch <$> genRuntimeRep <*> genValue,
        GrinUpdate <$> genValue <*> genValue,
        GrinEval <$> genRuntimeRep <*> genValue,
        GrinCpsEval <$> genRuntimeRep <*> genValue <*> genValue <*> genValue,
        GrinCall <$> genRuntimeRep <*> genFunctionName <*> smallList genValue,
        GrinPrimitiveCall <$> genRuntimeRep <*> genText <*> smallList genValue,
        GrinCpsPrimitiveCall <$> genRuntimeRep <*> genText <*> smallList genValue <*> genValue,
        GrinApply <$> genRuntimeRep <*> genValue <*> smallList genValue,
        GrinCpsApply <$> genRuntimeRep <*> genValue <*> smallList genValue <*> genValue,
        GrinContinue <$> genValue <*> smallList genValue,
        GrinUpdateBlackhole <$> genValue <*> genValue,
        GrinHalt <$> smallList genValue,
        GrinThrow <$> genValue,
        GrinCatch <$> genRuntimeRep <*> genValue <*> genValue <*> smallList genValue,
        GrinForeignCallExpr <$> genForeignCall <*> smallList genValue
      ]
    recursiveExpressions
      | size <= 0 = []
      | otherwise =
          [ GrinBind <$> smallList genVar <*> smaller <*> smaller,
            GrinStoreRec <$> smallList ((,) <$> genVar <*> genNode) <*> smaller,
            GrinStoreRecUnchecked <$> smallList ((,) <$> genVar <*> genNode) <*> smaller,
            GrinCase <$> genValue <*> genVar <*> smallList (genAlt smaller)
          ]

genAlt :: QC.Gen GrinExpr -> QC.Gen GrinAlt
genAlt rhs = GrinAlt <$> genAltCon <*> smallList genVar <*> rhs

genAltCon :: QC.Gen GrinAltCon
genAltCon =
  QC.oneof
    [ GrinDataAlt <$> genText,
      GrinLitAlt <$> genLiteral,
      pure GrinDefaultAlt
    ]

genValue :: QC.Gen GrinValue
genValue = QC.oneof [GrinVarValue <$> genVar, GrinLitValue <$> genLiteral]

genNode :: QC.Gen GrinNode
genNode = GrinNode <$> genNodeTag <*> smallList genValue

genNodeTag :: QC.Gen GrinNodeTag
genNodeTag =
  QC.oneof
    [ GrinConstructor <$> genText <*> QC.arbitrary,
      GrinClosure <$> genFunctionName <*> smallList (smallList genRuntimeRep),
      GrinThunk <$> genFunctionName
    ]

genLiteral :: QC.Gen GrinLiteral
genLiteral =
  QC.oneof
    [ GrinLitInt <$> genRuntimeRep <*> QC.arbitrary,
      GrinLitChar <$> genRuntimeRep <*> QC.arbitrary,
      GrinLitString <$> genText,
      GrinLitAddr . BS.pack <$> smallList QC.arbitrary
    ]

genVar :: QC.Gen GrinVar
genVar = GrinVar <$> genText <*> QC.arbitrary <*> genRuntimeRep

genFunctionName :: QC.Gen FunctionName
genFunctionName = FunctionName <$> genText

genForeignCall :: QC.Gen GrinForeignCall
genForeignCall =
  GrinForeignCall
    <$> genText
    <*> genText
    <*> (GrinForeignSignature <$> smallList genForeignType <*> genForeignType <*> genForeignEffect)

genForeignEffect :: QC.Gen GrinForeignEffect
genForeignEffect = QC.elements [GrinForeignPure, GrinForeignRealWorld]

genForeignType :: QC.Gen GrinForeignType
genForeignType = QC.elements [GrinForeignInt32, GrinForeignWord64, GrinForeignAddr]

genRuntimeRep :: QC.Gen RuntimeRep
genRuntimeRep = QC.sized genRuntimeRepSized

genRuntimeRepSized :: Int -> QC.Gen RuntimeRep
genRuntimeRepSized size =
  QC.oneof (baseRepresentations <> recursiveRepresentations)
  where
    baseRepresentations =
      [ VecRep <$> QC.elements allVecCounts <*> QC.elements allVecElems,
        BoxedRep <$> QC.elements [Lifted, Unlifted],
        RuntimeRepVar . Unique <$> QC.arbitrary,
        RuntimeRepMeta . Unique <$> QC.arbitrary,
        QC.elements
          [ IntRep,
            Int8Rep,
            Int16Rep,
            Int32Rep,
            Int64Rep,
            WordRep,
            Word8Rep,
            Word16Rep,
            Word32Rep,
            Word64Rep,
            AddrRep,
            FloatRep,
            DoubleRep
          ]
      ]
    recursiveRepresentations
      | size <= 0 = []
      | otherwise =
          [ TupleRep <$> QC.resize (size `div` 2) (smallList genRuntimeRep),
            SumRep <$> QC.resize (size `div` 2) (smallList genRuntimeRep)
          ]

allVecCounts :: [VecCount]
allVecCounts = [Vec2, Vec4, Vec8, Vec16, Vec32, Vec64]

allVecElems :: [VecElem]
allVecElems =
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

genText :: QC.Gen Text
genText = T.pack <$> QC.arbitrary

smallList :: QC.Gen value -> QC.Gen [value]
smallList generator = QC.sized $ \size -> do
  length' <- QC.chooseInt (0, min 3 size)
  QC.vectorOf length' generator
