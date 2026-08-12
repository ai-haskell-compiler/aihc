{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Arbitrary
  ( genGrinProgram,
    prop_grinPrettyRoundTrip,
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
import Data.Word (Word8)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
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
            -- GrinVar equality ignores runtime representations. Compare derived forms to include all fields.
            QC.counterexample
              ("rendered GRIN:\n" <> T.unpack rendered)
              (show reparsed QC.=== show program)

class (Monad generator) => GrinGenerator generator where
  genChoice :: [generator value] -> generator value
  genElement :: [value] -> generator value
  genSmallList :: generator value -> generator [value]
  genInt :: generator Int
  genInteger :: generator Integer
  genChar :: generator Char
  genWord8 :: generator Word8
  genTextValue :: generator Text
  genDepth :: generator Int

instance GrinGenerator QC.Gen where
  genChoice = QC.oneof
  genElement = QC.elements
  genSmallList = QC.resize 3 . QC.listOf
  genInt = QC.arbitrary
  genInteger = QC.arbitrary
  genChar = QC.arbitrary
  genWord8 = QC.arbitrary
  genTextValue = T.pack <$> QC.arbitrary
  genDepth = QC.chooseInt (0, 3)

instance GrinGenerator Gen where
  genChoice = Gen.choice
  genElement = Gen.element
  genSmallList = Gen.list (Range.linear 0 3)
  genInt = Gen.int (Range.linear (-1000) 1000)
  genInteger = Gen.integral (Range.linear (-100000) 100000)
  genChar = Gen.unicode
  genWord8 = Gen.word8 Range.constantBounded
  genTextValue = Gen.text (Range.linear 0 12) Gen.unicode
  genDepth = Gen.int (Range.linear 0 3)

genGrinProgram :: (GrinGenerator generator) => generator GrinProgram
genGrinProgram =
  GrinProgram
    <$> genSmallList ((,) <$> genTextValue <*> genSmallList (genSmallList genRuntimeRep))
    <*> genSmallList ((,) <$> genVar <*> genInt)
    <*> genSmallList genForeignCall
    <*> genSmallList genTextValue
    <*> genSmallList genCodeInfo
    <*> genSmallList ((,) <$> genVar <*> genNode)
    <*> genSmallList ((,) <$> genVar <*> genNode)
    <*> genSmallList genFunction

genCodeInfo :: (GrinGenerator generator) => generator GrinCodeInfo
genCodeInfo =
  GrinCodeInfo
    <$> genTextValue
    <*> genFunctionName
    <*> genSmallList (genSmallList genRuntimeRep)
    <*> genRuntimeRep

genFunction :: (GrinGenerator generator) => generator GrinFunction
genFunction =
  GrinFunction
    <$> genFunctionName
    <*> genChoice [pure Nothing, Just <$> genTextValue]
    <*> genSmallList genVar
    <*> genRuntimeRep
    <*> genExpr

genExpr :: (GrinGenerator generator) => generator GrinExpr
genExpr = genDepth >>= genExprSized

genExprSized :: (GrinGenerator generator) => Int -> generator GrinExpr
genExprSized depth =
  genChoice (atomicExpressions <> recursiveExpressions)
  where
    smaller = genExprSized (depth - 1)
    atomicExpressions =
      [ GrinConstant <$> genSmallList genValue,
        GrinStore <$> genNode,
        GrinEnsureHeap <$> genValue <*> genSmallList genValue,
        GrinStoreUnchecked <$> genNode,
        GrinFetch <$> genRuntimeRep <*> genValue,
        GrinUpdate <$> genValue <*> genValue,
        GrinEval <$> genRuntimeRep <*> genValue,
        GrinCpsEval <$> genRuntimeRep <*> genValue <*> genValue <*> genValue,
        GrinCall <$> genRuntimeRep <*> genFunctionName <*> genSmallList genValue,
        GrinPrimitiveCall <$> genRuntimeRep <*> genTextValue <*> genSmallList genValue,
        GrinCpsPrimitiveCall <$> genRuntimeRep <*> genTextValue <*> genSmallList genValue <*> genValue,
        GrinApply <$> genRuntimeRep <*> genValue <*> genSmallList genValue,
        GrinCpsApply <$> genRuntimeRep <*> genValue <*> genSmallList genValue <*> genValue,
        GrinContinue <$> genValue <*> genSmallList genValue,
        GrinCpsRaise <$> genValue <*> genValue,
        GrinUpdateBlackhole <$> genValue <*> genValue,
        GrinHalt <$> genSmallList genValue,
        GrinExit <$> genValue,
        GrinThrow <$> genValue,
        GrinCatch <$> genRuntimeRep <*> genValue <*> genValue <*> genSmallList genValue,
        GrinForeignCallExpr <$> genForeignCall <*> genSmallList genValue
      ]
    recursiveExpressions
      | depth <= 0 = []
      | otherwise =
          [ GrinBind <$> genSmallList genVar <*> smaller <*> smaller,
            GrinStoreRec <$> genSmallList ((,) <$> genVar <*> genNode) <*> smaller,
            GrinStoreRecUnchecked <$> genSmallList ((,) <$> genVar <*> genNode) <*> smaller,
            GrinCase <$> genValue <*> genVar <*> genSmallList (genAlt smaller)
          ]

genAlt :: (GrinGenerator generator) => generator GrinExpr -> generator GrinAlt
genAlt rhs = GrinAlt <$> genAltCon <*> genSmallList genVar <*> rhs

genAltCon :: (GrinGenerator generator) => generator GrinAltCon
genAltCon =
  genChoice
    [ GrinDataAlt <$> genTextValue,
      GrinLitAlt <$> genLiteral,
      pure GrinDefaultAlt
    ]

genValue :: (GrinGenerator generator) => generator GrinValue
genValue = genChoice [GrinVarValue <$> genVar, GrinLitValue <$> genLiteral]

genNode :: (GrinGenerator generator) => generator GrinNode
genNode = GrinNode <$> genNodeTag <*> genSmallList genValue

genNodeTag :: (GrinGenerator generator) => generator GrinNodeTag
genNodeTag =
  genChoice
    [ GrinConstructor <$> genTextValue <*> genInt,
      GrinClosure <$> genFunctionName <*> genSmallList (genSmallList genRuntimeRep),
      GrinThunk <$> genFunctionName
    ]

genLiteral :: (GrinGenerator generator) => generator GrinLiteral
genLiteral =
  genChoice
    [ GrinLitInt <$> genRuntimeRep <*> genInteger,
      GrinLitChar <$> genRuntimeRep <*> genChar,
      GrinLitString <$> genTextValue,
      GrinLitAddr . BS.pack <$> genSmallList genWord8
    ]

genVar :: (GrinGenerator generator) => generator GrinVar
genVar = GrinVar <$> genTextValue <*> genInt <*> genRuntimeRep

genFunctionName :: (GrinGenerator generator) => generator FunctionName
genFunctionName = FunctionName <$> genTextValue

genForeignCall :: (GrinGenerator generator) => generator GrinForeignCall
genForeignCall =
  GrinForeignCall
    <$> genTextValue
    <*> genTextValue
    <*> (GrinForeignSignature <$> genSmallList genForeignType <*> genForeignType <*> genForeignEffect)

genForeignEffect :: (GrinGenerator generator) => generator GrinForeignEffect
genForeignEffect = genElement [GrinForeignPure, GrinForeignRealWorld]

genForeignType :: (GrinGenerator generator) => generator GrinForeignType
genForeignType = genElement [GrinForeignInt, GrinForeignInt32, GrinForeignWord64, GrinForeignAddr]

genRuntimeRep :: (GrinGenerator generator) => generator RuntimeRep
genRuntimeRep = genDepth >>= genRuntimeRepSized

genRuntimeRepSized :: (GrinGenerator generator) => Int -> generator RuntimeRep
genRuntimeRepSized depth =
  genChoice (baseRepresentations <> recursiveRepresentations)
  where
    baseRepresentations =
      [ VecRep <$> genElement allVecCounts <*> genElement allVecElems,
        BoxedRep <$> genElement [Lifted, Unlifted],
        RuntimeRepVar . Unique <$> genInt,
        RuntimeRepMeta . Unique <$> genInt,
        genElement
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
      | depth <= 0 = []
      | otherwise =
          [ TupleRep <$> genSmallList (genRuntimeRepSized (depth - 1)),
            SumRep <$> genSmallList (genRuntimeRepSized (depth - 1))
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
