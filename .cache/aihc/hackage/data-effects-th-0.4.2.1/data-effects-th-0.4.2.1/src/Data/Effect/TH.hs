{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.TH (
    module Data.Effect.TH,
    module Data.Default,
    module Data.Function,
    EffectOrder (..),
    EffectConf (..),
    OpConf (..),
    keyedPerformerConf,
    normalPerformerConf,
    taggedPerformerConf,
    PerformerConf (..),
    performerName,
    doesGeneratePerformerSignature,
    performerDoc,
    performerArgDoc,
    performerConfs,
    deriveHFunctor,
    noGenerateNormalPerformer,
    noGenerateTaggedPerformer,
    noGenerateKeyedPerformer,
    noGeneratePerformerSignature,
    noGenerateLabel,
    noGenerateOrderInstance,
    Infinite ((:<)),
) where

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Writer.CPS (execWriterT, lift, tell)
import Data.Default (Default (def))
import Data.Effect (EffectOrder (FirstOrder, HigherOrder))
import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (
    EffectConf (..),
    EffectGenerator,
    OpConf (..),
    PerformerConf (..),
    doesGeneratePerformerSignature,
    genFOE,
    genHOE,
    keyedPerformerConf,
    noGenerateKeyedPerformer,
    noGenerateLabel,
    noGenerateNormalPerformer,
    noGenerateOrderInstance,
    noGeneratePerformerSignature,
    noGenerateTaggedPerformer,
    normalPerformerConf,
    performerArgDoc,
    performerConfs,
    performerDoc,
    performerName,
    reifyEffect,
    taggedPerformerConf,
 )
import Data.Function ((&))
import Data.List.Infinite (Infinite ((:<)))
import Language.Haskell.TH (Dec, Name, Q, Type (TupleT))

makeEffectF :: Name -> Q [Dec]
makeEffectsF :: [Name] -> Q [Dec]
makeEffectF' :: EffectConf -> Name -> Q [Dec]
(makeEffectF, makeEffectsF, makeEffectF') = effectMakers genFOEwithHFunctor

makeEffectF_ :: Name -> Q [Dec]
makeEffectsF_ :: [Name] -> Q [Dec]
makeEffectF_' :: EffectConf -> Name -> Q [Dec]
(makeEffectF_, makeEffectsF_, makeEffectF_') = effectMakers genFOE

makeEffectH :: Name -> Q [Dec]
makeEffectsH :: [Name] -> Q [Dec]
makeEffectH' :: EffectConf -> Name -> Q [Dec]
(makeEffectH, makeEffectsH, makeEffectH') = effectMakers genHOEwithHFunctor

makeEffectH_ :: Name -> Q [Dec]
makeEffectsH_ :: [Name] -> Q [Dec]
makeEffectH_' :: EffectConf -> Name -> Q [Dec]
(makeEffectH_, makeEffectsH_, makeEffectH_') = effectMakers genHOE

effectMakers
    :: EffectGenerator
    -> ( Name -> Q [Dec]
       , [Name] -> Q [Dec]
       , EffectConf -> Name -> Q [Dec]
       )
effectMakers gen =
    ( execWriterT . gen' def
    , execWriterT . mapM (gen' def)
    , \conf -> execWriterT . gen' conf
    )
  where
    gen' conf e = do
        (info, dataInfo, eInfo) <- reifyEffect e & lift
        runReaderT gen (conf, e, info, dataInfo, eInfo)

genFOEwithHFunctor :: EffectGenerator
genFOEwithHFunctor = do
    genFOE
    (_, _, _, dataInfo, _) <- ask
    deriveHFunctor (const $ pure $ TupleT 0) dataInfo & lift & lift >>= tell

genHOEwithHFunctor :: EffectGenerator
genHOEwithHFunctor = do
    genHOE
    (_, _, _, dataInfo, _) <- ask
    deriveHFunctor (const $ pure $ TupleT 0) dataInfo & lift & lift >>= tell
