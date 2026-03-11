{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Data.Delta.EmbeddingSpec
    ( spec
    ) where

import Prelude

import Control.Exception
    ( Exception (..)
    , SomeException
    )
import Data.Delta.Core
    ( Delta (..)
    , Replace (..)
    , apply
    )
import Data.Delta.Embedding
    ( Embedding' (..)
    , Embedding
    , fromEmbedding
    , liftUpdates
    , mkEmbedding
    , pair
    )
import Data.Delta
    ( Semigroupoid (o)
    )
import Data.Maybe
    ( isJust
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , frequency
    , forAll
    , property
    , (===)
    , (.&&.)
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "embeddingAB'" $ do
        it "prop_load_write" $
            prop_load_write embeddingAB'

        it "prop_update_apply" $
            prop_update_apply embeddingAB'

    describe "fromEmbedding . mkEmbedding'" $ do
        let embeddingAB'2 = fromEmbedding $ mkEmbedding embeddingAB'

        it "prop_load_write" $
            prop_load_write embeddingAB'2

        it "prop_update_apply" $
            prop_update_apply embeddingAB'2

    describe "pair" $ do
        let embeddingAABB = fromEmbedding $ pair embeddingAB embeddingAB

        it "prop_load_write" $
            prop_load_write embeddingAABB

        it "prop_update_apply" $
            prop_update_apply embeddingAABB

    describe "liftUpdates" $ do
        it "load . apply" $ property $
            \(das :: [DeltaA]) (a :: A) ->
                let Embedding'{load} = embeddingAB'
                    (b, _) = liftUpdates embeddingAB das a
                in  toMaybe (load b)
                        ===  Just (apply das a)

        it "apply . load" $ property $
            \(das :: [DeltaA]) (a :: A) ->
                let Embedding'{write} = embeddingAB'
                    (b, dbs) = liftUpdates embeddingAB das a
                in  b  ===  apply dbs (write a)

    describe "Semigroupid" $ do
        let e1 = diagonal `o` embeddingAB
            e2 = pair embeddingAB embeddingAB `o` diagonal

        it "prop_load_write" $
            prop_load_write (fromEmbedding e1)
            .&&. prop_load_write (fromEmbedding e2)

        it "prop_update_apply" $
            prop_update_apply (fromEmbedding e1)
            .&&. prop_update_apply (fromEmbedding e2)

        it "diagonal commutes" $ property $
            \(das :: [DeltaA]) (a :: A) ->
                let Embedding'{load=load1} = fromEmbedding e1
                    Embedding'{load=load2} = fromEmbedding e2
                    (b1, _) = liftUpdates e1 das a
                    (b2, _) = liftUpdates e2 das a
                in  isJust (toMaybe (load1 b1))
                    .&&. toMaybe (load1 b1)  ===  toMaybe (load2 b2)

-- | Law relating 'load' and 'write'.
prop_load_write
    :: ( Arbitrary (Base da), Show (Base da), Eq (Base da) )
    => Embedding' da db -> Property
prop_load_write Embedding'{load,write} =
    forAll arbitrary $ \a ->
        toMaybe (load (write a)) === Just a

-- | Law relating 'update' and 'load'.
prop_update_apply
    :: ( Delta da, Delta db, Arbitrary da, Arbitrary (Base da)
       , Show (Base da), Show da, Eq (Base da)
       )
    => Embedding' da db -> Property
prop_update_apply Embedding'{load,write,update} =
    forAll arbitrary $ \a ->
    forAll arbitrary $ \da ->
        let b = write a
            db = update a b da
        in  Just (apply da a)
                === toMaybe (load (apply db b))

toMaybe :: Either SomeException b -> Maybe b
toMaybe = either (const Nothing) Just

{-----------------------------------------------------------------------------
    Embeddings
------------------------------------------------------------------------------}
data NoPreimage = NoPreimage
    deriving Show

instance Exception NoPreimage

type A = Int
type DeltaA = Replace Int

type B = Maybe (A, Char)
data DeltaB
    = ChangeNone
    | ChangeA DeltaA
    | ChangeChar Char
    deriving Show

instance Delta DeltaB where
    type Base DeltaB = B
    apply ChangeNone b = b
    apply (ChangeA _ ) Nothing = Nothing
    apply (ChangeA da) (Just (a, c)) = Just (apply da a, c)
    apply (ChangeChar _) Nothing = Nothing
    apply (ChangeChar c) (Just (a, _)) = Just (a, c)

embeddingAB :: Embedding DeltaA DeltaB
embeddingAB = mkEmbedding embeddingAB'

embeddingAB' :: Embedding' DeltaA DeltaB
embeddingAB' = Embedding'
    { load = \b -> case b of
        Nothing -> Left $ toException NoPreimage
        Just (a,_) -> Right a
    , write = \a -> Just (a, 'X')
    , update = \_ _ da -> ChangeA da
    }

-- | Embedding into the diagonal
diagonal :: (Delta da, Eq (Base da)) => Embedding da (da, da)
diagonal = mkEmbedding diagonal'

diagonal' :: (Delta da, Eq (Base da)) => Embedding' da (da, da)
diagonal' = Embedding'
    { load = \(a1, a2) ->
        if a1 == a2
        then Right a1
        else Left $ toException NoPreimage
    , write = \a -> (a, a)
    , update = \_ _ da -> (da, da)
    }

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary DeltaA where
    arbitrary = Replace <$> arbitrary

instance Arbitrary DeltaB where
    arbitrary = frequency
        [ (1, pure ChangeNone)
        , (4, ChangeA <$> arbitrary)
        , (4, ChangeChar <$> arbitrary)
        ]
