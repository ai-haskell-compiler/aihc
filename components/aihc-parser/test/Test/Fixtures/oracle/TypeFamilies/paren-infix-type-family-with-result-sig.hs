{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module ParenInfixTypeFamilyWithResultSig where

type family ((a :: [k]) ++ (b :: [k])) :: [k]
