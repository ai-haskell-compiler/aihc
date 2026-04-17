{- ORACLE_TEST xfail reason="infix type operator in instance context roundtrips to prefix form" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module M where

class (xs :: [k]) `IsPrefixOf` (ys :: [k])

instance (xs `IsPrefixOf` ys) => (x ': xs) `IsPrefixOf` (x ': ys)
