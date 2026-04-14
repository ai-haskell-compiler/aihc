{- ORACLE_TEST xfail reason="typelevel-tools-yj uses promoted list syntax in infix typeclass instance heads, which the parser rejects" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module M where

class (xs :: [k]) `IsPrefixOf` (ys :: [k])

instance '[] `IsPrefixOf` ys
