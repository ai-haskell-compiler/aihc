{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module M where

class (xs :: [k]) `IsPrefixOf` (ys :: [k])

instance '[] `IsPrefixOf` ys
