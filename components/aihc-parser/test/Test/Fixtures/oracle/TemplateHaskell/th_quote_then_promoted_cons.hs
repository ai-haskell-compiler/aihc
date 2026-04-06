{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TH_Quote_Then_Promoted_Cons where

data FSDir
''FSDir

f :: x (FSDir ': r)
f = undefined
