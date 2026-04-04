{- ORACLE_TEST xfail polysemy-fs template haskell quote before promoted cons -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TH_Quote_Then_Promoted_Cons where

data FSDir
''FSDir

f :: x (FSDir ': r)
f = undefined
