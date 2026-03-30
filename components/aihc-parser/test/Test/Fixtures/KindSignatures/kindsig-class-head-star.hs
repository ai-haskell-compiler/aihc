{- ORACLE_TEST
id: kindsig-class-head-star
category: declarations
expected: pass
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}

module KindSignaturesClassHeadStar where

class Unit (x :: *)
