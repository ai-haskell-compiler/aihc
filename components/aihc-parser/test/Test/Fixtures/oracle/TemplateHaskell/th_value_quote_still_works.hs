{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TH_Value_Quote_Not_Broken where

-- Test that regular TH value quotes still work
valueQuote = 'justConstructor

justConstructor :: Maybe a -> Bool
justConstructor (Just _) = True
justConstructor Nothing = False

-- Test TH type quotes
typeQuote = ''Int

-- Test promoted cons in types (should work alongside TH)
data HList (xs :: [*])

cons :: HList (a ': b)
cons = undefined

-- Test promoted empty list
empty :: HList '[]
empty = undefined

-- Test promoted list syntax
list :: HList '[Int, Bool]
list = undefined
