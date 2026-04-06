{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TH_Promoted_Cons_Edge_Cases where

-- Test 1: TH quote followed by promoted cons in same module
''Int

f1 :: x (a ': b ': c)
f1 = undefined

-- Test 2: Nested promoted cons
f2 :: x (a ': (b ': c))
f2 = undefined

-- Test 3: Multiple TH quotes before promoted cons
''Bool
''Char
f3 :: x (a ': b)
f3 = undefined

-- Test 4: Promoted cons in function type signature
f4 :: (a ': b) -> (c ': d)
f4 = undefined

-- Test 5: Promoted cons with complex types
data Foo
data Bar

f5 :: x (Foo ': Bar ': '[])
f5 = undefined
