{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module THEmptyListNameQuote where

import Language.Haskell.TH

-- TH value name quote for the empty list constructor
emptyListName :: Name
emptyListName = '[]

-- TH type name quote for the list type constructor
listTypeName :: Name
listTypeName = ''[]

-- Both in a where clause to exercise layout interaction
f :: Name -> Name
f x = result
  where
    result = if x == '[] then ''[] else x
