{- ORACLE_TEST pass -}
module RecordConstructionQualifiedField where

data R = R {fieldA :: Int, fieldB :: Int}

-- Construction with a qualified field name
x = R {Qual.fieldA = 1, fieldB = 2}

-- Update with a qualified field name
y = x {Qual.fieldA = 10}

-- Mixed local and qualified fields in construction
z = R {fieldA = 3, Qual.fieldB = 4}
