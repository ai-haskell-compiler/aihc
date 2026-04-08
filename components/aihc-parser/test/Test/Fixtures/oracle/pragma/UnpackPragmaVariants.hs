{- ORACLE_TEST pass -}
module UnpackPragmaVariants where

-- Test case-insensitive and extra whitespace
data X1 = X1 {-#     unpack      #-} !X1

-- Test no spaces around pragma name
data X2 = X2 {-#unpack#-} !X2

-- Test no spaces with NOUNPACK
data X3 = X3 {-#nounpack#-} !X3

-- Test extra whitespace with NOUNPACK
data X4 = X4 {-#    NOUNPACK   #-} !X4
