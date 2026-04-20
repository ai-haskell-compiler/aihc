{- ORACLE_TEST xfail Local infix operator in where clause -}
module LocalInfixWhere where

data Key = MongoKey Int

instance Show Key where
    showsPrec _ input =
      shows input
      where
        infixl 3 <!>
        Left _ <!> y = y
        x      <!> _ = x
