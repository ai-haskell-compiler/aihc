{- ORACLE_TEST xfail parser rejects operator symbol as record field name -}
newtype T = MkT { ($$) :: Int }
