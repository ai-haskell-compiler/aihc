{- ORACLE_TEST xfail Local infix operator in where clause -}
instance FromHttpApiData (BackendKey DB.MongoContext) where
    parseUrlPiece input = do
      MongoKey <$> readTextData s
      where
        infixl 3 <!>
        Left _ <!> y = y
        x      <!> _ = x
