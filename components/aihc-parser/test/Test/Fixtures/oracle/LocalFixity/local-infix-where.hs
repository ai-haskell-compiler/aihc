{- ORACLE_TEST pass -}
instance FromHttpApiData (BackendKey DB.MongoContext) where
    parseUrlPiece input = do
      MongoKey <$> readTextData s
      where
        infixl 3 <!>
        Left _ <!> y = y
        x      <!> _ = x
