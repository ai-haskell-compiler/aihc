{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Database.PostgreSQL.Helpers
    ( TableName(..), FieldName(..)
    , (@=), insert
    )
where

import           Control.Monad
import qualified Data.List                          as L
import           Data.Monoid
import           Data.String
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField

newtype TableName
    = TableName { unTableName :: Query }
      deriving (Show, Eq, IsString)

newtype FieldName
    = FieldName { unFieldName :: Query }
      deriving (Show, Eq, IsString)

(@=) :: ToField a => FieldName -> a -> (FieldName, Action)
fld @= val = (fld, toField val)

-- | Build and run an insert query
insert :: Connection -> TableName -> [(FieldName, Action)] -> IO ()
insert conn (TableName tbl) xs =
    void $ execute conn q (map snd xs)
    where
        q =
            "INSERT INTO " <> tbl <> " (" <> fst fieldNames <> ") VALUES (" <> snd fieldNames <> ")"
        fieldNames =
            snd $
            L.foldl' (\(isFirst, (fldQ, placeQ)) (FieldName fld, _) ->
                        let prefix =
                                if not isFirst then ", " else ""
                        in ( False
                           , ( fldQ <> prefix <> fld
                             , placeQ <> prefix <> "?"
                             )
                           )
                   ) (True, ("", "")) xs
