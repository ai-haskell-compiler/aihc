{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.DBCleaner.PostgreSQLSimpleSpec where

import           Control.Applicative                 ((<$>), (<*>))
import           Control.Monad                       (void)
import           Data.Text                           (Text)
import           Database.PostgreSQL.Simple          (Connection,
                                                      connectPostgreSQL,
                                                      execute, query_)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Test.Hspec

import           Database.DBCleaner.PostgreSQLSimple (withConnection)
import           Database.DBCleaner.Types            (Strategy (..))

data User = User
  { userName  :: Text
  , userEmail :: Text
  } deriving (Show, Eq)

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance ToRow User where
  toRow User{..} = [ toField userName
                   , toField userEmail
                   ]

withTestConnection :: Strategy -> (Connection -> IO a) -> IO a
withTestConnection s f = connectPostgreSQL "dbname=dbcleaner" >>= withConnection s f

createUser :: Connection -> User -> IO ()
createUser c = void . execute c "INSERT INTO users (name, email) VALUES (?, ?)"

listUsers :: Connection -> IO [User]
listUsers c = query_ c "SELECT name, email FROM users"

spec :: Spec
spec =
  describe "withPGConnection" $ do
    context "when strategy is Transaction" $
      it "deletes all records create inside the transaction" $ do
        withTestConnection Transaction $ \c -> do
          let user1 = User "user1" "user1@example.com"
              user2 = User "user2" "user2@example.com"
          mapM_ (createUser c) [user1, user2]
          listUsers c >>= shouldMatchList [user1, user2]

        withTestConnection Transaction listUsers `shouldReturn` []

    context "when strategy is Truncation" $
      it "truncates all the tables of the schema" $ do
        withTestConnection Truncation $ \c -> do
          let user1 = User "user1" "user1@example.com"
              user2 = User "user2" "user2@example.com"
          mapM_ (createUser c) [user1, user2]
          listUsers c >>= shouldMatchList [user1, user2]

        withTestConnection Truncation listUsers `shouldReturn` []
