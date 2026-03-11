{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestApp where

import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (ToJSON, Value, defaultOptions, genericToJSON, toJSON)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Database.Persist
  ( Entity(entityKey)
  , Key
  , SelectOpt(..)
  , keyValueEntityToJSON
  , persistIdField
  , selectList
  , (<.)
  , (==.)
  , (>.)
  )
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400)
import Yesod
  ( Approot(..)
  , MonadHandler
  , MonadUnliftIO
  , Yesod(approot)
  , YesodPersist
  , YesodPersistBackend
  , lookupGetParam
  , mkYesod
  , parseRoutes
  , renderRoute
  , returnJson
  , runDB
  , sendResponseStatus
  )
import Yesod.Page

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SomeAssignment
  teacherId Int
  courseId Int
  createdAt UTCTime
  deriving (Generic)
|]

instance ToJSON SomeAssignment where
  toJSON = genericToJSON defaultOptions

data Simple = Simple

mkYesod "Simple" [parseRoutes|
/some-route SomeR GET
/some-route-absolute SomeAbsoluteR GET
/some-route-link SomeLinkR GET
/some-route-link-absolute SomeLinkAbsoluteR GET
|]

instance Yesod Simple where
  approot = ApprootStatic "http://localhost:3000"

runDB'
  :: (MonadUnliftIO m, MonadLogger m, MonadLoggerIO m)
  => ReaderT SqlBackend m a
  -> m a
runDB' f = withSqliteConn ":test:" $ runReaderT f

instance YesodPersist Simple where
  type YesodPersistBackend Simple = SqlBackend
  runDB = runDB'

optionalParam :: Read a => MonadHandler m => Text -> m (Maybe a)
optionalParam name = fmap (read . unpack) <$> lookupGetParam name

requireParam :: (MonadHandler m, Read a) => Text -> m a
requireParam name = maybe badRequest pure =<< optionalParam name
 where
  badRequest =
    sendResponseStatus status400 $ "A " <> name <> " parameter is required."

getSomeR :: Handler Value
getSomeR = makePaginationRoute withPage

getSomeAbsoluteR :: Handler Value
getSomeAbsoluteR = makePaginationRoute withPageAbsolute

getSomeLinkR :: Handler Value
getSomeLinkR = makePaginationRoute withPageLink

getSomeLinkAbsoluteR :: Handler Value
getSomeLinkAbsoluteR = makePaginationRoute withPageLinkAbsolute

type Pagination m f a
  = Int
  -> (Entity a -> Key a)
  -> (Cursor (Key a) -> m [Entity a])
  -> m (f (Entity a))

makePaginationRoute
  :: (Functor f, ToJSON (f Value))
  => Pagination Handler f SomeAssignment
  -> Handler Value
makePaginationRoute withPage' = do
  teacherId <- requireParam "teacherId"
  mCourseId <- optionalParam "courseId"

  items <- withPage' 100 entityKey $ \Cursor {..} ->
    runDB $ sort cursorPosition <$> selectList
      (catMaybes
        [ Just $ SomeAssignmentTeacherId ==. teacherId
        , (SomeAssignmentCourseId ==.) <$> mCourseId
        , whereClause cursorPosition
        ]
      )
      [LimitTo $ unLimit cursorLimit, orderBy cursorPosition]
  returnJson $ keyValueEntityToJSON <$> items
 where
  whereClause = \case
    First -> Nothing
    Next p -> Just $ persistIdField >. p
    Previous p -> Just $ persistIdField <. p
    Last -> Nothing

  orderBy = \case
    First -> Asc persistIdField
    Next{} -> Asc persistIdField
    Previous{} -> Desc persistIdField
    Last -> Desc persistIdField

  sort = \case
    First -> id
    Next{} -> id
    Previous{} -> sortOn entityKey
    Last -> sortOn entityKey
