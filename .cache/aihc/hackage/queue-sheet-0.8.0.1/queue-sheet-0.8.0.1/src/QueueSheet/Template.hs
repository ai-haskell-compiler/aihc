------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet.Template
-- Description : queue sheet template functions
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QueueSheet.Template
  ( -- * API
    loadTemplate
  , renderTemplate
  ) where

-- https://hackage.haskell.org/package/base
import Data.Bifunctor (first)
import Data.Either (fromRight)
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif
import qualified System.IO as IO

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger
import Text.Ginger ((~>))

-- https://hackage.haskell.org/package/text
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Writer (Writer)

-- (queue-sheet)
import QueueSheet.Types
  ( Date, Item, Name
  , Queue
      ( Queue, queueDate, queueItems, queueName, queueSection, queueTags
      , queueUrl
      )
  , QueueSheet(QueueSheet, qsQueues, qsSections), Section, Tag(Tag), Url
  )

------------------------------------------------------------------------------
-- $QueueCtx

-- | Queue context
data QueueCtx
  = QueueCtx
    { name      :: !Name
    , url       :: !(Maybe Url)
    , date      :: !(Maybe Date)
    , tags      :: ![Tag]
    , prevItem  :: !(Maybe Item)
    , nextItems :: ![Item]
    }

instance Ginger.ToGVal m QueueCtx where
  toGVal QueueCtx{..} = Ginger.dict $
    [ "name"       ~> name
    , "url"        ~> url
    , "date"       ~> date
    , "prev_item"  ~> prevItem
    , "next_items" ~> nextItems
    ] ++ [("tag_" <> tag) ~> True | Tag tag <- tags]

-- | Construct a queue context
queueCtx :: Queue -> QueueCtx
queueCtx Queue{..} = QueueCtx
    { name      = queueName
    , url       = queueUrl
    , date      = queueDate
    , tags      = queueTags
    , prevItem  = either Just (const Nothing) =<< queueItems
    , nextItems = maybe [] (fromRight []) queueItems
    }

------------------------------------------------------------------------------
-- $SectionCtx

-- | Section context
newtype SectionCtx = SectionCtx (Section, [QueueCtx])

instance Ginger.ToGVal m SectionCtx where
  toGVal (SectionCtx (section, queues)) = Ginger.dict
    [ "name"   ~> section
    , "queues" ~> queues
    ]

-- | Check if a section context has any queues
sectionCtxHasQueues :: SectionCtx -> Bool
sectionCtxHasQueues (SectionCtx (_, [])) = False
sectionCtxHasQueues _                    = True

------------------------------------------------------------------------------
-- $Context

-- | Template context
newtype Context = Context [SectionCtx]
  deriving newtype (Ginger.ToGVal m)

-- | Template context constructor
context :: [Section] -> [Queue] -> Context
context sections queues = Context $ filter sectionCtxHasQueues
    [ SectionCtx
        ( section
        , [ queueCtx queue
          | queue <- queues, queueSection queue == section
          ]
        )
    | section <- sections
    ]

-- | Create a Ginger context from a template context
gingerContext
  :: Context
  -> Ginger.GingerContext Ginger.SourcePos (Writer Text) Text
gingerContext ctx = Ginger.makeContextText $ \case
    "sections" -> Ginger.toGVal ctx
    _          -> Ginger.toGVal (Nothing :: Maybe Text)

------------------------------------------------------------------------------
-- $API

-- | Load a Ginger template
--
-- @since 0.3.0.0
loadTemplate
  :: FilePath
  -> IO (Either String (Ginger.Template Ginger.SourcePos))
loadTemplate path = first formatError <$> Ginger.parseGingerFile' options path
  where
    options :: Ginger.ParserOptions IO
    options = Ginger.ParserOptions
      { poIncludeResolver     = fmap Just . IO.readFile
      , poSourceName          = Nothing
      , poKeepTrailingNewline = False
      , poLStripBlocks        = False
      , poTrimBlocks          = False
      , poDelimiters          = Ginger.Delimiters
          { delimOpenInterpolation  = "<<"
          , delimCloseInterpolation = ">>"
          , delimOpenTag            = "<!"
          , delimCloseTag           = "!>"
          , delimOpenComment        = "<#"
          , delimCloseComment       = "#>"
          }
      }

    formatError :: Ginger.ParserError -> String
    formatError err = concat
      [ "error loading template: "
      , maybe path show $ Ginger.peSourcePosition err
      , ": "
      , Ginger.peErrorMessage err
      ]

-- | Render a template using the given context
--
-- @since 0.3.0.0
renderTemplate
  :: FilePath
  -> Ginger.Template Ginger.SourcePos
  -> QueueSheet
  -> IO ()
renderTemplate path template QueueSheet{..} =
    let ctx = gingerContext $ context qsSections qsQueues
    in  TIO.writeFile path $ Ginger.runGinger ctx template
