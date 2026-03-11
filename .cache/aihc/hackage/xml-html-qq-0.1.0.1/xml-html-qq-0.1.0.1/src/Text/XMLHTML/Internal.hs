{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Text.XMLHTML.Internal

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

-}

module Text.XMLHTML.Internal where

import Control.Exception (SomeException)
import Instances.TH.Lift ()
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.XML
       (Doctype(..), Document(..), Element(..), ExternalID(..),
        Instruction(..), Miscellaneous(..), Name(..), Node(..),
        Prologue(..))

$(deriveLiftMany
    [ ''ExternalID
    , ''Node
    , ''Name
    , ''Instruction
    , ''Doctype
    , ''Element
    , ''Miscellaneous
    , ''Prologue
    , ''Document
    ])

-- | Create a 'QuasiQuoter' for 'Exp's.
createExpQuasiQuoter
  :: (String -> Q Exp)
  -- ^ The function to use for 'QuasiQuoter's 'quoteExp'.
  -> QuasiQuoter
createExpQuasiQuoter f =
  QuasiQuoter
  { quoteExp = f
  , quotePat = error "not used"
  , quoteType = error "not used"
  , quoteDec = error "not used"
  }

-- | This function handles errors that occur when a 'Document' can't be parsed.
--
-- This function throws an 'error' with an explanation of what happened.
handleParseDocErr
  :: String
  -- ^ The type of a document that was being parsed.  Should either be
  -- @\"XML\"@ or @\"HTML\"@.
  -> String
  -- ^ The name of the function that was being used to parse the document.
  -- Should probably either be @\"Text.XML.parseText\"@ or
  -- @\"Text.HTML.DOM.parseLT\"@ depending on whether you're parsing XML or
  -- HTML.
  -> String
  -- ^ The actual XML or HTML string that you were trying to parse into a
  -- 'Document'.
  -> SomeException
  -- ^ The exception that occurred when trying to parse the 'Document'.
  -> a
handleParseDocErr docType parseFunction string exception =
  let msg =
        "ERROR: Trying to parse a string into an " `mappend`
        docType `mappend`
        " Document,\n" `mappend`
        "but got the following error from " `mappend`
        parseFunction `mappend`
        ":\n" `mappend`
        show exception `mappend`
        "\n" `mappend`
        "attempting to parse the following document:\n" `mappend`
        string
  in error msg
