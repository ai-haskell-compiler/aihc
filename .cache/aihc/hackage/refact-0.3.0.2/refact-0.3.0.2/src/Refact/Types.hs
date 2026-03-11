{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Refact.Types where

import Data.Data

-- | A generic SrcSpan, usually this is converted immediately to a native
-- representation. (For example a GHC SrcSpan or a HSE SrcSpan)
data SrcSpan = SrcSpan
                { startLine :: {-# UNPACK #-} !Int
                , startCol  :: {-# UNPACK #-} !Int
                , endLine   :: {-# UNPACK #-} !Int
                , endCol    :: {-# UNPACK #-} !Int }
                deriving (Read, Show, Eq, Ord, Data, Typeable)


-- | Types of expressions which we are able to replace.
data RType = Expr | Decl | Type | Pattern | Stmt | ModuleName | Bind | Match | Import
           deriving (Read, Ord, Show, Eq, Data, Typeable)

-- | Supported refactorings
data Refactoring a =
  Replace  {
      rtype :: RType -- ^ Type of expression to be replaced
    , pos :: a  -- ^ Expression to replace
    , subts :: [(String, a)] -- ^ Substitutions to make
    , orig  :: String -- ^ Replacement template
    }
  | ModifyComment {
      pos :: a
    , newComment :: String
    }
  | InsertComment {
      pos :: a
    , newComment :: String
    }
  | Delete {
      rtype :: RType
    , pos :: a
    }

  | -- |  Takes the position of a import decl and removes the as keyword
    RemoveAsKeyword
      {
      pos :: a
      }
--  | Rename {
--      nameSubts :: [(String, String)]
--    }
  deriving (Show, Read, Functor, Eq, Ord, Data, Typeable )



