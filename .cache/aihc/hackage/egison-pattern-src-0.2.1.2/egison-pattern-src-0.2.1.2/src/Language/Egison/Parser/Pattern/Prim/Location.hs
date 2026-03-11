{-# LANGUAGE CPP #-}

-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.Location
-- Description: Helpers to handle source code locations
-- Stability:   experimental
--
-- A helper module to handle source code locations during parsing.


module Language.Egison.Parser.Pattern.Prim.Location
  ( Location(..)
  , Position(..)
  , Locate(..)
  -- * Conversion
  , fromSourcePos
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data
                                                , Typeable
                                                )

import qualified Text.Megaparsec               as Parsec
                                                ( SourcePos(..)
                                                , unPos
                                                )


-- | Position in source code.
data Position
  = Position { line   :: Int
             , column :: Int
             }
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Location, a range of positions in source code.
data Location
  = Location { begin :: Position
             , end   :: Position
             }
  deriving (Show, Eq, Generic, Data, Typeable)

-- | 'Monad' that scans over a source code.
class Monad m => Locate m where
  getPosition :: m Position
  getLocation :: m a -> m (a, Location)

  getLocation m = do
    begin <- getPosition
    x <- m
    end <- getPosition
    let location = Location { begin, end }
    pure (x, location)


-- | Make 'Position' from 'Parsec.SourcePos'
fromSourcePos :: Parsec.SourcePos -> Position
fromSourcePos Parsec.SourcePos { Parsec.sourceLine, Parsec.sourceColumn } =
  Position { line, column }
 where
  line   = Parsec.unPos sourceLine
  column = Parsec.unPos sourceColumn
