-- | Bindings to the [SQLite3 C library](https://www.sqlite.org).
-- This is a very simple library that puts a minimal number of
-- abstractions between the user and the underlying C library.
-- Some notable abstractions that do appear:
--
-- * use of 'Data.Text.Text' and 'Data.ByteString.ByteString'
-- in the user-facing API rather than the underlying C types
--
-- * use of the [SQLite3 extended error
-- codes](https://www.sqlite.org/c3ref/errcode.html)
--
-- * all errors indicated with exceptions (embraces the philosophy
-- that [exceptions are inevitable in
-- IO](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell))
--
-- * extensive contextual information given in exceptions, to help
-- you figure out where the problem arose
--
-- * 'Database' and 'Statement' handles are cleaned up for you, so
-- there is no need to manually de-allocate them
--
-- The SQLite library is built along with Squeather, so there is no
-- dependency on a system-wide library and there is nothing else to
-- build.  The following [run-time loadable
-- extensions](https://www.sqlite.org/loadext.html) are enabled in
-- the build:
--
-- * [FTS3](https://www.sqlite.org/fts3.html)
-- * [FTS4](https://www.sqlite.org/fts3.html)
-- * [FTS5](https://www.sqlite.org/fts5.html)
-- * [JSON1](https://www.sqlite.org/json1.html)
--
-- However, at this point none of the run-time loadable extensions
-- have been tested.
--
-- In addition, the SQLite C library is compiled with the
-- @-DSQLITE_DQS=0@ compile-time option, so that double-quoted
-- string literals in SQL are NOT accepted.  Be sure to
-- single-quote your string literals and double-quote your
-- identifiers in SQL.  For details on what this is about, see
--
-- https://sqlite.org/quirks.html#double_quoted_string_literals_are_accepted
--
-- The name comes from @SQL@ and the feather which is used in the
-- SQLite logo.
module Squeather
  ( -- * Database handles, opening databases
    Database
  , open

  -- ** Opening with flags
  , Create(..)
  , WriteMode(..)
  , ThreadMode(..)
  , CacheMode(..)
  , OpenFlags(..)
  , openFlags
  , openWithFlags

  -- * Easy execution of statements
  -- | Often this is all you will need to execute statements.
  , exec
  , SQLData(..)
  , execute
  , executeNamed
  , executeNamedWithColumns

  -- * Statistics
  , lastInsertRowId
  , changes

  -- * Backups
  , Source(..)
  , Destination(..)
  , backup

  -- * Statements
  -- | For more control over statement execution.
  , Statement
  , StepResult(..)
  , prepare
  , columnCount
  , columnName
  , columnNames
  , bindParams
  , step
  , column
  , columns
  , allRows
  , reset
  , clearBindings

  -- * Errors
  , ErrorFlag(..)
  , SqueatherErrorFlag(..)
  , Error(..)

  -- * Version
  , sqliteVersion
  ) where

import Squeather.Internal
import Squeather.Internal.Bindings
import Squeather.Internal.Types
