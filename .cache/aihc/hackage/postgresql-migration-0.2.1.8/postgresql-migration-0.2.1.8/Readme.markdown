# PostgreSQL Migrations for Haskell

Forked from [postgresql-simple-migration](https://github.com/ameingast/postgresql-simple-migration) created by [Andreas Meingast](https://github.com/ameingast/postgresql-simple-migration)

[![Haskell-CI](https://github.com/andrevdm/postgresql-simple-migration/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/andrevdm/postgresql-simple-migration/actions/workflows/haskell-ci.yml)


Welcome to postgresql-migrations, a tool for helping you with
PostgreSQL schema migrations.

This project is an open-source database migration tool. It favors simplicity
over configuration.

It is implemented in Haskell and uses the (excellent) postgresql-simple
library to communicate with PostgreSQL.

It comes in two flavors: a library that features an easy to use Haskell
API and as a standalone application.

Database migrations can be written in SQL (in this case PostgreSQL-sql)
or in Haskell.

## Why?
Database migrations should not be hard. They should be under version control
and documented both in your production systems and in your project files.

## What?
This library executes SQL/Haskell migration scripts and keeps track of their
meta information.

Scripts are executed exactly once and any changes to scripts will cause
a run-time error notifying you of a corrupted database.

The meta information consists of:
* an MD5 checksum of the executed script to make sure already existing
  scripts cannot be modified in your production system.
* a time-stamp of the date of execution so you can easily track when a change
  happened.

This library also supports migration validation so you can ensure (some)
correctness before your application logic kicks in.

## How?
This utility can be used in two ways: embedded in your Haskell program or as
a standalone binary.

### Standalone
The standalone program supports file-based migrations. To execute all SQL-files
in a directory $BASE\_DIR, execute the following command to initialize the database
in a first step.

```bash
CON="host=$host dbname=$db user=$user password=$pw"
cabal run migrate -- init $CON
cabal run migrate -- migrate $CON $BASE_DIR
```

To validate already executed scripts, execute the following:
```bash
CON="host=$host dbname=$db user=$user password=$pw"
cabal run migrate -- init $CON
cabal run migrate -- validate $CON $BASE_DIR
```

For more information about the PostgreSQL connection string, see:
[libpq-connect](http://www.postgresql.org/docs/9.3/static/libpq-connect.html).

### Library
The library supports more actions than the standalone program.

Initializing the database:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    con <- connectPostgreSQL (BS8.pack url)
    runMigration con defaultOptions MigrationInitialization
```

For file-based migrations, the following snippet can be used:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    let dir = "."
    con <- connectPostgreSQL (BS8.pack url)
    runMigration con defaltOptions $ MigrationDirectory dir
```

To run Haskell-based migrations, use this:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    let name = "my script"
    let script = "create table users (email varchar not null)";
    con <- connectPostgreSQL (BS8.pack url)
    runMigration con defaultOptions $ MigrationScript name script
```

Validations wrap _MigrationCommands_. This means that you can re-use all
MigrationCommands to perform a read-only validation of your migrations.

To perform a validation on a directory-based migration, you can use the
following code:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    con <- connectPostgreSQL (BS8.pack url)
    runMigration con default Options $ MigrationValidation (MigrationDirectory dir)
```

### Transactions

Database migrations should always be performed in a transactional context.

The standalone binary and the API default to using a new transaction for the 
entire set of migrations.

Using the `-t` argument to the binary will change this to using a new transaction
per migration step (script).

When using the library you have full control over the behaviour of transactions
by setting `optTransactionControl` on the `MigrationOptions` record.

There are three options

 1) No new transaction: you manage the transaction, e.g. if you want to run multiple migrations in a single transaction
 2) Transaction per run: This is the default. New transaction for the entire migration
 3) Transaction per step

The tests make use `TransactionPerRun`, after executing all migration-tests, the
transaction is rolled back.


### Options

The `runMigration` and `runMigrations` functions take an options record that let you
set the following

 - `optVerbose`: Is verbose logging enabled or not
 - `optLogWriter`: The function used to write log messages. Defaults to `stdout` for info and `stderr` for errors
 - `optTableName`: The name for the migrations table. This defaults to `schema_migrations`.
 - `optTransactionControl`: How transactions should be hanbled


## Compilation and Tests
The program can be built with _cabal_ or _stack_ build systems. 

The following command builds the library, the standalone binary and the test package with _cabal_

```bash
cabal configure --enable-tests && cabal build -j
```

To execute the tests, you need a running PostgreSQL server. You need to set the correct
environment varaiables for the psql connection.

Tests are executed through cabal as follows:

```bash
cabal configure --enable-tests 
PGHOST=localhost PGDATABASE=test make cabal-test
```

To build with stack use the following command

```bash
stack build
```

To run the tests with stack use the following command

```bash
PGHOST=localhost PGDATABASE=test make stack-test
```

NB note that the **psql** test argument must be set for the test suite to run. This is so that other CI environments like
stackage or nix that won't have a running test psql instance will not fail on these tests


# Changes from the original postgresql-simple-migration (version 0.1)

**postgresql-migration** is fork of *postgresql-simple-migration* created when the original *postgresql-simple-migration* project was archived.

**postgresql-migration** version 0.2.x introduces some new features that will require some minor changes if you were using a 0.1.x version before

The new features are

 - Support for custom logging (original PR from https://github.com/ameingast/postgresql-simple-migration/pull/36. Thanks @unclechu)
 - Custom migrations table name (original PR from https://github.com/ameingast/postgresql-simple-migration/pull/30)
 - Transaction control from the API (original request from https://github.com/ameingast/postgresql-simple-migration/issues/40) 


There are two ways to move to **postgresql-migration**


## Compatability layer - the simple way, but no new features

 1) Replace `postgresql-simple-migration` with `postgresql-migration` in your .cabal file
 2) Import `Database.PostgreSQL.Simple.Migration.V1Compat` rather than `Database.PostgreSQL.Simple.Migration`

All your existing code should work as is


## Porting to version 2

The most obvious code change is that you now use a `MigrationOptions` rather than a `MigrationContext`. 


_Version 0.1.x_

This what you would have had

```haskell
 withTransaction con . runMigration $ MigrationContext Pgm.MigrationInitialization True con
```


_Version 0.2.x_

Version 2 with the defaultOptions

```haskell
 runMigration con defaultOptions Pgm.MigrationInitialization
```

or if you want to change the default options

```haskell
 let options = defaultOptions { optTransactionControl = TransactionPerRun, optVerbosity = Verbose }
 runMigration con options Pgm.MigrationInitialization
```

That is all that needs to change. Your migrations scripts etc all remain as is.
