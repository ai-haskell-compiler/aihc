{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , NamedFieldPuns
  , ScopedTypeVariables
  , OverloadedStrings
  , QuasiQuotes
  , DeriveGeneric
  #-}

module Path.Extended
  ( -- * Types
    Location (..)
  , LocationPath (..)
  , QueryParam
  , -- * Classes
    ToPath (..)
  , ToLocation (..)
  , FromPath (..)
  , FromLocation (..)
  , -- * Combinators
    -- ** Path
    fromDir
  , fromFile
  , prependAbs
  , prependRel
  , -- ** Query Parameters
    setQuery
  , addQuery
  , (<&>)
  , addQueries
  , delQuery
  , getQuery
  , -- ** Fragment
    setFragment
  , addFragment
  , (<#>)
  , delFragment
  , getFragment
  , -- ** Parser & Printer
    locationAbsParser
  , locationRelParser
  , printLocation
  ) where

-- import Path as P hiding ((</>))
import Path (Path, Abs, Rel, Dir, File, (</>), toFilePath, parseAbsFile, parseAbsDir, parseRelDir, parseRelFile, stripProperPrefix, absdir, reldir)

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text (Parser, char, takeWhile, takeWhile1, sepBy)
import qualified Data.Text as T
import Control.Applicative ((<|>), optional)
import Control.Exception (SomeException)
import Control.Monad (void)
import GHC.Generics (Generic)



-- | Convenience typeclass for symbolic, stringless routes - make an instance
-- for your own data type to use your constructors as route-referencing symbols.
class ToPath sym base type' | sym -> base type' where
  toPath :: sym -> Path base type'

-- | Convenience typeclass for symbolic, stringless routes - make an instance
-- for your own data type to use your constructors as route-referencing symbols.
class ToLocation sym base | sym -> base where
  toLocation :: sym -> Location base

class FromPath sym base type' | sym -> base type' where
  parsePath :: Path base type' -> Either String sym

class FromLocation sym base | sym -> base where
  parseLocation :: Location base -> Either String sym


data LocationPath base
  = Dir (Path base Dir)
  | File (Path base File)
  deriving (Eq, Ord, Generic, Show)

-- | A location for some base and type - internally uses @Path@.
data Location base = Location
  { locPath        :: LocationPath base
  , locQueryParams :: [QueryParam]
  , locFragment    :: Maybe String
  } deriving (Eq, Ord, Generic, Show)



fromDir :: Path base Dir -> Location base
fromDir path = Location
  { locPath = Dir path
  , locQueryParams = []
  , locFragment = Nothing
  }

fromFile :: Path base File -> Location base
fromFile path = Location
  { locPath = File path
  , locQueryParams = []
  , locFragment = Nothing
  }


locationAbsParser :: Parser (Location Abs)
locationAbsParser = do
  divider
  locPath <- do
    xs <- chunk `sepBy` divider
    case xs of
      [] -> pure $ Dir [absdir|/|]
      _ -> do
        let dir = do
              divider
              case parseAbsDir (T.unpack ("/" <> T.intercalate "/" xs <> "/")) of
                Left (e :: SomeException) -> fail (show e)
                Right x -> pure (Dir x)
            file =
              case parseAbsFile (T.unpack ("/" <> T.intercalate "/" xs)) of
                Left (e :: SomeException) -> fail (show e)
                Right x -> pure (File x)
        dir <|> file
  locQueryParams <- do
    xs <- optional $ do
      let val = T.unpack <$> takeWhile (`notElem` ['=','&','#'])
      void (char '?')
      let kv = do
            k <- val
            mV <- optional $ do
              void (char '=')
              val
            pure (k,mV)
      kv `sepBy` void (char '&')
    case xs of
      Nothing -> pure []
      Just xs' -> pure xs'
  locFragment <- optional $ do
    void (char '#')
    xs <- takeWhile (const True)
    pure (T.unpack xs)
  pure Location
    { locPath
    , locQueryParams
    , locFragment
    }
  where
    divider = void (char '/')
    chunk = takeWhile1 (`notElem` ['?','&','/','#'])

locationRelParser :: Parser (Location Rel)
locationRelParser = do
  locPath <- do
    xs <- chunk `sepBy` divider
    case xs of
      [] -> pure $ Dir [reldir|./|]
      _ -> do
        let dir = do
              divider
              case parseRelDir (T.unpack (T.intercalate "/" xs <> "/")) of
                Left (e :: SomeException) -> fail (show e)
                Right x -> pure (Dir x)
            file =
              case parseRelFile (T.unpack (T.intercalate "/" xs)) of
                Left (e :: SomeException) -> fail (show e)
                Right x -> pure (File x)
        dir <|> file
  locQueryParams <- do
    xs <- optional $ do
      let val = T.unpack <$> takeWhile (`notElem` ['=','&','#'])
      void (char '?')
      let kv = do
            k <- val
            mV <- optional $ do
              void (char '=')
              val
            pure (k,mV)
      kv `sepBy` void (char '&')
    case xs of
      Nothing -> pure []
      Just xs' -> pure xs'
  locFragment <- optional $ do
    void (char '#')
    xs <- takeWhile (const True)
    pure (T.unpack xs)
  pure Location
    { locPath
    , locQueryParams
    , locFragment
    }
  where
    divider = void (char '/')
    chunk = takeWhile1 (`notElem` ['?','&','/','#'])


prependAbs :: Path Abs Dir -> Location Abs -> Location Abs
prependAbs path l@Location{locPath} =
  case locPath of
    File f ->
      l { locPath = case stripProperPrefix [absdir|/|] f of
            Nothing -> undefined
            Just f' -> File (path </> f')
        }
    Dir d ->
      l { locPath = case stripProperPrefix [absdir|/|] d of
            Nothing -> undefined
            Just d' -> Dir (path </> d')
        }

prependRel :: Path Rel Dir -> Location Rel -> Location Rel
prependRel path l@Location{locPath} =
  case locPath of
    File f ->
      l { locPath = File (path </> f)
        }
    Dir d ->
      l { locPath = Dir (path </> d)
        }


printLocation :: Location base -> T.Text
printLocation (Location pa qp fr) =
  let loc = case pa of
        Dir x -> toFilePath x
        File x -> toFilePath x
      query = case qp of
                [] -> ""
                qs -> "?" <> T.intercalate "&" (go <$> qs)
        where
          go (k,mv) = T.pack k <> maybe "" (\v -> "=" <> T.pack v) mv
  in T.pack loc <> query <> maybe "" (\f -> "#" <> T.pack f) fr

type QueryParam = (String, Maybe String)



setQuery :: [QueryParam] -> Location base -> Location base
setQuery qp (Location pa _ fr) =
  Location pa qp fr

-- | Appends a query parameter
addQuery :: QueryParam -> Location base -> Location base
addQuery q (Location pa qp fr) =
  Location pa (qp ++ [q]) fr

(<&>) :: Location base -> QueryParam -> Location base
(<&>) = flip addQuery

infixl 7 <&>

addQueries :: [QueryParam] -> Location base -> Location base
addQueries qs (Location pa qs' fr) =
  Location pa (qs' ++ qs) fr

delQuery :: Location base -> Location base
delQuery = setQuery []

getQuery :: Location base -> [QueryParam]
getQuery (Location _ qp _) =
  qp


setFragment :: Maybe String -> Location base -> Location base
setFragment fr (Location pa qp _) =
  Location pa qp fr

addFragment :: String -> Location base -> Location base
addFragment fr = setFragment (Just fr)

(<#>) :: Location base -> String -> Location base
(<#>) = flip addFragment

infixl 8 <#>

delFragment :: Location base -> Location base
delFragment = setFragment Nothing

getFragment :: Location base -> Maybe String
getFragment (Location _ _ x) = x
