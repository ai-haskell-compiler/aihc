-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Query
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Command line parsing
-----------------------------------------------------------------------------

module Lentil.Args where

import Lentil.Types
import Lentil.Query
import Lentil.Helpers

import Options.Applicative
import qualified Data.Char  as C
import qualified Data.Maybe as M


-----------
-- TYPES --
-----------

data LOptions = LOptions { loInExcl    :: ([FilePath], [FilePath]),
                           loFormat    :: Format,
                           loFilters   :: [LFilter],
                           loSort      :: [LSort],
                           loAlias     :: [Alias],
                           loFlagwords :: [FlagWord],
                           loOutFile   :: Maybe FilePath }

type LFilter = [Issue] -> [Issue]
type ChType  = [LFilter] -> [Issue] -> [Issue] -- AND or OR chain
type LSort   = [Issue] -> [Issue]


lOpts :: Parser LOptions
lOpts = LOptions <$> inexcls <*> format <*> filters <*>
                     issort <*> aliases <*> flagwords <*> outfile
    where inexcls = (,) <$> includes <*> many exclude
          filters = many $ foldl1 (<|>) [tag, notag, path,
                                         nopath, desc, nodesc]
          aliases   = fmap M.catMaybes (many alias)
          flagwords = many flagword


-------------
-- PARSERS --
-------------

-- argument "." gets replaced to "" (all files)
includes :: Parser [FilePath]
includes = many (argument str (metavar "PATH..."))

exclude :: Parser FilePath
exclude = strOption ( short 'x'      <>
                      metavar "PATH" <>
                      help "file/directory to exclude" )

format :: Parser Format
format = option (str >>= parseFormat)
                    ( short 'f'      <>
                      metavar "TYPE" <>
                      value Pretty   <>
                      help "output format (pretty, tagpop, file, csv, comp, xml)" )
    where
          parseFormat :: String -> ReadM Format
          parseFormat s = let asl = map forTup (enumFrom minBound)
                          in maybe (rerr s "unrecognised format")
                                   return (lookup s asl)

          forTup f      = (map C.toLower $ show f, f)


alias :: Parser (Maybe Alias)
alias = option (str >>= parseAlias)
                   ( short 'a'       <>
                     metavar "ALIAS" <>
                     help ("extension alias (e.g.: -a d" ++ aliasSign ++
                          "cpp)") )
    where
          parseAlias :: String -> ReadM (Maybe Alias)
          parseAlias s = return (aliasp s)


flagword :: Parser FlagWord
flagword =  fmap normaliseFlagword $
                strOption ( short 'w'          <>
                            metavar "FLAGWORD" <>
                            help "additional flagword (e.g.: -w Hack)" )


outfile :: Parser (Maybe FilePath)
outfile = optional $ strOption
                    ( long "output"  <>
                      metavar "FILE" <>
                      help "output file (if not present, prints to stdout)" )



issort :: Parser [LSort]
issort = pure []


-------------------
-- FILTER PARAMS --
-------------------

path :: Parser LFilter
path = option (filterFilepath <$> str)
                       ( short 'p'        <>
                         metavar "EXPR"   <>
                         help "filters for filepath matching EXPR" )

nopath :: Parser LFilter
nopath = option (negFilter . filterFilepath <$> str)
                         ( short 'P'        <>
                           metavar "EXPR"   <>
                           help "filters for filepath NOT matching EXPR" )

desc :: Parser LFilter
desc = option (filterDescription <$> str)
                       ( short 'd'        <>
                         metavar "EXPR"   <>
                         help "filters for description matching EXPR" )

nodesc :: Parser LFilter
nodesc = option (negFilter . filterDescription <$> str)
                       ( short 'D'        <>
                         metavar "EXPR"   <>
                         help "filters for description NOT matching EXPR" )

tag :: Parser LFilter
tag = option (filterTags <$> str)
                       ( short 't'       <>
                         metavar "EXPR"  <>
                         help "filter for tag matching EXPR" )

notag :: Parser LFilter
notag = option (negFilter . filterTags <$> str)
                       ( short 'T'       <>
                         metavar "EXPR"  <>
                         help "filter for tag NOT matching EXPR" )


-----------------
-- ANCILLARIES --
-----------------

rerr :: String -> String -> ReadM a
rerr var msg = readerError $ msg ++ " \"" ++ var ++ "\""
