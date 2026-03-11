import Lentil.Types
import Lentil.Args
import Lentil.File
import Lentil.Print
import Lentil.Query
import Lentil.Export
import Data.Monoid () -- 8.2

import Data.Semigroup as S
import Options.Applicative

import qualified System.IO as I

-- todo [feature:basic] ignore dist dist-newstyle and common folders
-- todo [feature:basic] help su `lentil` più semplie (`use lentil .`)
-- todo [design] lentil con lexer?
-- todo [feature:basic] lentil <-- invoke with lentil . or lentil --help
-- todo [feature:basic] add groff syntax
-- todo [feature:advanced] support python/lisp docstrings

main :: IO ()
main = I.hSetBuffering I.stderr I.NoBuffering >> -- b/c progress bar
       execParser opts >>= \lo ->
       if null $ (fst . loInExcl) lo
         then putStrLn "lentil - frugal issue tracker" >>
              putStrLn "\n  Missing PATH..." >>
              putStrLn "\n  example usage: lentil source-folder/" >>
              putStrLn "\nlentil --help  for more options"
         else runLentil lo
    where opts = info (lOpts <* version <* helpOvert)
            ( fullDesc S.<>
              header "lentil - frugal issue tracker" S.<>
              progDesc "example: lentil source-folder/" S.<>
              footer "manual and examples: http://www.ariis.it/static/articles/lentil-manual/page.html")

-- overt help text (even in condensed help)
helpOvert :: Parser (a -> a)
helpOvert = abortOption sht $ mconcat [ long "help",
                                        short 'h',
                                        help "show this help text" ]
    where
          sht = ShowHelpText Nothing

version :: Parser (a -> a)
version = infoOption versionCopy ( long "version" S.<>
                                   short 'v'      S.<>
                                   help "show version and copyright info" )
    where
          versionCopy = "\nlentil - frugal issue tracker, version 1.5.10.0\n\
                        \(C) 2015-2024 Francesco Ariis - http://www.ariis.it\n\
                        \released under the GNU General Public License v3\n"


runLentil :: LOptions -> IO ()
runLentil lo = uncurry (findIssues (loAlias lo) (loFlagwords lo))
                       (loInExcl lo) >>= \is ->            -- grab issues
               let fil = filterAnd (loFilters lo) is       -- filtered
                   -- ord = chainSorts fil (loSort lo)     -- ordered

                   -- which function to use to output stuff (file/term)
                   outFunction :: String -> IO ()
                   outFunction = case loOutFile lo of
                                   Nothing -> putStrLn
                                   Just fp -> writeFile fp

                   -- col or not?
                   bCol :: Bool
                   bCol = if loOutFile lo == Nothing
                            then True
                            else False

               in

               case loFormat lo of
                 Pretty -> outFunction (ppIssues bCol fil)
                 TagPop -> outFunction (ppPopularity bCol fil)
                 File   -> outFunction (issues2File fil)
                 Csv    -> outFunction (issues2CSV fil)
                 Comp   -> outFunction (issues2Compiler fil)
                 Xml    -> outFunction (issues2Xml fil)
