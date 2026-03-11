{-# language OverloadedStrings #-}
module Hinfo 
  (
    app
  , execute
  ) where

import Options.Applicative                (execParser)
import Hinfo.Hpack
import Hinfo.Options
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as T

app :: IO ()
app = do
  options <- execParser appOpts
  packageFile <- loadDefault
  execute options packageFile


execute :: AppOption -> PackageFile -> IO ()
execute Name              pf = T.putStrLn $ packageName pf
execute Version           pf = T.putStrLn $ packageVersion pf
execute Github            pf = maybe (die "Missing Github") T.putStrLn (packageGithub pf)
execute Author            pf = maybe (die "Missing Author") T.putStrLn (packageAuthor pf)
execute Maintainer        pf = maybe (die "Missing Maintainer") T.putStrLn (packageMaintainer pf)
execute Copyright         pf = maybe (die "Mising Copyright") T.putStrLn (packageCopyright pf)
execute ExtraSourceFiles  pf = maybe (die "Missing Extra Source Files") (T.putStrLn . T.intercalate "\n") (packageExtraSourceFiles pf)
execute Synopsis          pf = maybe (die "Missing Synopsis") T.putStrLn (packageSynopsis pf)
execute Category          pf = maybe (die "Missing Category") T.putStrLn (packageCategory pf)
execute Description       pf = maybe (die "Missing Description") T.putStrLn $ packageDescription pf
execute Dependencies      pf = T.putStrLn $ T.intercalate ", " $ packageDependencies pf
