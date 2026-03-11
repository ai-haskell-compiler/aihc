import Criterion.Main
import qualified System.Directory.Glob as GP      -- glob-posix
import qualified System.FilePath.Glob as G        -- Glob
import qualified System.Path.Glob as MH           -- MissingH

main :: IO ()
main = defaultMain [
        bgroup "glob-posix" $ cases (GP.glob GP.globDefaults),
        bgroup "MissingH" $ cases MH.glob,
        bgroup "Glob" $ cases G.glob
    ]
  where
      cases glob = [
              bench "Simple path"     . whnfIO $ glob "/usr/bin",
              bench "Python versions" . whnfIO $ glob "/usr/lib/python?.?",
              bench "Python site"     . whnfIO $ glob "/usr/lib/python?.?/site-packages/test"
          ]

