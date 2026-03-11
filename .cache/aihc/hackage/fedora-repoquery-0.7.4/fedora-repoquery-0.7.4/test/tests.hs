import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Control.Monad (unless)
import SimpleCmd

fdrq :: ([String],Maybe String) -> IO ()
fdrq (args,mpkg) = do
  putStrLn ""
  putStrLn $ "# " ++ unwords args +-+ fromMaybe "" mpkg
  let debug = False
  (ok, out, err) <- cmdFull "fedora-repoquery" (["-d" | debug] ++ args ++ maybeToList mpkg) ""
  if null err
    then unless (isNothing mpkg || "-T" `notElem` args) $ putStrLn "stderr empty"
    else putStrLn err
  if null out
    then unless (isNothing mpkg) $ error' "no output"
    else putStrLn out
  unless ok $ error' "failed"

-- copied from dl-fedora
branched :: Int
branched = 42
current, previous, prevprev, rawhide :: String
current = show branched
previous = show (branched - 1)
prevprev = show (branched - 2)
rawhide = show (branched + 1)

tests :: [([String],Maybe String)]
tests =
  [(["rawhide"], Nothing)
  ,(["-D","rawhide"], Nothing)
  ,([current], Nothing)
  ,([previous], Nothing)
  ,(["eln"], Nothing)
  ,(["epel9"], Nothing)
  ,(["c10s"], Nothing)
  ,(["-T", "rawhide"], Just "coreutils")
  ,(["--dynamic", rawhide], Just "coreutils")
  ,(["-T", current], Just "gtk4")
  ,([previous], Just "bash")
  ,([prevprev], Just "fontconfig")
  ,(["-t", previous], Just "podman")
  ,(["eln"], Just "ibus")
  ,(["epel9"], Just "ghc")
  ,(["epel10"], Just "ghc")
  ,(["epel10.0"], Just "ghc-srpm-macros")
  ,(["c10"], Just "bash")
  ,(["c9"], Just "kernel")
  ,(["rawhide", "--whatprovides"], Just "perl(Regexp::Pattern::License)")
  ]

main :: IO ()
main = do
  mapM_ fdrq tests
  putStrLn $ show (length tests) ++ " tests ran"
