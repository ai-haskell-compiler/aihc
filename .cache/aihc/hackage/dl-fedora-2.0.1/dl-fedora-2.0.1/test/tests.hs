import Control.Monad (forM_, unless)
import Data.List (sort)
import Data.Maybe
import Distribution.Fedora.Release (getCurrentFedoraVersion, getRawhideVersion)
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)
import SimpleCmd

dlFedora :: Bool -> [String] -> IO ()
dlFedora ghAction args =
  putStrLn "" >> cmdLog "dl-fedora"
  ((if ghAction then ("--dl" :) . ("-T" :) else id) args)

tests :: Bool -> Natural -> Natural -> [[String]]
tests ghAction rawhide oldest =
  let latest = show $ rawhide - 1
      previous = show $ rawhide - 2
  in
  [["-n", previous, "--checksum"]
  ,["-n", "rawhide", "silverblue"]
  ,["-c", "respin"]
  ,["-l", latest]
  ,["-l", "rawhide", "-n"]
  ,["-n", previous, "kde"]
  ] ++
  if ghAction then []
  else
    [["-c", latest, "silverblue"]
    ,["-T", "-n", latest, "everything"]
    ,["-n", previous, "server", "--dvd", "--arch", "aarch64"]
    ,["41", "iot", "-n"]
    ,["-n", "c8s"]
    ,["-n", "c9s", "--dvd"]
    ,["-n", "c10s"]
    ,["-c", "eln"]
    ,["-n", "c9s-live"]
    ,["-n", "c10s-live"]
    ]
    ++
    allFedoraEditions oldest rawhide
    ++
    [["--dl", "-T", "-n", "respin", "--all-desktops"]
    ,["-T", "-n", "c9s-live", "--all-desktops"]
    ,["-T", "-n", "c10s-live", "--all-editions"]
    ]

mkFedEdition :: String -> [String]
mkFedEdition ver =
  ["--dl", "-T", "-n", ver, "--all-editions"]

allFedoraEditions :: Natural -> Natural -> [[String]]
allFedoraEditions oldest rawhide =
  map (mkFedEdition . show) [oldest..(rawhide-1)]
  ++
  [mkFedEdition "stage"]
  ++
  -- currently MATE is broken in Rawhide
  [["--dl", "-T", "-n", show rawhide, "--exclude", "mate"]]

allEditions :: Natural -> [String]
allEditions 9 = allEditions 10 ++ ["cinnamon", "mate", "xfce"]
allEditions 10 = ["workstation", "kde", "max", "min"]
allEditions rel = ["cloud","container","everything","server","workstation","budgie","cinnamon","i3","kde","lxde","lxqt","mate","soas","sway","xfce","silverblue","kinoite","onyx","sericea"] ++
  ["cosmic" | rel >= 42] ++ ["kdemobile" | rel >= 41] ++ ["miracle" | rel >= 41] ++ ["iot" | rel < 42]

listEditions :: Natural -> IO ()
listEditions n = do
  let args = ["--list", show n ++ if n < 11 then "-live" else ""]
  cmdN "dl-fedora" args
  es <- sort . words <$> cmd "dl-fedora" args
  let aes = sort $ allEditions n
  unless (es == aes) $
    error' $ show n +-+ "editions unmatched:\n" ++ show es ++ "\n" ++ show aes

main :: IO ()
main = do
  ghAction <- isJust <$> lookupEnv "GITHUB_ACTIONS"
  current <- getCurrentFedoraVersion
  rawhide <- getRawhideVersion
  let oldest = current -1
  unless ghAction $ do
    forM_ [oldest..rawhide] listEditions
    forM_ [9..10] listEditions
  let cases = tests ghAction rawhide oldest
  mapM_ (dlFedora ghAction) cases
  putStrLn $ show (length cases) ++ " tests ran"
