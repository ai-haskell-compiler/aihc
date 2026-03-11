import SimpleCmd
import System.IO

program :: [String] -> IO ()
program test = do
  timeIO $ cmdLog "pkgtreediff" test
  putStrLn ""

tests :: [[String]]
tests =
  [
    ["-r", "test/files/35", "test/files/36"]
  -- FIXME until native repoquerying use dnf-repo
  , ["--ignore-release",
     "dnf-repo -q -d * -u http://download.fedoraproject.org/pub/fedora/linux/releases/35/Everything/x86_64/os/ repoquery --qf %{name}-%{version}-%{release}.%{arch}",
     "dnf-repo -q -d * -u http://download.fedoraproject.org/pub/fedora/linux/releases/36/Everything/x86_64/os/ repoquery --qf %{name}-%{version}-%{release}.%{arch}"]
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program tests
  putStrLn $ "\n" ++ show (length tests) ++ " command tests run"
