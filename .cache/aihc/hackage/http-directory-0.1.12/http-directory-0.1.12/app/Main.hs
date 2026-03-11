import qualified Data.Text.IO as T
import Network.HTTP.Directory
import SimpleCmdArgs (simpleCmdArgs, strArg)

main :: IO ()
main =
  simpleCmdArgs Nothing "http-directory"
  "Simple http directory lister" $
  run
  <$> strArg "URL"
  where
    run url =
      httpDirectory' url >>= mapM_ T.putStrLn
