-- Accepts file uploads and saves the files in the given directory.
-- WARNING: this script is a SECURITY RISK and only for
-- demo purposes. Do not put it on a public web server.

import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy as BS (writeFile)
import Network.CGI
import Text.XHtml
   ( Html, paragraph, (!), href, (+++), form, method, enctype, afile, submit
   , renderHtml, header, thetitle, body, (<<), anchor
   )

dir :: String
dir = "../upload"

saveFile :: (MonadCGI m, MonadIO m) => String -> m Html
saveFile n =
    do cont <- fromJust <$> getInputFPS "file"
       let p = dir ++ "/" ++ basename n
       liftIO $ BS.writeFile p cont
       return $ paragraph << ("Saved as " +++ anchor ! [href p] << p +++ ".")

fileForm :: Html
fileForm = form ! [method "post", enctype "multipart/form-data"]
             << [afile "file", submit "" "Upload"]


basename :: String -> String
basename = reverse . takeWhile (`notElem` "/\\") . reverse

cgiMain :: CGI CGIResult
cgiMain =
    do mn <- getInputFilename "file"
       h <- maybe (return fileForm) saveFile mn
       output $ renderHtml $ header << thetitle << "Upload example"
                               +++ body << h

main :: IO ()
main = runCGI cgiMain
