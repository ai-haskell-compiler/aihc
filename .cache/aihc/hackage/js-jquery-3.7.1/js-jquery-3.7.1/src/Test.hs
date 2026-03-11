
module Main(main) where

import Paths_js_jquery
import qualified Language.Javascript.JQuery as JQuery
import Data.Version
import Network.HTTP


main :: IO ()
main = do
    length (versionBranch JQuery.version) === 3
    show JQuery.version === show version{versionBranch = take 3 $ versionBranch version}
    a <- wget JQuery.url
    b <- readFile =<< JQuery.file
    a === b
    putStrLn "\nSuccess"


wget url = do
    res <- simpleHTTP (getRequest $ "http:" ++ url)
    case res of
        Left err -> error $ show err
        Right r | rspCode r /= (2,0,0) -> error $
                    "Incorrect code: " ++ show (rspCode r,rspReason r,url) ++ "\n" ++ rspBody r
                | otherwise -> pure $ rspBody r


a === b | a == b = putChar '.'
        | otherwise = error $ show (a,b)
