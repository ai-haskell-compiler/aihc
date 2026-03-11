import System.Environment
import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.DOM.ShowXml

import Text.XML.HXT.CSS

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path, sel] -> run path sel
        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ prog ++ " PATH CSS_SELECTOR"

run :: FilePath -> String -> IO ()
run path sel = do
    let doc = readDocument [withParseHTML yes, withWarnings no] path
    elts <- runX $ doc >>> css sel >>> processChildren none
    mapM_ (\elt -> putStrLn (xshow [elt])) elts
    putStrLn $ "\nFound " ++ show (length elts) ++ " elemement(s)"
