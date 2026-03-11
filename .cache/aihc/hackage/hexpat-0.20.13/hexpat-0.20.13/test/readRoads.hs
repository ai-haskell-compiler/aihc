{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Expat.Tree
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Maybe


-- Reads the contents of ROADS.xml from stdin
main :: IO ()
main = do
    bs <- L.getContents
    let Element _ _ chs = parseThrowing defaultParseOptions bs :: UNode Text
    forM_ chs $ \ch -> do
        case ch of
            elt@(Element "shape" _ _) -> do
                putStrLn $ T.unpack $ fromMaybe "" $ getAttribute elt "FULL_NAME"
            _ -> return ()
