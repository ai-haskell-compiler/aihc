module Main where

import qualified Server.Option as Option
import qualified Shell.Utility.Verbosity as Verbosity

import qualified Game.ZeilenSpalten.HTML as ZeilenSpalten
import qualified Game.VierGewinnt.HTML as VierGewinnt
import qualified Game.Mastermind.HTML as Mastermind

import qualified Network.Shed.Httpd as HTTPd
import Network.URI (uriQuery, uriPath, )

import Text.Html((<<), (+++), )
import qualified Text.Html as Html


headers :: [(String, String)]
headers =
   [("Content-Type", "text/html; charset=latin1")]

withQuery :: String -> (Maybe String -> IO HTTPd.Response) -> IO HTTPd.Response
withQuery query action =
   case query of
      '?':str -> print str >> (action $ Just str)
      [] -> action $ Nothing
      _ -> return $ HTTPd.Response 400 [] $ "invalid query: " ++ query

main :: IO ()
main = do
  opt <- Option.get
  HTTPd.initServer (Option.port opt) $ \ req -> do
    -- FixMe: should check for HTTP method here
    Option.printVerbose opt Verbosity.verbose req
    let uri = HTTPd.reqURI req
    Option.printVerbose opt Verbosity.deafening $ uriQuery uri
    Option.printVerbose opt Verbosity.deafening $
       HTTPd.queryToArguments $ uriQuery uri
    case uriPath uri of
       "/" ->
          return $ HTTPd.Response 200 headers $
             Html.renderHtml $
             Html.header (Html.thetitle << "Web games") +++
             Html.body (Html.unordList (map
                (\(path,name) -> (Html.anchor << name) Html.! [Html.href path]) $
                   ("VierGewinnt", "Vier gewinnt") :
                   ("ZeilenSpalten", "Zeilen und Spalten") :
                   ("Mastermind", "Mastermind") :
                   []))
       "/VierGewinnt" ->
          withQuery (uriQuery uri) $ \query ->
          return $ HTTPd.Response 200 headers $ Html.renderHtml $
             VierGewinnt.komplett $ VierGewinnt.erzeuge query
       "/ZeilenSpalten" ->
          withQuery (uriQuery uri) $ \query ->
          fmap (HTTPd.Response 200 headers . Html.renderHtml .
             ZeilenSpalten.komplett) $ ZeilenSpalten.erzeuge query
       "/Mastermind" ->
          withQuery (uriQuery uri) $ \query ->
          fmap (HTTPd.Response 200 headers . Html.renderHtml .
             Mastermind.complete) $ Mastermind.generate query
       path ->
          return $ HTTPd.Response 404 [] $ "unknown game: " ++ path
  return ()
