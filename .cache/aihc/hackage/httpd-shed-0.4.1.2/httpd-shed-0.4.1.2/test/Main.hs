import Network.Shed.Httpd
import Network.URI

main = do 
  initServer 8091 $ \ req -> do
                     print req
                     print $ uriQuery (reqURI req)
                     print $ queryToArguments (uriQuery (reqURI req))
                     if uriPath (reqURI req) == "/"
                        then 
                          return $ Response 200 [] "<root dir>"
                        else do
                          str <- readFile ("." ++ uriPath (reqURI req))
                          return $ Response 200 [] str
  return ()
