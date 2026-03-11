import Network.CGI (CGI, CGIResult, runCGI, liftIO, output, handleErrors)

cgiMain :: CGI CGIResult
cgiMain = liftIO (readFile "foo") >> output "bar"

main :: IO ()
main = runCGI (handleErrors cgiMain)
