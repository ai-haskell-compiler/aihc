import TCE.Data.ReadConf ( readConfig )

-- Write your own custom data structure for config, like this:
data Config = Config
   { foo :: String
   , bar :: Int
   , baz :: [String]
   , qux :: Bool
   }
   deriving Read  -- Make it an instance of Read

main = do
   -- Parse a String containing a single instance of the above data type
   econf <- readConfig <$> readFile "resources/read-example.conf"

   -- The result is an Either String Config
   either
      print  -- Failure is reported as a Left
      (\c -> (print $ bar c) >> (print $ qux c))
      econf
