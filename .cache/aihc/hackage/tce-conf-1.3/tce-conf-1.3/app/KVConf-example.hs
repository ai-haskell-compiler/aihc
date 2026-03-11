import Data.Map ( lookup )
import Prelude hiding ( lookup )
import TCE.Data.KVConf ( parseToMap )

main = do
   -- Parse a String into a KVConf datatype
   conf <- parseToMap <$> readFile "resources/kv-example.conf"

   -- It's just a map, so use Data.Map functions to access
   print $ lookup "foo" conf
   print $ lookup "baz-blorp" conf
