module Main where

import qualified Test.Data.EventList.Absolute.BodyEnd as AbsBodyEnd
import qualified Test.Data.EventList.Absolute.TimeEnd as AbsTimeEnd
import qualified Test.Data.EventList.Relative.BodyEnd as RelBodyEnd
import qualified Test.Data.EventList.Relative.TimeEnd as RelTimeEnd

import qualified System.IO as IO


prefix :: String -> [(String, IO ())] -> [(String, IO ())]
prefix msg =
   map (\(str,test) -> (msg ++ "." ++ str, test))

main :: IO ()
main =
   IO.hSetBuffering IO.stdout IO.NoBuffering >>
   (mapM_ (\(msg,io) -> putStr (msg++": ") >> io) $
    concat $
       prefix "Absolute.BodyEnd" AbsBodyEnd.tests :
       prefix "Absolute.TimeEnd" AbsTimeEnd.tests :
       prefix "Relative.BodyEnd" RelBodyEnd.tests :
       prefix "Relative.TimeEnd" RelTimeEnd.tests :
       [])
