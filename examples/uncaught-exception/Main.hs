module Main where

import Control.Exception (Exception, throwIO)

data GeneralException = GeneralException

instance Exception GeneralException

main :: IO ()
main = throwIO GeneralException
