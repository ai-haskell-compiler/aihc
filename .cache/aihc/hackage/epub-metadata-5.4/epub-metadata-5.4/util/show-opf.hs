#! /usr/bin/runhaskell -isrc

import Control.Monad.Error ( runErrorT )
import System.Environment ( getArgs )

import Codec.Epub.Opf.Parse ( parseXmlToOpf )


main :: IO ()
main = do
   opfXmlContents <- (fmap head) getArgs >>= readFile
   parseResult <- runErrorT $ parseXmlToOpf opfXmlContents
   either print print parseResult
