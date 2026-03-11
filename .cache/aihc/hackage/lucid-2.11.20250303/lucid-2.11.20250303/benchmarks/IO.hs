{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lucid
import Criterion.Main
import Control.Applicative (Applicative)
import Control.Monad (replicateM_)
import qualified Data.Text.Lazy as LT
import Control.Monad.Trans.Reader (runReader)
import Data.Functor.Identity (runIdentity)

lotsOfDivs :: (Applicative m, Monad m) => Int -> HtmlT m ()
lotsOfDivs n = body_
  $ replicateM_ n
  $ div_ "hello world!"

main :: IO ()
main = defaultMain
    [ bench "renderText"            $ nf (renderText  . lotsOfDivs) size
    , bench "renderTextT Identity"  $ nf (runIdentity . renderTextT . lotsOfDivs) size
    , bench "renderTextT Reader"    $ nf (\(r, s) -> flip runReader r . renderTextT . lotsOfDivs $ s) ((), size)
    , bench "renderTextT IO"        $ nfIO (renderTextT (lotsOfDivs size) :: IO LT.Text)
    ]
  where
    size = 10000
