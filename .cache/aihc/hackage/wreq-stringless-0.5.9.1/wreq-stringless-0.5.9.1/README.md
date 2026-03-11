# wreq-stringless: Simple wrapper to use wreq without Strings

This Haskell library wraps [Network.Wreq](http://hackage.haskell.org/package/wreq) to use the functions without Strings.

You can use:

- Data.Text
- Data.Text.Lazy
- Data.ByteString.UTF8
- Data.ByteString.Layz.UTF8

for all functions from [Network.Wreq](http://hackage.haskell.org/package/wreq) where the original implementation use Strings.



## Example:

    {-# LANGUAGE OverloadedStrings #-}
    module Main where

    import qualified Data.Text               as T
    import           Lens.Micro.Extras       (view)
    import qualified Network.Wreq            as Wreq
    import qualified Network.Wreq.StringLess as Wreq'


    -- say we use a alternate prelude like 'Protolude', and we don't use Strings
    -- or we have the url from somewhere else, and it's not a String.
    url :: T.Text
    url = "http://httpbin.org"


    main :: IO ()
    main = do
      -- with plain wreq
      view Wreq.responseStatus  <$> Wreq.get (T.unpack url) >>= print

      -- with wreq-stringless - no manual conversion necessary
      view Wreq'.responseStatus <$> Wreq'.get url >>= print



## How to use it:

* replace your **wreq** dependency with **wreq-stringless**
* replace the import **Network.Wreq** with **Network.Wreq.StringLess**
