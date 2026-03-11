{-# LANGUAGE CPP #-}

module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (redact:test)
import qualified Redact.Markdown.Test
#if __GLASGOW_HASKELL__ >= 808
import qualified Redact.Markdown.Mock
import qualified Redact.Monad.Terminal.Mock
#endif

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ Redact.Markdown.Test.tests
#if __GLASGOW_HASKELL__ >= 808
    , Redact.Markdown.Mock.tests
    , Redact.Monad.Terminal.Mock.tests
#endif
    ]
