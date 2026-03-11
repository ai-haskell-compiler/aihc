{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : Main
Copyright   : © 2020-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the `doclayout` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import HsLua (Lua)
import HsLua.Module.DocLayout (documentedModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

import qualified HsLua as Lua

main :: IO ()
main = do
  luaTestResults <- Lua.run @Lua.Exception $ do
    Lua.openlibs
    Lua.registerModule documentedModule
    Lua.pop 1 -- pop module table
    translateResultsFromFile "test/test-doclayout.lua"
  defaultMain $ testGroup "hslua-module-doclayout" [tests, luaTestResults]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua doclayout module"
  [ testCase "module can be pushed to the stack" $
      Lua.run (void (Lua.pushModule documentedModule) :: Lua ())

  , testCase "module can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      Lua.preloadModule documentedModule
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        Lua.getglobal' "package.preload.doclayout"
        Lua.ltype (-1)

  , testCase "module can be loaded as `layout`" . Lua.run $ do
      Lua.openlibs
      Lua.preloadModuleWithName documentedModule "layout"
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'layout'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
