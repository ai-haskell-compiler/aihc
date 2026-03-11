module Main (main) where

import Control.Monad.Fix
import Control.Monad.Catch
import Data.ByteString.Lazy as BSL
import GHC.IO.Encoding (setLocaleEncoding)
import System.FilePath
import System.IO
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Data.Elf
import Data.Elf.PrettyPrint

import DummyLd
import AsmAArch64
import HelloWorld
import ForwardLabel

helloWorldExe :: MonadCatch m => m Elf
helloWorldExe = assemble helloWorld >>= dummyLd

forwardLabelExe :: (MonadCatch m, MonadFix m) => m Elf
forwardLabelExe = assemble forwardLabel >>= dummyLd

helloWorldObj :: MonadCatch m => m Elf
helloWorldObj = assemble helloWorld

fixTargetName :: String -> String
fixTargetName = fmap f
    where
        f '.' = '_'
        f x   = x

writeElf :: FilePath -> Elf -> IO ()
writeElf path elf = do
    e <- serializeElf elf
    BSL.writeFile path e

testElf :: String -> IO Elf -> [ TestTree ]
testElf elfFileName elf =
    [ testCase makeTargetName (elf >>= writeElf f)
    , after AllSucceed makeTargetName $ testGroup checkTargetName
        [ goldenVsFile "dump"   (d <.> "golden") d (writeElfDump   f d)
        , goldenVsFile "layout" (l <.> "golden") l (writeElfLayout f l)
        ]
    ]
    where
        makeTargetName  = "make_"  ++ fixTargetName elfFileName
        checkTargetName = "check_" ++ fixTargetName elfFileName
        t = "examples"
        f = t </> elfFileName
        d = t </> elfFileName <.> "dump"
        l = t </> elfFileName <.> "layout"

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $ testGroup "examples"
        (  testElf "helloWorldObj.o" helloWorldObj
        ++ testElf "helloWorldExe"   helloWorldExe
        ++ testElf "forwardLabelExe" forwardLabelExe
        )
