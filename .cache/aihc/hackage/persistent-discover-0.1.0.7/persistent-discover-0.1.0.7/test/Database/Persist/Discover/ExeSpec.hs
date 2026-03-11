module Database.Persist.Discover.ExeSpec where

import Test.Hspec
import Database.Persist.Discover.Exe
import System.FilePath

spec :: Spec
spec = do
    describe "renderFile" do
        it "should have TemplateHaskell pragma" do
            let rendered  =
                    renderFile AllModelsFile
                        { amfModuleBase =
                            Module
                                { moduleName = "Asdf"
                                , modulePath = "src/Asdf.hs"
                                }
                        , amfModuleImports =
                            []
                        }
                firstLines =
                    unlines . take 4 . lines $ rendered
                expected =
                    render do
                        "{-# LINE 1 \"Asdf\" #-}"
                        "{-# LANGUAGE TemplateHaskell #-}"
                        ""
                        "module Asdf where"
            firstLines `shouldBe` expected

    describe "Render" do
        it "works" do
            shouldBe
                do render do
                    "hello"
                    "world"
                do
                    "hello\nworld\n"
        describe "indent" do
            it "works" do
                shouldBe
                    do render do
                        "hello"
                        indent 4 do
                            "world"
                        "goodbye"
                    do
                        "hello\n    world\ngoodbye\n"

    describe "mkModulePieces" do
        let
            exPath =
                "src/Foo/Bar.hs"
        it "works" do
            mkModulePieces exPath
                `shouldBe`
                    ["Foo", "Bar"]

        it "does not eat non-hs files" do
            let
                mdFile =
                    "src/Foo/README.md"
            mkModulePieces mdFile
                `shouldBe`
                    []

    describe "pathToModule" do
        let
            exPath =
                "./src/Foo/Bar.hs"
            (dir, file) =
                splitFileName exPath
        it "splits how i expect" $ do
            dir `shouldBe` "./src/Foo/"
            file `shouldBe` "Bar.hs"
        it "works" do
            pathToModule exPath
                `shouldBe`
                    Just Module
                        { moduleName =
                            "Foo.Bar"
                        , modulePath =
                            exPath
                        }
        it "pathToModule absolute path" do
            let absPath = "/Users/user/Code/project/src/Foo/Bar.hs"
            pathToModule absPath
                `shouldBe`
                    Just Module
                        { moduleName =
                            "Foo.Bar"
                        , modulePath =
                            absPath
                        }
