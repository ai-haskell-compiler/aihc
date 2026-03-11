{-# LANGUAGE QuasiQuotes #-}

module GeneratorSpec where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import TestImport

type UserId = Int
type NonEmptyUserId = NonEmpty UserId

resources :: [ResourceTree String]
resources = [parseRoutes|
  /api ApiP:
    /users/#UserId UsersR GET
    /users/#Int/foo UsersFooR GET
    /users/#NonEmptyUserId/bar UsersBarR GET
    /users/#Other/baz UsersBazR GET
|]

spec :: Spec
spec = do
  describe "genFlowClasses" $ do
    it "should generate classes" $ do
      let classes = genFlowClasses Map.empty [] [] resources
      map className classes
        `shouldBe`
          [ "PATHS_TYPE_paths"
          , "PATHS_TYPE_paths_static_pages"
          , "PATHS_TYPE_paths_api"
          ]

    it "should identify path variable types" $ do
      let
        classes = genFlowClasses Map.empty [] [] resources
        apiClass = find ((== "PATHS_TYPE_paths_api") . className) classes
      map classMembers apiClass
        `shouldBe`
          Just
            [ Method
              "users"
              [ Path "api"
              , Path "users"
              , Dyn NumberT
              ]
            , Method
              "users_bar"
              [ Path "api"
              , Path "users"
              , Dyn (NonEmptyT NumberT)
              , Path "bar"
              ]
            , Method
              "users_baz"
              [ Path "api"
              , Path "users"
              , Dyn StringT
              , Path "baz"
              ]
            , Method
              "users_foo"
              [ Path "api"
              , Path "users"
              , Dyn NumberT
              , Path "foo"
              ]
            ]
    it "should respect overrides " $ do
      let
        classes = genFlowClasses (Map.fromList [("UserId", StringT)]) [] [] resources
        apiClass = find ((== "PATHS_TYPE_paths_api") . className) classes
      map classMembers apiClass
        `shouldBe`
          Just
            [ Method
              "users"
              [ Path "api"
              , Path "users"
              , Dyn StringT
              ]
            , Method
              "users_bar"
              [ Path "api"
              , Path "users"
              , Dyn (NonEmptyT StringT)
              , Path "bar"
              ]
            , Method
              "users_baz"
              [ Path "api"
              , Path "users"
              , Dyn StringT
              , Path "baz"
              ]
            , Method
              "users_foo"
              [ Path "api"
              , Path "users"
              , Dyn NumberT
              , Path "foo"
              ]
            ]
  describe "classesToFlow" $ do
    it "should generate formal parameters for each path variable" $ do
      let
        cls =
          [ Class
            "x"
            [ Method
              "y"
              [ Path "x"
              , Dyn StringT
              , Path "y"
              , Dyn NumberT
              , Path "z"
              , Dyn StringT
              ]
            ]
          ]
      normalizeText (classesToFlow cls)
        `shouldBe`
          normalizeText [st|
            class x {
              y(a: string, aa: number, aaa: string): string { return this.root + '/x/' + a + '/y/' + aa.toString() + '/z/' + aaa + ''; }
              root: string;
              constructor(root: string) {
                this.root = root;
              }
            }
          |]
    it "should avoid name shadowing in nested binders" $ do
      let
        cls =
          [ Class
            "x"
            [ Method
              "y"
              [ Path "x"
              , Dyn (NonEmptyT (NonEmptyT NumberT))
              , Path "y"
              ]
            ]
          ]
      normalizeText (classesToFlow cls)
        `shouldBe`
          normalizeText [st|
            class x {
              y(a: Array<Array<number>>): string { return this.root + '/x/' + a.map(function(a1) { return a1.map(function(a2) { return a2.toString() }).join(',') }).join(',') + '/y'; }
              root: string;
              constructor(root: string) {
                this.root = root;
              }
            }
          |]

  describe "genFlowSource" $
    it "should work end-to-end" $ do
      let source = genFlowSource Map.empty [] [] "'test'" resources
      normalizeText source
        `shouldBe`
          normalizeText [st|
            /* @flow */
            class PATHS_TYPE_paths {
              api : PATHS_TYPE_paths_api;
              static_pages : PATHS_TYPE_paths_static_pages;
              root: string;
              constructor(root: string) {
                this.root = root;
                this.api = new PATHS_TYPE_paths_api(root);
                this.static_pages = new PATHS_TYPE_paths_static_pages(root);
              }
            }
            class PATHS_TYPE_paths_static_pages {
              root: string;
              constructor(root: string) {
                this.root = root;
              }
            }
            class PATHS_TYPE_paths_api {
              users(a: number): string { return this.root + '/api/users/' + a.toString() + ''; }
              users_bar(a: Array<number>): string { return this.root + '/api/users/' + a.map(function(a1) { return a1.toString() }).join(',') + '/bar'; }
              users_baz(a: string): string { return this.root + '/api/users/' + a + '/baz'; }
              users_foo(a: number): string { return this.root + '/api/users/' + a.toString() + '/foo'; }
              root: string;
              constructor(root: string) {
                this.root = root;
              }
            }
            var PATHS: PATHS_TYPE_paths = new PATHS_TYPE_paths('test');
          |]
