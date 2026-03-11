module BM.Test (tests) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as V

-- (bm)
import qualified BM

------------------------------------------------------------------------------

config :: BM.Config
config = BM.Config
    { BM.configCommand = "xdg-open"
    , BM.configArgs = V.fromList [ddg, google, nix, postgres, python, tst]
    }
  where
    ddg = BM.Bookmark
      { BM.keyword     = "ddg"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://duckduckgo.com"
      , BM.queryOrArgs = Left $ BM.Query
          { BM.action           = "https://duckduckgo.com/"
          , BM.parameter        = "q"
          , BM.hiddenParameters = V.fromList
              [ BM.Parameter
                  { BM.name  = "ia"
                  , BM.value = "web"
                  }
              ]
          }
      }

    google = BM.Bookmark
      { BM.keyword     = "google"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Nothing
      , BM.queryOrArgs = Left $ BM.Query
          { BM.action           = "https://www.google.com/search"
          , BM.parameter        = "q"
          , BM.hiddenParameters = V.empty
          }
      }

    nix = BM.Bookmark
      { BM.keyword     = "nix"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://nixos.org/"
      , BM.queryOrArgs = Right $ V.fromList [nixManual, nixOS, nixPkgs]
      }
    nixManual = BM.Bookmark
      { BM.keyword     = "manual"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://nixos.org/manual/nix/stable"
      , BM.queryOrArgs = Right V.empty
      }
    nixOS = BM.Bookmark
      { BM.keyword     = "OS"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://nixos.org/manual/nixos/stable"
      , BM.queryOrArgs = Right V.empty
      }
    nixPkgs = BM.Bookmark
      { BM.keyword     = "pkgs"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://github.com/NixOS/nixpkgs"
      , BM.queryOrArgs = Right $ V.fromList [nixPkgsManual]
      }
    nixPkgsManual = BM.Bookmark
      { BM.keyword     = "manual"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://nixos.org/manual/nixpkgs/stable"
      , BM.queryOrArgs = Right V.empty
      }

    postgres = BM.Bookmark
      { BM.keyword     = "postgres"
      , BM.mCommand    = Nothing
      , BM.mUrl = Just "https://www.postgresql.org/docs/current/index.html"
      , BM.queryOrArgs = Right V.empty
      }

    python = BM.Bookmark
      { BM.keyword     = "python"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "https://docs.python.org/3/"
      , BM.queryOrArgs = Right V.empty
      }

    tst = BM.Bookmark
      { BM.keyword     = "tst"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Nothing
      , BM.queryOrArgs = Right $ V.fromList [tstAUrl, tstCommand, tstNoUrl]
      }
    tstAUrl = BM.Bookmark
      { BM.keyword     = "aurl"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "http://localhost:8001"
      , BM.queryOrArgs = Right V.empty
      }
    tstCommand = BM.Bookmark
      { BM.keyword     = "cmd"
      , BM.mCommand    = Just "firefox"
      , BM.mUrl        = Just "http://localhost:8002"
      , BM.queryOrArgs = Right $ V.fromList [tstCommandChild]
      }
    tstCommandChild = BM.Bookmark
      { BM.keyword     = "child"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Just "http://localhost:8003"
      , BM.queryOrArgs = Right V.empty
      }
    tstNoUrl = BM.Bookmark
      { BM.keyword     = "nourl"
      , BM.mCommand    = Nothing
      , BM.mUrl        = Nothing
      , BM.queryOrArgs = Right V.empty
      }

------------------------------------------------------------------------------

testRun :: TestTree
testRun = testGroup "run"
    [ testCase "exact" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://nixos.org/"]
            }
        , [ "[xdg-open]"
          , "<nix>"
          , "xdg-open https://nixos.org/"
          ]
        ) @=? BM.run config ["nix"]
    , testCase "prefix" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://nixos.org/"]
            }
        , [ "[xdg-open]"
          , "<nix>"
          , "xdg-open https://nixos.org/"
          ]
        ) @=? BM.run config ["n"]
    , testCase "subExact" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://nixos.org/manual/nixpkgs/stable"]
            }
        , [ "[xdg-open]"
          , "<nix>"
          , "<pkgs>"
          , "<manual>"
          , "xdg-open https://nixos.org/manual/nixpkgs/stable"
          ]
        ) @=? BM.run config ["nix", "pkgs", "manual"]
    , testCase "subPrefix" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://nixos.org/manual/nixpkgs/stable"]
            }
        , [ "[xdg-open]"
          , "<nix>"
          , "<pkgs>"
          , "<manual>"
          , "xdg-open https://nixos.org/manual/nixpkgs/stable"
          ]
        ) @=? BM.run config ["n", "p", "m"]
    , testCase "first" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments =
                ["https://www.postgresql.org/docs/current/index.html"]
            }
        , [ "[xdg-open]"
          , "<postgres>"
          , "xdg-open https://www.postgresql.org/docs/current/index.html"
          ]
        ) @=? BM.run config ["p"]
    , testCase "queryUrlNoArgs" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://duckduckgo.com"]
            }
        , [ "[xdg-open]"
          , "<ddg>"
          , "xdg-open https://duckduckgo.com"
          ]
        ) @=? BM.run config ["d"]
    , testCase "queryUrlArgs" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://duckduckgo.com/?q=haskell+nix&ia=web"]
            }
        , [ "[xdg-open]"
          , "<ddg>"
          , "xdg-open https://duckduckgo.com/?q=haskell+nix&ia=web"
          ]
        ) @=? BM.run config ["d", "haskell", "nix"]
    , testCase "queryNoUrlArgs" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["https://www.google.com/search?q=haskell+nix"]
            }
        , [ "[xdg-open]"
          , "<google>"
          , "xdg-open https://www.google.com/search?q=haskell+nix"
          ]
        ) @=? BM.run config ["goo", "haskell", "nix"]
    , testCase "queryNoUrlNoArgs" $
        ( Left "no query for google"
        , [ "[xdg-open]"
          , "<google>"
          ]
        ) @=? BM.run config ["goo"]
    , testCase "noUrlSub" $
        ( Right $ BM.Proc
            { BM.command   = "xdg-open"
            , BM.arguments = ["http://localhost:8001"]
            }
        , [ "[xdg-open]"
          , "<tst>"
          , "<aurl>"
          , "xdg-open http://localhost:8001"
          ]
        ) @=? BM.run config ["tst"]
    , testCase "noUrlError" $
        ( Left "no URL for nourl"
        , [ "[xdg-open]"
          , "<tst>"
          , "<nourl>"
          ]
        ) @=? BM.run config ["tst", "nourl"]
    , testCase "unknownError" $
        ( Left "unknown argument: unknown"
        , [ "[xdg-open]"
          , "<tst>"
          ]
        ) @=? BM.run config ["tst", "unknown"]
    , testCase "noArgsError" $
        ( Left "no arguments"
        , [ "[xdg-open]"
          ]
        ) @=? BM.run config []
    , testCase "command" $
        ( Right $ BM.Proc
            { BM.command   = "firefox"
            , BM.arguments = ["http://localhost:8002"]
            }
        , [ "[xdg-open]"
          , "<tst>"
          , "<cmd> [firefox]"
          , "firefox http://localhost:8002"
          ]
        ) @=? BM.run config ["tst", "cmd"]
    , testCase "commandChild" $
        ( Right $ BM.Proc
            { BM.command   = "firefox"
            , BM.arguments = ["http://localhost:8003"]
            }
        , [ "[xdg-open]"
          , "<tst>"
          , "<cmd> [firefox]"
          , "<child>"
          , "firefox http://localhost:8003"
          ]
        ) @=? BM.run config ["tst", "cmd", "child"]
    ]

------------------------------------------------------------------------------

testGetCompletion :: TestTree
testGetCompletion = testGroup "getCompletion"
    [ testCase "all" $
        [ "ddg", "google", "nix", "postgres", "python", "tst"
        ] @=? BM.getCompletion config [""]
    , testCase "exact" $
        ["nix"] @=? BM.getCompletion config ["nix"]
    , testCase "subExact" $
        ["pkgs"] @=? BM.getCompletion config ["nix", "pkgs"]
    , testCase "prefix" $
        ["postgres", "python"] @=? BM.getCompletion config ["p"]
    , testCase "none" $
        [] @=? BM.getCompletion config ["google", "maps"]
    , testCase "noneFirst" $
        [] @=? BM.getCompletion config ["j", "e"]
    , testCase "empty" $
        [] @=? BM.getCompletion config []
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "BM"
    [ testRun
    , testGetCompletion
    ]
