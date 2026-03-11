{-# LANGUAGE OverloadedStrings #-}

module Report where

import Data.String (fromString)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

import Trace.Hpc.Lcov.Report

test_report_serialization :: TestTree
test_report_serialization =
  goldenVsString "report serialization" "test/golden/report_serialization.golden" $
    pure . fromString . showReport $
      LcovReport
        [ FileReport
            { fileReportLocation = "src/MyModule/Foo.hs"
            , fileReportFunctions =
                [ FunctionReport
                    { functionReportLine = 1
                    , functionReportName = "foo1"
                    , functionReportHits = 0
                    }
                , FunctionReport
                    { functionReportLine = 5
                    , functionReportName = "foo2"
                    , functionReportHits = 1
                    }
                ]
            , fileReportBranches =
                [ BranchReport
                    { branchReportLine = 1
                    , branchReportHash = 100
                    , branchReportTrueHits = 10
                    , branchReportFalseHits = 5
                    }
                ]
            , fileReportLines =
                [ LineReport
                    { lineReportLine = 1
                    , lineReportHits = 10
                    }
                ]
            }
        , FileReport
            { fileReportLocation = "src/MyModule/Bar.hs"
            , fileReportFunctions = []
            , fileReportBranches = []
            , fileReportLines = []
            }
        , FileReport
            { fileReportLocation = "src/MyModule/Bar/Baz.hs"
            , fileReportFunctions =
                [ FunctionReport
                    { functionReportLine = 1
                    , functionReportName = "baz1"
                    , functionReportHits = 10
                    }
                ]
            , fileReportBranches =
                [ BranchReport
                    { branchReportLine = 1
                    , branchReportHash = 200
                    , branchReportTrueHits = 0
                    , branchReportFalseHits = 0
                    }
                ]
            , fileReportLines =
                [ LineReport
                    { lineReportLine = 1
                    , lineReportHits = 0
                    }
                ]
            }
        ]
