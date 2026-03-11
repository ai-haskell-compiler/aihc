import Test.Tasty.Focus
import Test.Tasty.ExpectedFailure
import Test.Tasty
import Test.Tasty.HUnit

failure :: TestTree
failure = testCase "failure" $ assertFailure "test failure"

success :: TestTree
success = testCase "success" $ pure ()

main :: IO ()
main =
  defaultMain $
    testGroup
      "tasty-focus"
      [ expectFail . withFocus $
          testGroup
            "no focus runs everything"
            [ failure
            ],
        expectFail . withFocus . focus $
          testGroup
            "focus entire tree runs everything"
            [ failure
            ],
        withFocus $
          testGroup
            "focus out failure"
            [ focus success,
              failure
            ],
        withFocus $
          testGroup
            "deep focus"
            [ testGroup
                "nest"
                [ focus success,
                  failure
                ],
              failure
            ],
        withFocus . focus $
          testGroup
            "nested focus precedence"
            [ testGroup
                "nest"
                [ focus success,
                  failure
                ],
              failure
            ]
      ]
