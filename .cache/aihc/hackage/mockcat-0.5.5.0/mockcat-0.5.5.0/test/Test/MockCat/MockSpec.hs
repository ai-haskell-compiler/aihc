{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}

module Test.MockCat.MockSpec (spec) where

import qualified Control.Exception as E
import Data.Function ((&))
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "Test of Mock" do
    describe "combination test" do
      it "arity = 1" do
        f <- createStubFn $ True |> False
        f True `shouldBe` False

      it "arity = 2" do
        f <- createStubFn $ True |> False |> True
        f True False `shouldBe` True

      it "arity = 3" do
        f <- createStubFn $ True |> "False" |> True |> "False"
        f True "False" True `shouldBe` "False"

      it "arity = 4" do
        f <- createStubFn $ True |> "False" |> True |> "False" |> True
        f True "False" True "False" `shouldBe` True

      it "Param |> a" do
        f <- createStubFn $ any |> False
        f True `shouldBe` False

      it "Param |> (a |> b)" do
        f <- createStubFn $ any |> False |> True
        f True False `shouldBe` True

      it "a     |> (Param |> b)" do
        f <- createStubFn $ True |> any |> True
        f True False `shouldBe` True

      it "Param |> (Param |> a)" do
        f <- createStubFn $ any |> any |> True
        f True False `shouldBe` True

      it "a     |> (Param |> (Param |> a))" do
        f <- createStubFn $ "any" |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

      it "param |> (Param |> (Param |> a))" do
        f <- createStubFn $ any |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

    mockTest
      Fixture
        { name = "arity = 1",
          create = createMock $ "a" |> False,
          apply = (`stubFn` "a"),
          applyFailed = Just (`stubFn` "x"),
          expected = False,
          verifyApply = (`shouldApplyTo` "a"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = (`shouldApplyTo` "2"),
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` "a"
        }

    mockTest
      Fixture
        { name = "arity = 2",
          create = createMock $ "a" |> "b" |> True,
          apply = \m -> stubFn m "a" "b",
          applyFailed = Just (\m -> stubFn m "a" "x"),
          expected = True,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "2" |> "b",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b")
        }

    mockTest
      Fixture
        { name = "arity = 3",
          create = createMock $ "a" |> "b" |> "c" |> False,
          apply = \m -> stubFn m "a" "b" "c",
          applyFailed = Just (\m -> stubFn m "a" "b" "x"),
          expected = False,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "d",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c")
        }

    mockTest
      Fixture
        { name = "arity = 4",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> True,
          apply = \m -> stubFn m "a" "b" "c" "d",
          applyFailed = Just (\m -> stubFn m "a" "b" "c" "x"),
          expected = True,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "x",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d")
        }

    mockTest
      Fixture
        { name = "arity = 5",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> False,
          apply = \m -> stubFn m "a" "b" "c" "d" "e",
          applyFailed = Just (\m -> stubFn m "a" "b" "c" "d" "x"),
          expected = False,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "x",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e")
        }

    mockTest
      Fixture
        { name = "arity = 6",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> True,
          apply = \m -> stubFn m "a" "b" "c" "d" "e" "f",
          applyFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "x"),
          expected = True,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "x",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f")
        }

    mockTest
      Fixture
        { name = "arity = 7",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> False,
          apply = \m -> stubFn m "a" "b" "c" "d" "e" "f" "g",
          applyFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "f" "x"),
          expected = False,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "y",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g")
        }

    mockTest
      Fixture
        { name = "arity = 8",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> False,
          apply = \m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "h",
          applyFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "x"),
          expected = False,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "x",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h")
        }

    mockTest
      Fixture
        { name = "arity = 9",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i" |> False,
          apply = \m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "h" "i",
          applyFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "h" "x"),
          expected = False,
          verifyApply = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "x",
          verifyApplyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i")
        }

  describe "Test of Multi Mock" do
    mockTest
      Fixture
        { name = "arity = 1",
          create =
            createMock $ do
              onCase $ "1" |> True
              onCase $ "2" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m -> [stubFn m "1", stubFn m "2"],
          applyFailed = Just \m -> [stubFn m "3"],
          verifyApply = \m -> do
            m `shouldApplyTo` "1"
            m `shouldApplyTo` "2",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = (`shouldApplyTo` "3"),
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` "1"
            m `shouldApplyTimes` c `to` "2"
        }

    mockTest
      Fixture
        { name = "arity = 2",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> True
              onCase $ "2" |> "3" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2",
              stubFn m "2" "3"
            ],
          applyFailed = Just \m -> [stubFn m "1" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2")
            m `shouldApplyTo` ("2" |> "3"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2")
            m `shouldApplyTimes` c `to` ("2" |> "3"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 3",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> True
              onCase $ "2" |> "3" |> "4" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3",
              stubFn m "2" "3" "4"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3")
            m `shouldApplyTo` ("2" |> "3" |> "4"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 4",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3" "4",
              stubFn m "2" "3" "4" "5"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "3" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 5",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> "5" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> "6" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3" "4" "5",
              stubFn m "2" "3" "4" "5" "6"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "3" "4" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 6",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6",
              stubFn m "2" "3" "4" "5" "6" "7"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 7",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6" "7",
              stubFn m "2" "3" "4" "5" "6" "7" "8"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "6" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 8",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6" "7" "8",
              stubFn m "2" "3" "4" "5" "6" "7" "8" "9"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "6" "7" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 9",
          create =
            createMock $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6" "7" "8" "9",
              stubFn m "2" "3" "4" "5" "6" "7" "8" "9" "10"
            ],
          applyFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "6" "7" "8" "x"],
          verifyApply = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10"),
          verifyApplyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "x")
        }

  describe "Order Verification" do
    describe "exactly sequential order." do
      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 1",
            create = createMock $ any |> (),
            apply = \m -> do
              evaluate $ stubFn m "a"
              evaluate $ stubFn m "b"
              evaluate $ stubFn m "c",
            verifyApply = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b",
                                         "c"
                                       ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b",
                                         "b"
                                       ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 9",
            create = createMock $ any |> any |> any |> any |> any |> any |> any |> any |> (),
            apply = \m -> do
              evaluate $ stubFn m "1" "2" "3" "4" "5" "6" "7" "8"
              evaluate $ stubFn m "2" "3" "4" "5" "6" "7" "8" "9"
              evaluate $ stubFn m "3" "4" "5" "6" "7" "8" "9" "0",
            verifyApply = \m ->
              m
                `shouldApplyInOrder` [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8",
                                         "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9",
                                         "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "0"
                                       ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInOrder` [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x",
                                         "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "y",
                                         "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "z"
                                       ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "number of function calls doesn't match the number of params.",
            create = createMock $ any |> (),
            apply = \m -> do
              evaluate $ stubFn m "a"
              pure (),
            verifyApply = \m ->
              m
                `shouldApplyInOrder` [ "a"
                                       ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b"
                                       ]
          }

    describe "partially sequential order." do
      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 1",
            create = createMock $ any |> (),
            apply = \m -> do
              evaluate $ stubFn m "a"
              evaluate $ stubFn m "b"
              evaluate $ stubFn m "c"
              pure (),
            verifyApply = \m ->
              m
                `shouldApplyInPartialOrder` [ "a",
                                                "c"
                                              ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "b",
                                                "a"
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 9",
            create = createMock $ any |> any |> any |> any |> any |> any |> any |> any |> any |> (),
            apply = \m -> do
              evaluate $ stubFn m "a" "" "" "" "" "" "" "" ""
              evaluate $ stubFn m "b" "" "" "" "" "" "" "" ""
              evaluate $ stubFn m "c" "" "" "" "" "" "" "" ""
              pure (),
            verifyApply = \m ->
              m
                `shouldApplyInPartialOrder` [ "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
                                                "c" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
                                              ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "b" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
                                                "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "Uncalled value specified.",
            create = createMock $ any |> (),
            apply = \m -> do
              evaluate $ stubFn m "a"
              evaluate $ stubFn m "b"
              evaluate $ stubFn m "c"
              pure (),
            verifyApply = \m ->
              m
                `shouldApplyInPartialOrder` [ "b",
                                                "c"
                                              ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "a",
                                                "d"
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "number of function calls doesn't match the number of params",
            create = createMock $ any |> (),
            apply = \m -> do
              evaluate $ stubFn m "a"
              pure (),
            verifyApply = \m ->
              m
                `shouldApplyInPartialOrder` [ "a"
                                              ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "a",
                                                "b"
                                              ]
          }

  describe "The number of times applied can also be verified by specifying conditions." do
    it "greater than equal" do
      m <- createMock $ "a" |> True
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesGreaterThanEqual` 3 `to` "a"

    it "less than equal" do
      m <- createMock $ "a" |> False
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesLessThanEqual` 3 `to` "a"

    it "greater than" do
      m <- createMock $ "a" |> True
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesGreaterThan` 2 `to` "a"

    it "less than" do
      m <- createMock $ "a" |> False
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesLessThan` 4 `to` "a"

  describe "Monad" do
    it "Return IO Monad." do
      m <- createMock $ "Article Id" |> pure @IO "Article Title"

      result <- stubFn m "Article Id"

      result `shouldBe` "Article Title"

      m `shouldApplyTo` "Article Id"

  describe "Appropriate message when a test fails." do
    describe "anonymous mock" do
      describe "apply" do
        it "simple mock" do
          m <- createMock $ "a" |> pure @IO True
          stubFn m "b"
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected: \"a\"\n\
              \   but got: \"b\""

        it "multi mock" do
          m <-
            createMock $ do
              onCase $ "aaa" |> (100 :: Int) |> pure @IO True
              onCase $ "bbb" |> (200 :: Int) |> pure @IO False

          stubFn m "aaa" 200
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected one of the following:\n\
              \    \"aaa\",100\n\
              \    \"bbb\",200\n\
              \  but got:\n\
              \    \"aaa\",200"

      describe "verify" do
        it "simple mock verify" do
          m <- createMock $ any |> pure @IO True
          evaluate $ stubFn m "A"
          m `shouldApplyTo` "X"
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected: \"X\"\n\
              \   but got: \"A\""

        it "It has never been applied." do
          m <- createMock $ "X" |> pure @IO True
          m `shouldApplyTo` "X"
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected: \"X\"\n\
              \   but got: It has never been applied"

        it "count" do
          m <- createMock $ any |> pure @IO True
          evaluate $ stubFn m "A"
          let e =
                "function was not applied the expected number of times to the expected arguments.\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyTimes` (2 :: Int) `to` "A" `shouldThrow` errorCall e

        it "verify sequence" do
          m <- createMock $ any |> pure @IO False
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          evaluate $ stubFn m "A"
          let e =
                "function was not applied to the expected arguments in the expected order.\n\
                \  expected 1st applied: \"A\"\n\
                \   but got 1st applied: \"B\"\n\
                \  expected 2nd applied: \"B\"\n\
                \   but got 2nd applied: \"C\"\n\
                \  expected 3rd applied: \"C\"\n\
                \   but got 3rd applied: \"A\""
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify sequence (count mismatch)" do
          m <- createMock $ any |> True
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          let e =
                "function was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 3\n\
                \   but got: 2"
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify partially sequence" do
          m <- createMock $ any |> True
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "A"
          let e =
                "function was not applied to the expected arguments in the expected order.\n\
                \  expected order:\n\
                \    \"A\"\n\
                \    \"C\"\n\
                \  but got:\n\
                \    \"B\"\n\
                \    \"A\""
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify partially sequence (count mismatch)" do
          m <- createMock $ any |> False
          evaluate $ stubFn m "B"
          let e =
                "function was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify applied anything" do
          m <- createMock $ "X" |> True
          shouldApplyToAnything m `shouldThrow` errorCall "It has never been applied function"

    describe "named mock" do
      describe "aply" do
        it "simple mock" do
          m <- createNamedMock "mock function" $ "a" |> pure @IO ()
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected: \"a\"\n\
                \   but got: \"b\""
          stubFn m "b" `shouldThrow` errorCall e

        it "multi mock" do
          m <-
            createNamedMock
              "mock function"
              do 
                onCase $ "aaa" |> True |> pure @IO True
                onCase $ "bbb" |> False |> pure @IO False
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected one of the following:\n\
                \    \"aaa\",True\n\
                \    \"bbb\",False\n\
                \  but got:\n\
                \    \"aaa\",False"
          stubFn m "aaa" False `shouldThrow` errorCall e

      describe "verify" do
        it "simple mock verify" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected: \"X\"\n\
                \   but got: \"A\""
          m `shouldApplyTo` "X" `shouldThrow` errorCall e

        it "count" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied the expected number of times to the expected arguments.\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyTimes` (2 :: Int) `to` "A" `shouldThrow` errorCall e

        it "verify sequence" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied to the expected arguments in the expected order.\n\
                \  expected 1st applied: \"A\"\n\
                \   but got 1st applied: \"B\"\n\
                \  expected 2nd applied: \"B\"\n\
                \   but got 2nd applied: \"C\"\n\
                \  expected 3rd applied: \"C\"\n\
                \   but got 3rd applied: \"A\""
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify sequence (count mismatch)" do
          m <- createNamedMock "createStubFnc" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          let e =
                "function `createStubFnc` was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 3\n\
                \   but got: 2"
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify partially sequence" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied to the expected arguments in the expected order.\n\
                \  expected order:\n\
                \    \"A\"\n\
                \    \"C\"\n\
                \  but got:\n\
                \    \"B\"\n\
                \    \"A\""
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify partially sequence (count mismatch)" do
          m <- createNamedMock "createStubFnc" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          let e =
                "function `createStubFnc` was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify applied anything" do
          m <- createNamedMock "mock" $ "X" |> True
          shouldApplyToAnything m `shouldThrow` errorCall "It has never been applied function `mock`"

  describe "use expectation" do
    it "expectByExpr" do
      f <- createStubFn $ $(expectByExpr [|\x -> x == "y" || x == "z"|]) |> True
      f "y" `shouldBe` True

  describe "repeatable" do
    it "arity = 1" do
      f <- createStubFn $ do
        onCase $ "a" |> True
        onCase $ "b" |> False
        onCase $ "a" |> False
        onCase $ "b" |> True
        
      v1 <- evaluate $ f "a"
      v2 <- evaluate $ f "a"
      v3 <- evaluate $ f "b"
      v4 <- evaluate $ f "b"
      v1 `shouldBe` True
      v2 `shouldBe` False
      v3 `shouldBe` False
      v4 `shouldBe` True

    it "arity = 2" do
      f <- createStubFn $ do
        onCase $ "a" |> "b" |> (0 :: Int)
        onCase $ "a" |> "c" |> (1 :: Int)
        onCase $ "a" |> "b" |> (2 :: Int)
        onCase $ "a" |> "c" |> (3 :: Int)

      v1 <- evaluate $ f "a" "b"
      v2 <- evaluate $ f "a" "b"
      v3 <- evaluate $ f "a" "c"
      v4 <- evaluate $ f "a" "c"
      v5 <- evaluate $ f "a" "b"
      v1 `shouldBe` (0 :: Int)
      v2 `shouldBe` (2 :: Int)
      v3 `shouldBe` (1 :: Int)
      v4 `shouldBe` (3 :: Int)
      v5 `shouldBe` (2 :: Int)

  describe "constant" do
    it "createConstantMock" do
      m <- createConstantMock "foo"
      stubFn m `shouldBe` "foo"
      shouldApplyToAnything m

    it "createNamedConstantMock" do
      m <- createNamedConstantMock "const" "foo"
      stubFn m `shouldBe` "foo"
      shouldApplyToAnything m

    it "createConstantMock (error message)" do
      m <- createConstantMock "foo"
      shouldApplyToAnything m `shouldThrow` errorCall "It has never been applied function"

    it "createNamedConstantMock (error message)" do
      m <- createNamedConstantMock "constant" "foo"
      shouldApplyToAnything m `shouldThrow` errorCall "It has never been applied function `constant`"

    it "verify constant IO mock" do
      m <- createMock $ pure @IO "foo"
      stubFn m `shouldReturn` "foo"
      stubFn m `shouldReturn` "foo"
      stubFn m `shouldReturn` "foo"
      m `shouldApplyTimesToAnything` 3

    it "verify constant multi IO mock" do
      m <- createMock $ do
        onCase $ pure @IO "foo"
        onCase $ pure @IO "bar"
        onCase $ pure @IO "baz"

      stubFn m `shouldReturn` "foo"
      stubFn m `shouldReturn` "bar"
      stubFn m `shouldReturn` "baz"
      m `shouldApplyTimesToAnything` 3

data Fixture mock r = Fixture
  { name :: String,
    create :: IO mock,
    apply :: mock -> r,
    applyFailed :: Maybe (mock -> r),
    expected :: r,
    verifyApply :: mock -> IO (),
    verifyApplyAny :: mock -> IO (),
    verifyApplyFailed :: mock -> IO (),
    verifyApplyCount :: mock -> Int -> IO ()
  }

data VerifyOrderFixture mock r = VerifyOrderFixture
  { name :: String,
    create :: IO mock,
    apply :: mock -> IO r,
    verifyApply :: mock -> IO (),
    verifyApplyFailed :: mock -> IO ()
  }

class Eval a where
  evaluate :: a -> IO a

instance Eval [a] where
  evaluate v = do
    mapM_ (\x -> E.evaluate (x `seq` ())) v
    pure v

instance {-# OVERLAPPABLE #-} Eval a where
  evaluate = E.evaluate

-- mock test template
mockTest :: (Eq r, Show r, Eval r) => Fixture mock r -> SpecWith (Arg Expectation)
mockTest f = describe f.name do
  it "Expected argument is applied, the expected value is returned." do
    m <- f.create
    f.apply m `shouldBe` f.expected

  it "Unexpected argument is applied, an exception is thrown." do
    f.applyFailed & maybe mempty \func -> do
      m <- f.create
      evaluate (func m) `shouldThrow` anyErrorCall

  it "Expected arguments are applied, the verification succeeds." do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApply m

  it "Unexpected arguments are applied, the verification fails." do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApplyFailed m `shouldThrow` anyErrorCall

  it "The number of times a function has been applied can be verification (0 times)." do
    m <- f.create
    f.verifyApplyCount m 0

  it "The number of times a function has been applied can be verification (3 times)." do
    m <- f.create
    evaluate $ f.apply m
    evaluate $ f.apply m
    evaluate $ f.apply m
    f.verifyApplyCount m 3

  it "Fails to verification the number of times it has been applied, an exception is thrown." do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApplyCount m 3 `shouldThrow` anyErrorCall

  it "verify any" do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApplyAny m

mockOrderTest :: VerifyOrderFixture mock r -> SpecWith (Arg Expectation)
mockOrderTest f = describe f.name do
  it "If the functions are applied in the expected order, the verification succeeds." do
    m <- f.create
    f.apply m
    f.verifyApply m

  it "If the functions are not applied in the expected order, verification fails." do
    m <- f.create
    f.apply m
    f.verifyApplyFailed m `shouldThrow` anyErrorCall
