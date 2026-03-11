-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Help (suite_Help) where

import Chell (equalLines)
import Options.Help
import Options.Tokenize (Token (TokenUnary), Tokens (Tokens))
import Options.Types
import Test.Hspec (Spec, context, shouldBe, specify)

suite_Help :: Spec
suite_Help = context "help" do
  suite_AddHelpFlags
  test_CheckHelpFlag
  test_ShowHelpSummary
  test_ShowHelpSummary_Subcommand
  test_ShowHelpAll
  test_ShowHelpAll_Subcommand
  test_ShowHelpGroup
  test_ShowHelpGroup_Subcommand
  test_ShowHelpGroup_SubcommandInvalid

suite_AddHelpFlags :: Spec
suite_AddHelpFlags = context "addHelpFlags" do
  test_AddHelpFlags_None
  test_AddHelpFlags_Short
  test_AddHelpFlags_Long
  test_AddHelpFlags_Both
  test_AddHelpFlags_NoAll
  test_AddHelpFlags_Subcommand

groupHelp :: Maybe Group
groupHelp =
  Just
    ( Group
        { groupName = "all",
          groupTitle = "Help Options",
          groupDescription = "Show all help options."
        }
    )

infoHelpSummary :: [Char] -> [String] -> OptionInfo
infoHelpSummary shorts longs =
  OptionInfo
    { optionInfoKey = OptionKeyHelpSummary,
      optionInfoShortFlags = shorts,
      optionInfoLongFlags = longs,
      optionInfoDefault = "",
      optionInfoUnary = True,
      optionInfoUnaryOnly = True,
      optionInfoDescription = "Show option summary.",
      optionInfoGroup = groupHelp,
      optionInfoLocation = Nothing,
      optionInfoTypeName = ""
    }

infoHelpAll :: OptionInfo
infoHelpAll =
  OptionInfo
    { optionInfoKey = OptionKeyHelpGroup "all",
      optionInfoShortFlags = [],
      optionInfoLongFlags = ["help-all"],
      optionInfoDefault = "",
      optionInfoUnary = True,
      optionInfoUnaryOnly = True,
      optionInfoDescription = "Show all help options.",
      optionInfoGroup = groupHelp,
      optionInfoLocation = Nothing,
      optionInfoTypeName = ""
    }

test_AddHelpFlags_None :: Spec
test_AddHelpFlags_None = specify "none" $ do
  let commandDefs =
        OptionDefinitions
          [ OptionInfo (OptionKey "test.help") ['h'] ["help"] "default" False False "" Nothing Nothing ""
          ]
          []
  let helpAdded = addHelpFlags commandDefs
  let OptionDefinitions opts subcmds = helpAdded

  shouldBe
    opts
    [ infoHelpAll,
      OptionInfo (OptionKey "test.help") ['h'] ["help"] "default" False False "" Nothing Nothing ""
    ]
  shouldBe subcmds []

test_AddHelpFlags_Short :: Spec
test_AddHelpFlags_Short = specify "short" $ do
  let commandDefs =
        OptionDefinitions
          [ OptionInfo (OptionKey "test.help") [] ["help"] "default" False False "" Nothing Nothing ""
          ]
          []
  let helpAdded = addHelpFlags commandDefs
  let OptionDefinitions opts subcmds = helpAdded

  shouldBe
    opts
    [ infoHelpSummary ['h'] [],
      infoHelpAll,
      OptionInfo (OptionKey "test.help") [] ["help"] "default" False False "" Nothing Nothing ""
    ]
  shouldBe subcmds []

test_AddHelpFlags_Long :: Spec
test_AddHelpFlags_Long = specify "long" $ do
  let commandDefs =
        OptionDefinitions
          [ OptionInfo (OptionKey "test.help") ['h'] [] "default" False False "" Nothing Nothing ""
          ]
          []
  let helpAdded = addHelpFlags commandDefs
  let OptionDefinitions opts subcmds = helpAdded

  shouldBe
    opts
    [ infoHelpSummary [] ["help"],
      infoHelpAll,
      OptionInfo (OptionKey "test.help") ['h'] [] "default" False False "" Nothing Nothing ""
    ]
  shouldBe subcmds []

test_AddHelpFlags_Both :: Spec
test_AddHelpFlags_Both = specify "both" $ do
  let commandDefs = OptionDefinitions [] []
  let helpAdded = addHelpFlags commandDefs
  let OptionDefinitions opts subcmds = helpAdded

  shouldBe
    opts
    [ infoHelpSummary ['h'] ["help"],
      infoHelpAll
    ]
  shouldBe subcmds []

test_AddHelpFlags_NoAll :: Spec
test_AddHelpFlags_NoAll = specify "no-all" $ do
  let commandDefs =
        OptionDefinitions
          [ OptionInfo (OptionKey "test.help") ['h'] ["help", "help-all"] "default" False False "" Nothing Nothing ""
          ]
          []
  let helpAdded = addHelpFlags commandDefs
  let OptionDefinitions opts subcmds = helpAdded

  shouldBe
    opts
    [ OptionInfo (OptionKey "test.help") ['h'] ["help", "help-all"] "default" False False "" Nothing Nothing ""
    ]
  shouldBe subcmds []

test_AddHelpFlags_Subcommand :: Spec
test_AddHelpFlags_Subcommand = specify "subcommand" $ do
  let cmd1_a =
        OptionInfo
          (OptionKey "test.cmd1.a")
          ['a']
          []
          ""
          False
          False
          ""
          ( Just
              Group
                { groupName = "foo",
                  groupTitle = "Foo Options",
                  groupDescription = "More Foo Options"
                }
          )
          Nothing
          ""
  let cmd1_b =
        OptionInfo
          (OptionKey "test.cmd1.b")
          ['b']
          []
          ""
          False
          False
          ""
          ( Just
              Group
                { groupName = "all",
                  groupTitle = "All Options",
                  groupDescription = "More All Options"
                }
          )
          Nothing
          ""
  let commandDefs =
        OptionDefinitions
          []
          [("cmd1", [cmd1_a, cmd1_b])]
  let helpAdded = addHelpFlags commandDefs
  let OptionDefinitions opts subcmds = helpAdded

  let helpFoo =
        OptionInfo
          { optionInfoKey = OptionKeyHelpGroup "foo",
            optionInfoShortFlags = [],
            optionInfoLongFlags = ["help-foo"],
            optionInfoDefault = "",
            optionInfoUnary = True,
            optionInfoUnaryOnly = True,
            optionInfoDescription = "More Foo Options",
            optionInfoGroup =
              Just
                ( Group
                    { groupName = "all",
                      groupTitle = "Help Options",
                      groupDescription = "Show all help options."
                    }
                ),
            optionInfoLocation = Nothing,
            optionInfoTypeName = ""
          }

  shouldBe
    opts
    [ infoHelpSummary ['h'] ["help"],
      infoHelpAll
    ]
  shouldBe subcmds [("cmd1", [helpFoo, cmd1_a, cmd1_b])]

test_CheckHelpFlag :: Spec
test_CheckHelpFlag = specify "checkHelpFlag" $ do
  let checkFlag keys = shouldBe (checkHelpFlag (Tokens [([k], TokenUnary "-h") | k <- keys] []))

  checkFlag [] Nothing
  checkFlag [OptionKeyHelpSummary] (Just HelpSummary)
  checkFlag [OptionKeyHelpGroup "all"] (Just HelpAll)
  checkFlag [OptionKeyHelpGroup "foo"] (Just (HelpGroup "foo"))

variedOptions :: OptionDefinitions
variedOptions =
  addHelpFlags $
    OptionDefinitions
      [ OptionInfo (OptionKey "test.a") ['a'] ["long-a"] "def" False False "a description here" Nothing Nothing "",
        OptionInfo (OptionKey "test.long1") [] ["a-looooooooooooong-option"] "def" False False "description here" Nothing Nothing "",
        OptionInfo (OptionKey "test.long2") [] ["a-loooooooooooooong-option"] "def" False False "description here" Nothing Nothing "",
        OptionInfo (OptionKey "test.b") ['b'] ["long-b"] "def" False False "b description here" Nothing Nothing "",
        OptionInfo
          (OptionKey "test.g")
          ['g']
          ["long-g"]
          "def"
          False
          False
          "g description here"
          ( Just
              Group
                { groupName = "group",
                  groupTitle = "Grouped options",
                  groupDescription = "Show grouped options."
                }
          )
          Nothing
          ""
      ]
      [ ( "cmd1",
          [ OptionInfo (OptionKey "test.cmd1.z") ['z'] ["long-z"] "def" False False "z description here" Nothing Nothing ""
          ]
        ),
        ( "cmd2",
          [ OptionInfo (OptionKey "test.cmd2.y") ['y'] ["long-y"] "def" False False "y description here" Nothing Nothing "",
            OptionInfo
              (OptionKey "test.cmd2.g2")
              []
              ["long-g2"]
              "def"
              False
              False
              "g2 description here"
              ( Just
                  Group
                    { groupName = "group",
                      groupTitle = "Grouped options",
                      groupDescription = "Show grouped options."
                    }
              )
              Nothing
              ""
          ]
        )
      ]

test_ShowHelpSummary :: Spec
test_ShowHelpSummary = specify "showHelpSummary" $ do
  let expected =
        "\
        \Help Options:\n\
        \  -h, --help\n\
        \    Show option summary.\n\
        \  --help-all\n\
        \    Show all help options.\n\
        \  --help-group\n\
        \    Show grouped options.\n\
        \\n\
        \Application Options:\n\
        \  -a, --long-a\n\
        \    a description here\n\
        \    default: def\n\
        \  --a-looooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  --a-loooooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  -b, --long-b\n\
        \    b description here\n\
        \    default: def\n\
        \\n\
        \Subcommands:\n\
        \  cmd1\n\
        \  cmd2\n\
        \\n"
  equalLines expected (helpFor HelpSummary variedOptions Nothing)

test_ShowHelpSummary_Subcommand :: Spec
test_ShowHelpSummary_Subcommand = specify "showHelpSummary-subcommand" $ do
  let expected =
        "\
        \Help Options:\n\
        \  -h, --help\n\
        \    Show option summary.\n\
        \  --help-all\n\
        \    Show all help options.\n\
        \  --help-group\n\
        \    Show grouped options.\n\
        \\n\
        \Application Options:\n\
        \  -a, --long-a\n\
        \    a description here\n\
        \    default: def\n\
        \  --a-looooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  --a-loooooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  -b, --long-b\n\
        \    b description here\n\
        \    default: def\n\
        \\n\
        \Options for subcommand \"cmd1\":\n\
        \  -z, --long-z\n\
        \    z description here\n\
        \    default: def\n\
        \\n"
  equalLines expected (helpFor HelpSummary variedOptions (Just "cmd1"))

test_ShowHelpAll :: Spec
test_ShowHelpAll = specify "showHelpAll" $ do
  let expected =
        "\
        \Help Options:\n\
        \  -h, --help\n\
        \    Show option summary.\n\
        \  --help-all\n\
        \    Show all help options.\n\
        \  --help-group\n\
        \    Show grouped options.\n\
        \\n\
        \Grouped options:\n\
        \  -g, --long-g\n\
        \    g description here\n\
        \    default: def\n\
        \\n\
        \Application Options:\n\
        \  -a, --long-a\n\
        \    a description here\n\
        \    default: def\n\
        \  --a-looooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  --a-loooooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  -b, --long-b\n\
        \    b description here\n\
        \    default: def\n\
        \\n\
        \Options for subcommand \"cmd1\":\n\
        \  -z, --long-z\n\
        \    z description here\n\
        \    default: def\n\
        \\n\
        \Options for subcommand \"cmd2\":\n\
        \  -y, --long-y\n\
        \    y description here\n\
        \    default: def\n\
        \  --long-g2\n\
        \    g2 description here\n\
        \    default: def\n\
        \\n"
  equalLines expected (helpFor HelpAll variedOptions Nothing)

test_ShowHelpAll_Subcommand :: Spec
test_ShowHelpAll_Subcommand = specify "showHelpAll-subcommand" $ do
  let expected =
        "\
        \Help Options:\n\
        \  -h, --help\n\
        \    Show option summary.\n\
        \  --help-all\n\
        \    Show all help options.\n\
        \  --help-group\n\
        \    Show grouped options.\n\
        \\n\
        \Grouped options:\n\
        \  -g, --long-g\n\
        \    g description here\n\
        \    default: def\n\
        \\n\
        \Application Options:\n\
        \  -a, --long-a\n\
        \    a description here\n\
        \    default: def\n\
        \  --a-looooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  --a-loooooooooooooong-option\n\
        \    description here\n\
        \    default: def\n\
        \  -b, --long-b\n\
        \    b description here\n\
        \    default: def\n\
        \\n\
        \Options for subcommand \"cmd1\":\n\
        \  -z, --long-z\n\
        \    z description here\n\
        \    default: def\n\
        \\n"
  equalLines expected (helpFor HelpAll variedOptions (Just "cmd1"))

test_ShowHelpGroup :: Spec
test_ShowHelpGroup = specify "showHelpGroup" $ do
  let expected =
        "\
        \Grouped options:\n\
        \  -g, --long-g\n\
        \    g description here\n\
        \    default: def\n\
        \\n"
  equalLines expected (helpFor (HelpGroup "group") variedOptions Nothing)

test_ShowHelpGroup_Subcommand :: Spec
test_ShowHelpGroup_Subcommand = specify "showHelpGroup-subcommand" $ do
  let expected =
        "\
        \Grouped options:\n\
        \  -g, --long-g\n\
        \    g description here\n\
        \    default: def\n\
        \  --long-g2\n\
        \    g2 description here\n\
        \    default: def\n\
        \\n"
  equalLines expected (helpFor (HelpGroup "group") variedOptions (Just "cmd2"))

test_ShowHelpGroup_SubcommandInvalid :: Spec
test_ShowHelpGroup_SubcommandInvalid = specify "showHelpGroup-subcommand-invalid" $ do
  let expected =
        "\
        \Grouped options:\n\
        \  -g, --long-g\n\
        \    g description here\n\
        \    default: def\n\
        \\n"
  equalLines expected (helpFor (HelpGroup "group") variedOptions (Just "noexist"))
