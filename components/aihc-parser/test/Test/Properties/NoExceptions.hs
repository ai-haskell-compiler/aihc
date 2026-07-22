{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Properties.NoExceptions
  ( prop_preprocessorArbitraryTextNoExceptions,
    prop_lexerArbitraryTextNoExceptions,
    prop_moduleParserArbitraryTokensNoExceptions,
    prop_exprParserArbitraryTokensNoExceptions,
    prop_typeParserArbitraryTokensNoExceptions,
    prop_patternParserArbitraryTokensNoExceptions,
    prop_declParserArbitraryTokensNoExceptions,
    prop_importDeclParserArbitraryTokensNoExceptions,
    prop_moduleHeaderParserArbitraryTokensNoExceptions,
    prop_genLexTokenKindConstructorCoverage,
    genLexToken,
    shrinkLexToken,
  )
where

import Aihc.Parser.Internal.Testing
  ( LexToken (..),
    LexTokenKind (..),
    TokenOrigin (..),
    lexModuleTokens,
    lexTokens,
    parseDeclFromTokens,
    parseExprFromTokens,
    parseImportDeclFromTokens,
    parseModuleFromTokens,
    parseModuleHeaderFromTokens,
    parsePatternFromTokens,
    parseTypeFromTokens,
  )
import Aihc.Parser.Syntax (ExtensionSetting (..), FloatType (..), NumericType (..), SourceSpan (..))
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, evaluate, try)
import CppSupport (preprocessForParserWithoutIncludes)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Coverage (assertAnyCtorCoverage)
import Test.QuickCheck

prop_preprocessorArbitraryTextNoExceptions :: Property
prop_preprocessorArbitraryTextNoExceptions =
  forAll genArbitraryText $ \source ->
    ioProperty $
      noExceptionProperty
        "preprocessForParserWithoutIncludes"
        (preprocessForParserWithoutIncludes "QuickCheck.hs" [] source)

prop_lexerArbitraryTextNoExceptions :: Property
prop_lexerArbitraryTextNoExceptions =
  forAll genArbitraryText $ \source ->
    ioProperty $ do
      lexTokensProp <- noExceptionProperty "lexTokens" (lexTokens source)
      lexModuleTokensProp <- noExceptionProperty "lexModuleTokens" (lexModuleTokens source)
      pure (lexTokensProp .&&. lexModuleTokensProp)

prop_moduleParserArbitraryTokensNoExceptions :: Property
prop_moduleParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parseModuleFromTokens" (parseModuleFromTokens "<quickcheck>" tokens))

prop_exprParserArbitraryTokensNoExceptions :: Property
prop_exprParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parseExprFromTokens" (parseExprFromTokens "<quickcheck>" tokens))

prop_typeParserArbitraryTokensNoExceptions :: Property
prop_typeParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parseTypeFromTokens" (parseTypeFromTokens "<quickcheck>" tokens))

prop_patternParserArbitraryTokensNoExceptions :: Property
prop_patternParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parsePatternFromTokens" (parsePatternFromTokens "<quickcheck>" tokens))

prop_declParserArbitraryTokensNoExceptions :: Property
prop_declParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parseDeclFromTokens" (parseDeclFromTokens "<quickcheck>" tokens))

prop_importDeclParserArbitraryTokensNoExceptions :: Property
prop_importDeclParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parseImportDeclFromTokens" (parseImportDeclFromTokens "<quickcheck>" tokens))

prop_moduleHeaderParserArbitraryTokensNoExceptions :: Property
prop_moduleHeaderParserArbitraryTokensNoExceptions =
  forAllShrink genTokenStream shrinkTokenStream $ \tokens ->
    ioProperty (noExceptionProperty "parseModuleHeaderFromTokens" (parseModuleHeaderFromTokens "<quickcheck>" tokens))

prop_genLexTokenKindConstructorCoverage :: Property
prop_genLexTokenKindConstructorCoverage =
  checkCoverage $
    forAll (vectorOf 512 genLexTokenKind) $ \kinds ->
      assertAnyCtorCoverage [] kinds (property True)

noExceptionProperty :: forall a. (NFData a) => String -> a -> IO Property
noExceptionProperty operation value = do
  outcome <- (try (evaluate (force value)) :: IO (Either SomeException a))
  pure $
    case outcome of
      Left err -> counterexample (operation <> " threw exception: " <> show err) False
      Right _ -> property True

genArbitraryText :: Gen Text
genArbitraryText = T.pack <$> sized (\size -> do n <- chooseInt (0, min 256 (size * 8 + 8)); vectorOf n arbitrary)

genTokenStream :: Gen [LexToken]
genTokenStream = sized $ \size -> do
  n <- chooseInt (0, min 80 (size * 4 + 4))
  vectorOf n genLexToken

shrinkTokenStream :: [LexToken] -> [[LexToken]]
shrinkTokenStream = shrinkList shrinkLexToken

genLexToken :: Gen LexToken
genLexToken = do
  kind <- genLexTokenKind
  tokenText <- genTokenText
  span' <- genSourceSpan
  tokenOrigin <- elements [FromSource, InsertedLayout]
  atLineStart <- arbitrary
  pure LexToken {lexTokenKind = kind, lexTokenText = tokenText, lexTokenSpan = span', lexTokenOrigin = tokenOrigin, lexTokenAtLineStart = atLineStart}

shrinkLexToken :: LexToken -> [LexToken]
shrinkLexToken token =
  [token {lexTokenText = shrunkText} | shrunkText <- shrinkText (lexTokenText token)]
    <> [token {lexTokenSpan = shrunkSpan} | shrunkSpan <- shrinkSourceSpan (lexTokenSpan token)]

genLexTokenKind :: Gen LexTokenKind
genLexTokenKind =
  oneof
    [ pure TkKeywordModule,
      pure TkKeywordWhere,
      pure TkKeywordDo,
      pure TkKeywordClass,
      pure TkKeywordData,
      pure TkKeywordDefault,
      pure TkKeywordDeriving,
      pure TkKeywordImport,
      pure TkKeywordCase,
      pure TkKeywordForall,
      pure TkKeywordForeign,
      pure TkKeywordOf,
      pure TkKeywordLet,
      pure TkKeywordIn,
      pure TkKeywordIf,
      pure TkKeywordThen,
      pure TkKeywordElse,
      pure TkKeywordInfix,
      pure TkKeywordInfixl,
      pure TkKeywordInfixr,
      pure TkKeywordInstance,
      pure TkKeywordNewtype,
      pure TkKeywordType,
      pure TkKeywordUnderscore,
      pure TkKeywordProc,
      pure TkKeywordRec,
      pure TkKeywordMdo,
      pure TkKeywordPattern,
      pure TkKeywordBy,
      pure TkKeywordUsing,
      TkQualifiedDo <$> genModuleText,
      TkQualifiedMdo <$> genModuleText,
      pure TkReservedDotDot,
      pure TkReservedColon,
      pure TkReservedDoubleColon,
      pure TkReservedEquals,
      pure TkReservedBackslash,
      pure TkReservedPipe,
      pure TkReservedLeftArrow,
      pure TkReservedRightArrow,
      pure TkReservedAt,
      pure TkReservedDoubleArrow,
      pure TkPrefixPercent,
      pure TkLinearArrow,
      pure TkArrowTail,
      pure TkArrowTailReverse,
      pure TkDoubleArrowTail,
      pure TkDoubleArrowTailReverse,
      pure TkBananaOpen,
      pure TkBananaClose,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaLanguage <$> genExtensionSettings,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaInstanceOverlap <$> genInstanceOverlapPragma,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaWarning <$> genTokenText,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaDeprecated <$> genTokenText,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) <$> (Syntax.PragmaInline <$> genTokenText <*> genTokenText),
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaUnpack <$> genPragmaUnpackKind,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) <$> (Syntax.PragmaSource <$> genTokenText <*> genTokenText),
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaSCC <$> genTokenText,
      TkPragma . (\pt -> Syntax.Pragma {Syntax.pragmaType = pt, Syntax.pragmaRawText = ""}) . Syntax.PragmaUnknown <$> genTokenText,
      TkVarId <$> genIdentifierText,
      TkConId <$> genConstructorText,
      TkQVarId <$> genModuleText <*> genIdentifierText,
      TkQConId <$> genModuleText <*> genConstructorText,
      TkImplicitParam <$> genIdentifierText,
      TkVarSym <$> genOperatorText,
      TkConSym <$> genConOperatorText,
      TkQVarSym <$> genModuleText <*> genOperatorText,
      TkQConSym <$> genModuleText <*> genConOperatorText,
      TkInteger <$> arbitrary <*> genNumericType,
      TkFloat <$> arbitrary <*> genFloatType,
      TkChar <$> arbitrary,
      TkCharHash <$> arbitrary <*> genTokenText,
      TkString <$> genTokenText,
      TkStringHash <$> genTokenText <*> genTokenText,
      TkOverloadedLabel <$> genIdentifierText <*> genTokenText,
      pure TkSpecialLParen,
      pure TkSpecialRParen,
      pure TkSpecialUnboxedLParen,
      pure TkSpecialUnboxedRParen,
      pure TkSpecialComma,
      pure TkSpecialSemicolon,
      pure TkSpecialLBracket,
      pure TkSpecialRBracket,
      pure TkSpecialBacktick,
      pure TkSpecialLBrace,
      pure TkSpecialRBrace,
      pure TkMinusOperator,
      pure TkPrefixMinus,
      pure TkPrefixBang,
      pure TkPrefixTilde,
      pure TkRecordDot,
      pure TkTypeApp,
      pure TkTHExpQuoteOpen,
      pure TkTHExpQuoteClose,
      pure TkTHTypedQuoteOpen,
      pure TkTHTypedQuoteClose,
      pure TkTHDeclQuoteOpen,
      pure TkTHTypeQuoteOpen,
      pure TkTHPatQuoteOpen,
      pure TkTHQuoteTick,
      pure TkTHTypeQuoteTick,
      pure TkTHSplice,
      pure TkTHTypedSplice,
      TkQuasiQuote <$> genQuoterText <*> genTokenText,
      pure TkLineComment,
      pure TkBlockComment,
      TkError <$> genTokenText,
      pure TkEOF
    ]

genConstructorText :: Gen Text
genConstructorText =
  oneof
    [ pure "Foo",
      pure "Bar",
      pure "Just",
      pure "Nothing"
    ]

genConOperatorText :: Gen Text
genConOperatorText =
  oneof
    [ pure ":",
      pure ":+:",
      pure ":-:"
    ]

genExtensionSettings :: Gen [ExtensionSetting]
genExtensionSettings = do
  n <- chooseInt (0, 8)
  vectorOf n genExtensionSetting

genExtensionSetting :: Gen ExtensionSetting
genExtensionSetting = do
  extension <- elements Syntax.allKnownExtensions
  oneof [pure (EnableExtension extension), pure (DisableExtension extension)]

genInstanceOverlapPragma :: Gen Syntax.InstanceOverlapPragma
genInstanceOverlapPragma =
  elements [Syntax.Overlapping, Syntax.Overlappable, Syntax.Overlaps, Syntax.Incoherent]

genPragmaUnpackKind :: Gen Syntax.PragmaUnpackKind
genPragmaUnpackKind =
  elements [Syntax.UnpackPragma, Syntax.NoUnpackPragma]

genNumericType :: Gen NumericType
genNumericType =
  elements [TInteger, TIntHash, TWordHash, TInt8Hash, TInt16Hash, TInt32Hash, TInt64Hash, TWord8Hash, TWord16Hash, TWord32Hash, TWord64Hash]

genFloatType :: Gen FloatType
genFloatType =
  elements [TFractional, TFloatHash, TDoubleHash]

genTokenText :: Gen Text
genTokenText = T.pack <$> sized (\size -> do n <- chooseInt (0, min 32 (size + 4)); vectorOf n arbitrary)

shrinkText :: Text -> [Text]
shrinkText txt = map T.pack (shrink (T.unpack txt))

genIdentifierText :: Gen Text
genIdentifierText =
  oneof
    [ pure "x",
      pure "M.x",
      pure "_",
      pure "forall",
      genTokenText
    ]

genModuleText :: Gen Text
genModuleText =
  oneof
    [ pure "M",
      pure "Data.List",
      genConstructorText
    ]

genOperatorText :: Gen Text
genOperatorText =
  oneof
    [ elements ["+", "-", "*", "->", "=>", "::", "=", "|", ":", ".."],
      genTokenText
    ]

genQuoterText :: Gen Text
genQuoterText =
  oneof
    [ elements ["qq", "M.qq", "x", "_"],
      genTokenText
    ]

genSourceSpan :: Gen SourceSpan
genSourceSpan =
  oneof
    [ pure NoSourceSpan,
      do
        sourceName <- elements ["<input>", "source", "generated.h"]
        startLine <- chooseInt (1, 200)
        startCol <- chooseInt (1, 200)
        endLine <- chooseInt (startLine, startLine + 5)
        endCol <-
          if endLine == startLine
            then chooseInt (startCol, startCol + 10)
            else chooseInt (1, 200)
        startOffset <- chooseInt (0, 4000)
        endOffset <- chooseInt (startOffset, startOffset + 200)
        pure (SourceSpan sourceName startLine startCol endLine endCol startOffset endOffset)
    ]

shrinkSourceSpan :: SourceSpan -> [SourceSpan]
shrinkSourceSpan span' =
  case span' of
    NoSourceSpan -> []
    SourceSpan sourceName sl sc el ec startOffset endOffset ->
      [NoSourceSpan]
        <> [ SourceSpan sourceName sl' sc' el' ec' startOffset' endOffset'
           | (sl', sc', el', ec', startOffset', endOffset') <- shrink (sl, sc, el, ec, startOffset, endOffset),
             sl' >= 1,
             sc' >= 1,
             el' >= sl',
             ec' >= 1,
             el' > sl' || ec' >= sc',
             startOffset' >= 0,
             endOffset' >= startOffset'
           ]
