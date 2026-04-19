{-# LANGUAGE OverloadedStrings #-}

-- | Shared generators and helpers used by Expr.hs, Pattern.hs, Type.hs, and Decl.hs.
-- This module breaks import cycles because it doesn't depend on any of those modules.
module Test.Properties.Arb.Identifiers
  ( -- * Variable identifiers
    genVarId,
    genVarUnqualifiedName,
    genVarName,
    shrinkIdent,
    isValidGeneratedIdent,
    genVarSym,
    isValidGeneratedVarSym,

    -- * Constructor identifiers
    genConId,
    shrinkConIdent,
    isValidConIdent,

    -- * Constructor operator symbols
    genConSym,
    isValidGeneratedConSym,

    -- * Module qualifiers
    genOptionalQualifier,
    genModuleQualifier,
    genModuleSegment,

    -- * Field names
    genFieldName,

    -- * Quasi-quotation helpers
    genQuoterName,
    isValidQuoterName,
    genQuasiBody,

    -- * Character and string generators
    genCharValue,
    genStringValue,

    -- * Numeric helpers
    genTenths,
    showHex,
    shrinkFloat,
  )
where

import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax (Extension, Name, NameType (..), UnqualifiedName, allKnownExtensions, mkUnqualifiedName, qualifyName)
import Data.Char (GeneralCategory (..), generalCategory)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck (Gen, chooseInt, chooseInteger, elements, oneof, shrink, shrinkIntegral, vectorOf)

-- | All extensions enabled for maximum keyword coverage in testing.
allExtensions :: Set.Set Extension
allExtensions = Set.fromList allKnownExtensions

allChars :: [Char]
allChars = [minBound .. maxBound]

varIdentStartChars :: [Char]
varIdentStartChars = filter isValidGeneratedIdentStartChar allChars

conIdentStartChars :: [Char]
conIdentStartChars = filter isValidConIdentStartChar allChars

identTailChars :: [Char]
identTailChars = filter isValidIdentTailChar allChars

symbolChars :: [Char]
symbolChars = filter isValidSymbolChar allChars

-- Symbols starting with ':' are constructors, and symbols ending with '#' clashes with overloaded labels.
varSymStartChars :: [Char]
varSymStartChars = filter (\c -> c /= ':' && c /= '#') symbolChars

reservedOperators :: Set.Set Text
reservedOperators =
  Set.fromList
    [ "..",
      ":",
      "::",
      "=",
      "\\",
      "|",
      "<-",
      "->",
      "@",
      "~",
      "=>",
      "-<",
      ">-",
      "-<<",
      ">>-",
      "→",
      "←",
      "⇒",
      "∷",
      "∀",
      "⤙",
      "⤚",
      "⤛",
      "⤜",
      "⦇",
      "⦈",
      "⟦",
      "⟧",
      "⊸",
      "★"
    ]

-------------------------------------------------------------------------------
-- Variable identifiers
-------------------------------------------------------------------------------

genVarId :: Gen Text
genVarId = do
  first <- elements varIdentStartChars
  restLen <- chooseInt (0, 8)
  rest <- vectorOf restLen (elements identTailChars)
  hashCount <- chooseInt (0, 4)
  let candidate = T.pack (first : rest) <> T.replicate hashCount "#"
  if isValidGeneratedIdent candidate
    then pure candidate
    else genVarId

genVarUnqualifiedName :: Gen UnqualifiedName
genVarUnqualifiedName = oneof [mkUnqualifiedName NameVarId <$> genVarId, mkUnqualifiedName NameVarSym <$> genVarSym]

genVarName :: Gen Name
genVarName = do
  qual <- genOptionalQualifier
  qualifyName qual <$> genVarUnqualifiedName

shrinkIdent :: Text -> [Text]
shrinkIdent = shrinkWithPreservedFirstChar isValidGeneratedIdent

isValidGeneratedIdent :: Text -> Bool
isValidGeneratedIdent ident =
  case unsnocMagicHash ident of
    Just (baseIdent, _magicHashes) ->
      case T.uncons baseIdent of
        Just (first, rest) ->
          baseIdent /= "_"
            && isValidGeneratedIdentStartChar first
            && T.all isValidIdentTailChar rest
            && not (isReservedIdentifier allExtensions ident)
        Nothing -> False
    Nothing -> False

unsnocMagicHash :: Text -> Maybe (Text, Text)
unsnocMagicHash ident =
  let magicHashes = T.takeWhileEnd (== '#') ident
      baseIdent = T.dropEnd (T.length magicHashes) ident
   in if T.null ident || T.null baseIdent then Nothing else Just (baseIdent, magicHashes)

-------------------------------------------------------------------------------
-- Constructor identifiers (uppercase-starting names)
-------------------------------------------------------------------------------

-- | Generate a constructor/type constructor name starting with uppercase.
-- Produces names like @Foo@, @A1@, @T'x@, etc.
genConId :: Gen Text
genConId = do
  first <- elements conIdentStartChars
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements identTailChars)
  hashCount <- chooseInt (0, 4)
  pure (T.pack (first : rest) <> T.replicate hashCount "#")

shrinkConIdent :: Text -> [Text]
shrinkConIdent = shrinkWithPreservedFirstChar isValidConIdent

isValidConIdent :: Text -> Bool
isValidConIdent ident =
  case unsnocMagicHash ident of
    Just (baseIdent, _magicHashes) ->
      case T.uncons baseIdent of
        Just (first, rest) ->
          isValidConIdentStartChar first
            && T.all isValidIdentTailChar rest
        Nothing -> False
    Nothing -> False

-------------------------------------------------------------------------------
-- Constructor operator symbols
-------------------------------------------------------------------------------

-- | Generate a constructor operator symbol (starting with @:@).
-- Examples: @:+@, @:*@, @:==@, @:+:@
-- Rejects @:@ (built-in list cons) and @::@ (type signature operator).
genConSym :: Gen Text
genConSym = do
  restLen <- chooseInt (1, 3)
  rest <- vectorOf restLen (elements symbolChars)
  let op = T.pack (':' : rest)
  if isValidGeneratedConSym op then pure op else genConSym

isValidGeneratedConSym :: Text -> Bool
isValidGeneratedConSym op =
  case T.uncons op of
    Just (':', rest) -> not (T.null rest) && T.all isValidSymbolChar rest && op `Set.notMember` reservedOperators
    _ -> False

genVarSym :: Gen Text
genVarSym = do
  first <- elements varSymStartChars
  restLen <- chooseInt (0, 3)
  rest <- vectorOf restLen (elements symbolChars)
  let op = T.pack (first : rest)
  if isValidGeneratedVarSym op then pure op else genVarSym

isValidGeneratedVarSym :: Text -> Bool
isValidGeneratedVarSym op =
  case T.uncons op of
    Just (first, rest) ->
      first /= ':'
        && isValidSymbolChar first
        && T.all (/= '`') op
        && T.all isValidSymbolChar rest
        && op `Set.notMember` reservedOperators
        && not (isDashRun op)
    Nothing -> False

-------------------------------------------------------------------------------
-- Module qualifiers
-------------------------------------------------------------------------------

-- | Generate an optional module qualifier (e.g., Nothing or Just "Data.List").
-- Biased towards Nothing to keep most names unqualified.
genOptionalQualifier :: Gen (Maybe Text)
genOptionalQualifier =
  oneof
    [ pure Nothing,
      Just <$> genModuleQualifier
    ]

-- | Generate a module qualifier like "Data.List" or "Prelude".
genModuleQualifier :: Gen Text
genModuleQualifier = do
  segCount <- chooseInt (1, 3)
  segs <- vectorOf segCount genModuleSegment
  pure (T.intercalate "." segs)

-- | Generate a single module name segment (starts with uppercase). Contrary to conids, module segments may not contain magic hashes.
genModuleSegment :: Gen Text
genModuleSegment = T.filter (/= '#') <$> genConId

-------------------------------------------------------------------------------
-- Field names
-------------------------------------------------------------------------------

-- | Generate a record field name (lowercase-starting identifier).
-- Rejects reserved identifiers and extension-reserved identifiers.
genFieldName :: Gen Text
genFieldName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier allExtensions candidate
    then genFieldName
    else pure candidate

-------------------------------------------------------------------------------
-- Quasi-quotation helpers
-------------------------------------------------------------------------------

-- | Generate a quasi-quoter name, excluding TH bracket names (e, d, p, t).
genQuoterName :: Gen Text
genQuoterName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 4)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isValidQuoterName candidate
    then pure candidate
    else genQuoterName

isValidQuoterName :: Text -> Bool
isValidQuoterName name =
  case T.uncons name of
    Just (first, rest) ->
      (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'.")) rest
        -- Exclude names that clash with TH quote brackets
        && name `notElem` ["e", "t", "d", "p"]
    Nothing -> False

-- | Generate a quasi-quotation body (safe characters only).
genQuasiBody :: Gen Text
genQuasiBody = do
  len <- chooseInt (0, 10)
  chars <- vectorOf len (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " +-*/_()"))
  pure (T.pack chars)

-------------------------------------------------------------------------------
-- Character and string generators
-------------------------------------------------------------------------------

-- | Generate a printable character safe for use in literals.
genCharValue :: Gen Char
genCharValue = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _")

-- | Generate a string value for use in string literals.
genStringValue :: Gen Text
genStringValue = do
  len <- chooseInt (0, 8)
  T.pack <$> vectorOf len genCharValue

-------------------------------------------------------------------------------
-- Numeric helpers
-------------------------------------------------------------------------------

-- | Generate a decimal value with one decimal digit of precision.
genTenths :: Gen Rational
genTenths = do
  whole <- chooseInteger (0, 99)
  frac <- chooseInteger (0, 9)
  pure (fromInteger whole + fromInteger frac / 10)

-- | Show an integer as a hexadecimal string (without prefix).
showHex :: Integer -> String
showHex value
  | value < 16 = [hexDigit value]
  | otherwise = showHex (value `div` 16) <> [hexDigit (value `mod` 16)]
  where
    hexDigit x = "0123456789abcdef" !! fromInteger x

-- | Shrink a floating-point value, maintaining one decimal digit of precision.
shrinkFloat :: Rational -> [Rational]
shrinkFloat value =
  [fromInteger shrunk / 10 | shrunk <- shrinkIntegral (round (value * 10) :: Integer), shrunk >= 0]

isValidGeneratedIdentStartChar :: Char -> Bool
isValidGeneratedIdentStartChar c = c == '_' || generalCategory c == LowercaseLetter

isValidConIdentStartChar :: Char -> Bool
isValidConIdentStartChar c = generalCategory c `elem` [UppercaseLetter, TitlecaseLetter]

isValidIdentNumberChar :: Char -> Bool
isValidIdentNumberChar c =
  case generalCategory c of
    DecimalNumber -> True
    OtherNumber -> True
    _ -> False

isValidIdentTailChar :: Char -> Bool
isValidIdentTailChar c = c == '\'' || isValidGeneratedIdentStartChar c || isValidConIdentStartChar c || isValidIdentNumberChar c

isValidSymbolChar :: Char -> Bool
isValidSymbolChar c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isValidUnicodeSymbolChar c && c /= '`'

isValidUnicodeSymbolChar :: Char -> Bool
isValidUnicodeSymbolChar c =
  case generalCategory c of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    OtherPunctuation -> c > '\x7f'
    _ -> False

isDashRun :: Text -> Bool
isDashRun op = T.length op >= 2 && T.all (== '-') op

shrinkWithPreservedFirstChar :: (Text -> Bool) -> Text -> [Text]
shrinkWithPreservedFirstChar isValid name =
  case T.uncons name of
    Just (first, rest) ->
      [ candidate
      | shrunkRest <- shrink (T.unpack rest),
        let candidate = T.pack (first : shrunkRest),
        isValid candidate
      ]
    Nothing -> []
