{-# LANGUAGE OverloadedStrings #-}

-- | The inferred type signatures of a module.
--
-- The type checker gives each top-level binding a type. This module writes
-- the type as a signature line above each binding that has none, so the
-- reader sees the source with the types the compiler inferred.
module Aihc.Play.Types
  ( Signature (..),
    inferredSignatures,
    withSignatures,
    signatureJson,
    renderPlainType,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Module (..),
    Pattern (..),
    SourceSpan,
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    moduleName,
    sourceSpanStartLine,
  )
import Aihc.Play.Json (Json (..))
import Aihc.Tc (TcBindingResult (..), TcType, TcWiring, renderTcTypeInModule, tcModuleBindings)
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isUpper)
import Data.Data (Data, cast, gmapQ)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data Signature = Signature
  { -- | The source line of the binding, 1-based. The signature goes above it.
    signatureLine :: !Int,
    signatureName :: !Text,
    signatureType :: !Text
  }
  deriving (Eq, Show)

-- | The signatures of the top-level bindings that have no signature in the
-- source, in source order. The module is the output of the type checker.
inferredSignatures :: TcWiring -> Module -> [Signature]
inferredSignatures wiring modu =
  sortOn
    signatureLine
    [ Signature line name (renderPlainType current ty)
    | (line, names) <- bindings,
      name <- names,
      not (Set.member name declared),
      Just ty <- [Map.lookup name types]
    ]
  where
    current = moduleName modu
    types = Map.fromList [(tbDisplayName result, tbType result) | result <- tcModuleBindings wiring modu]
    declared = Set.fromList (concatMap signatureNames (moduleDecls modu))
    bindings = concatMap binding (moduleDecls modu)

-- | The names a type signature declaration covers.
signatureNames :: Decl -> [Text]
signatureNames decl =
  case decl of
    DeclAnn _ inner -> signatureNames inner
    DeclTypeSig names _ -> map unqualifiedNameText names
    _ -> []

-- | The start line and the binders of a value declaration.
binding :: Decl -> [(Int, [Text])]
binding = go Nothing
  where
    go span' decl =
      case decl of
        DeclAnn ann inner -> go (fromAnnotation ann <|> span') inner
        DeclValue (FunctionBind name _) -> [(line span', [unqualifiedNameText name])]
        DeclValue (PatternBind _ pat _) -> [(line span', patternBinders pat)]
        _ -> []
    line :: Maybe SourceSpan -> Int
    line = maybe 1 sourceSpanStartLine

-- | The variables a pattern binds, in source order.
patternBinders :: Pattern -> [Text]
patternBinders = walk
  where
    walk :: forall a. (Data a) => a -> [Text]
    walk value
      | Just (PVar name) <- cast value = [unqualifiedNameText name]
      | otherwise = concat (gmapQ walk value)

-- | The source with a signature line above each binding.
withSignatures :: Text -> [Signature] -> Text
withSignatures source signatures = T.unlines (go 1 (T.lines source) signatures)
  where
    go _ rest [] = rest
    go _ [] pending = map signatureLineText pending
    go n (line : rest) pending =
      let (here, later) = span ((<= n) . signatureLine) pending
       in map signatureLineText here <> (line : go (n + 1) rest later)

signatureLineText :: Signature -> Text
signatureLineText signature = binderText (signatureName signature) <> " :: " <> signatureType signature

-- | An operator binder in prefix position.
binderText :: Text -> Text
binderText name
  | Just (first, _) <- T.uncons name, isAlphaNum first || first == '_' = name
  | otherwise = "(" <> name <> ")"

signatureJson :: Signature -> Json
signatureJson signature =
  JObject
    [ ("line", JInt (signatureLine signature)),
      ("name", JString (signatureName signature)),
      ("type", JString (signatureType signature))
    ]

-- | A type in source spelling: ASCII arrows, no module qualifiers, and no
-- @forall@ at the front, as a signature in a source file spells it.
renderPlainType :: Maybe Text -> TcType -> Text
renderPlainType current =
  dropForall . T.unwords . map unqualify . T.words . asciiArrows . T.pack . renderTcTypeInModule current
  where
    asciiArrows =
      T.replace "→" "->" . T.replace "⇒" "=>" . T.replace "∀" "forall" . T.replace "∷" "::"
    dropForall rendered =
      case T.stripPrefix "forall " rendered of
        Just rest -> T.strip (T.drop 1 (T.dropWhile (/= '.') rest))
        Nothing -> rendered

-- | Drop the module of a qualified name, so @GHC.Types.Int@ reads as @Int@.
unqualify :: Text -> Text
unqualify word =
  case T.breakOnEnd "." (T.filter (`notElem` ("()[]," :: String)) word) of
    (prefix, rest)
      | not (T.null prefix),
        not (T.null rest),
        all isModuleSegment (T.splitOn "." (T.dropEnd 1 prefix)) ->
          T.replace (T.dropEnd 1 prefix <> "." <> rest) rest word
    _ -> word
  where
    isModuleSegment segment = maybe False (isUpper . fst) (T.uncons segment)
