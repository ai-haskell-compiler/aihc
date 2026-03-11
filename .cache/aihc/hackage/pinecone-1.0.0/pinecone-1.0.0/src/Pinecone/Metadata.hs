-- | Pinecone's filtering query language
module Pinecone.Metadata
    ( Scalar(..)
    , Filter(..)
    ) where

import Pinecone.Prelude

-- | A scalar value used for metadata filters
data Scalar
    = ScalarNumber Scientific
    | ScalarString Text
    | ScalarBoolean Bool
    deriving stock (Eq, Generic, Show)

instance IsString Scalar where
    fromString string = ScalarString (fromString string)

instance Num Scalar where
    fromInteger n = ScalarNumber (fromInteger n)

    ScalarNumber l + ScalarNumber r = ScalarNumber (l + r)
    _ + ScalarNumber r = ScalarNumber r
    l + _ = l

    ScalarNumber l * ScalarNumber r = ScalarNumber (l * r)
    _ * ScalarNumber r = ScalarNumber r
    l * _ = l

    abs (ScalarNumber n) = ScalarNumber (abs n)
    abs n = n

    signum (ScalarNumber n) = ScalarNumber (signum n)
    signum n = n

    negate (ScalarNumber n) = ScalarNumber (negate n)
    negate n = n

instance ToJSON Scalar where
    toJSON (ScalarNumber scientific) = Number scientific
    toJSON (ScalarString text) = String text
    toJSON (ScalarBoolean bool) = Bool bool

instance FromJSON Scalar where
    parseJSON (Number scientific) = pure (ScalarNumber scientific)
    parseJSON (String text) = pure (ScalarString text)
    parseJSON (Bool bool) = pure (ScalarBoolean bool)
    parseJSON object = typeMismatch "Number/String/Boolean" object

-- | Metadata query language
data Filter
    = Equal Text Scalar
    | NotEqual Text Scalar
    | GreaterThan Text Scientific
    | GreaterThanOrEqual Text Scientific
    | LessThan Text Scientific
    | LessThanOrEqual Text Scientific
    | In Text Scalar
    | NotIn Text Scalar
    | Exists Text Bool
    | And (Vector Filter)
    | Or (Vector Filter)
    deriving stock (Eq, Generic, Show)

instance ToJSON Filter where
    toJSON (Equal field literal) =
        toJSON @(Map Text (Map Text Scalar)) [ (field, [ ("$eq", literal) ]) ]
    toJSON (NotEqual field literal) =
        toJSON @(Map Text (Map Text Scalar)) [ (field, [ ("$ne", literal) ]) ]
    toJSON (GreaterThan field literal) =
        toJSON @(Map Text (Map Text Scientific)) [ (field, [ ("$gt", literal) ]) ]
    toJSON (GreaterThanOrEqual field literal) =
        toJSON @(Map Text (Map Text Scientific)) [ (field, [ ("$gte", literal) ]) ]
    toJSON (LessThan field literal) =
        toJSON @(Map Text (Map Text Scientific)) [ (field, [ ("$lt", literal) ]) ]
    toJSON (LessThanOrEqual field literal) =
        toJSON @(Map Text (Map Text Scientific)) [ (field, [ ("$lte", literal) ]) ]
    toJSON (In field literal) =
        toJSON @(Map Text (Map Text Scalar)) [ (field, [ ("$in", literal) ]) ]
    toJSON (NotIn field literal) =
        toJSON @(Map Text (Map Text Scalar)) [ (field, [ ("$nin", literal) ]) ]
    toJSON (Exists field literal) =
        toJSON @(Map Text (Map Text Bool)) [ (field, [ ("$exists", literal) ]) ]
    toJSON (And clauses) =
        toJSON @(Map Text (Vector Filter)) [ ("$and", clauses) ]
    toJSON (Or clauses) =
        toJSON @(Map Text (Vector Filter)) [ ("$or", clauses) ]

instance FromJSON Filter where
    parseJSON v =
            parseEqual v
        <|> parseNotEqual v
        <|> parseGreaterThan v
        <|> parseGreaterThanOrEqual v
        <|> parseLessThan v
        <|> parseLessThanOrEqual v
        <|> parseIn v
        <|> parseNotIn v
        <|> parseAnd v
        <|> parseOr v
      where
        parseEqual value = do
            [ (field, [ ("$eq", literal) ]) ] <- parseJSON @(Map Text (Map Text Scalar)) value
            return (Equal field literal)

        parseNotEqual value = do
            [ (field, [ ("$ne", literal) ]) ] <- parseJSON @(Map Text (Map Text Scalar)) value
            return (NotEqual field literal)

        parseGreaterThan value = do
            [ (field, [ ("$gt", literal) ]) ] <- parseJSON @(Map Text (Map Text Scientific)) value
            return (GreaterThan field literal)

        parseGreaterThanOrEqual value = do
            [ (field, [ ("$gte", literal) ]) ] <- parseJSON @(Map Text (Map Text Scientific)) value
            return (GreaterThanOrEqual field literal)

        parseLessThan value = do
            [ (field, [ ("$lt", literal) ]) ] <- parseJSON @(Map Text (Map Text Scientific)) value
            return (LessThan field literal)

        parseLessThanOrEqual value = do
            [ (field, [ ("$lte", literal) ]) ] <- parseJSON @(Map Text (Map Text Scientific)) value
            return (LessThanOrEqual field literal)

        parseIn value = do
            [ (field, [ ("$in", literal) ]) ] <- parseJSON @(Map Text (Map Text Scalar)) value
            return (In field literal)

        parseNotIn value = do
            [ (field, [ ("$nin", literal) ]) ] <- parseJSON @(Map Text (Map Text Scalar)) value
            return (NotIn field literal)

        parseAnd value = do
            [ ("$and", clauses) ] <- parseJSON @(Map Text (Vector Filter)) value
            return (And clauses)

        parseOr value = do
            [ ("$or", clauses) ] <- parseJSON @(Map Text (Vector Filter)) value
            return (Or clauses)
