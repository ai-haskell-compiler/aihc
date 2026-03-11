{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | After the document is parsed, before getting executed, the AST is
-- transformed into a similar, simpler AST. Performed transformations include:
--
--   * Replacing variables with their values.
--   * Inlining fragments. Some fragments can be completely eliminated and
--   replaced by the selection set they represent. Invalid (recursive and
--   non-existing) fragments are skipped. The most fragments are inlined, so the
--   executor doesn't have to perform additional lookups later.
--   * Evaluating directives (@\@include@ and @\@skip@).
--
-- This module is also responsible for smaller rewrites that touch only parts of
-- the original AST.
module Language.GraphQL.Execute.Transform
    ( Field(..)
    , Fragment(..)
    , Input(..)
    , Operation(..)
    , Replacement(..)
    , Selection(..)
    , TransformT(..)
    , document
    , transform
    ) where

import Control.Monad (foldM)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), local)
import qualified Control.Monad.Trans.Reader as Reader
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int32)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.AST.Document as Full
import Language.GraphQL.Type.Schema (Type)
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.Internal as Type
import Numeric (showFloat)

-- | Associates a fragment name with a list of 'Field's.
data Replacement m = Replacement
    { variableValues :: Type.Subs
    , fragmentDefinitions :: HashMap Full.Name Full.FragmentDefinition
    , visitedFragments :: HashSet Full.Name
    , types :: HashMap Full.Name (Type m)
    }

newtype TransformT m a = TransformT
    { runTransformT :: ReaderT (Replacement m) m a
    }

instance Functor m => Functor (TransformT m) where
    fmap f = TransformT . fmap f . runTransformT

instance Applicative m => Applicative (TransformT m) where
    pure = TransformT . pure
    TransformT f <*> TransformT x = TransformT $ f <*> x

instance Monad m => Monad (TransformT m) where
    TransformT x >>= f = TransformT $ x >>= runTransformT . f

instance MonadTrans TransformT where
    lift = TransformT . lift

instance MonadThrow m => MonadThrow (TransformT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (TransformT m) where
  catch (TransformT stack) handler =
      TransformT $ catch stack $ runTransformT . handler

asks :: Monad m => forall a. (Replacement m -> a) -> TransformT m a
asks = TransformT . Reader.asks

-- | GraphQL has 3 operation types: queries, mutations and subscribtions.
data Operation m
    = Operation Full.OperationType (Seq (Selection m)) Full.Location

-- | Field or inlined fragment.
data Selection m
    = FieldSelection (Field m)
    | FragmentSelection (Fragment m)

data Field m = Field
    (Maybe Full.Name)
    Full.Name
    (HashMap Full.Name (Full.Node Input))
    (Seq (Selection m))
    Full.Location

data Fragment m = Fragment
    (Type.CompositeType m) (Seq (Selection m)) Full.Location

data Input
    = Variable Type.Value
    | Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Null
    | Enum Full.Name
    | List [Input]
    | Object (HashMap Full.Name Input)
    deriving Eq

instance Show Input where
    showList = mappend . showList'
      where
        showList' list = "[" ++ intercalate ", " (show <$> list) ++ "]"
    show (Int integer) = show integer
    show (Float float') = showFloat float' mempty
    show (String text) = "\"" <> Text.foldr (mappend . Full.escape) "\"" text
    show (Boolean boolean') = show boolean'
    show Null = "null"
    show (Enum name) = Text.unpack name
    show (List list) = show list
    show (Object fields) = unwords
        [ "{"
        , intercalate ", " (HashMap.foldrWithKey showObject [] fields)
        , "}"
        ]
      where
        showObject key value accumulator =
            concat [Text.unpack key, ": ", show value] : accumulator
    show variableValue = show variableValue

-- | Extracts operations and fragment definitions of the document.
document :: Full.Document
    -> ([Full.OperationDefinition], HashMap Full.Name Full.FragmentDefinition)
document = foldr filterOperation ([], HashMap.empty)
  where
    filterOperation (Full.ExecutableDefinition executableDefinition) accumulator
        | Full.DefinitionOperation operationDefinition' <- executableDefinition =
            first (operationDefinition' :) accumulator
        | Full.DefinitionFragment fragmentDefinition <- executableDefinition
        , Full.FragmentDefinition fragmentName _ _ _ _ <- fragmentDefinition =
            HashMap.insert fragmentName fragmentDefinition <$> accumulator
    filterOperation _ accumulator = accumulator -- Type system definitions.

-- | Rewrites the original syntax tree into an intermediate representation used
-- for the query execution.
transform :: Monad m => Full.OperationDefinition -> TransformT m (Operation m)
transform (Full.OperationDefinition operationType _ _ _ selectionSet' operationLocation) = do
    transformedSelections <- selectionSet selectionSet'
    pure $ Operation operationType transformedSelections operationLocation
transform (Full.SelectionSet selectionSet' operationLocation) = do
    transformedSelections <- selectionSet selectionSet'
    pure $ Operation Full.Query transformedSelections operationLocation

selectionSet :: Monad m => Full.SelectionSet -> TransformT m (Seq (Selection m))
selectionSet = selectionSetOpt . NonEmpty.toList

selectionSetOpt :: Monad m => Full.SelectionSetOpt -> TransformT m (Seq (Selection m))
selectionSetOpt = foldM go Seq.empty
  where
    go accumulatedSelections currentSelection =
        selection currentSelection <&> (accumulatedSelections ><)

selection :: Monad m => Full.Selection -> TransformT m (Seq (Selection m))
selection (Full.FieldSelection field') =
    maybeToSelectionSet FieldSelection $ field field'
selection (Full.FragmentSpreadSelection fragmentSpread') =
    maybeToSelectionSet FragmentSelection $ fragmentSpread fragmentSpread'
selection (Full.InlineFragmentSelection inlineFragment') =
    either id (pure . FragmentSelection) <$> inlineFragment inlineFragment'

maybeToSelectionSet :: Monad m
    => forall a
    . (a -> Selection m)
    -> TransformT m (Maybe a)
    -> TransformT m (Seq (Selection m))
maybeToSelectionSet selectionType = fmap (maybe Seq.empty $ pure . selectionType)

directives :: Monad m => [Full.Directive] -> TransformT m (Maybe [Definition.Directive])
directives = fmap Type.selection . traverse directive

inlineFragment :: Monad m
    => Full.InlineFragment
    -> TransformT m (Either (Seq (Selection m)) (Fragment m))
inlineFragment (Full.InlineFragment maybeCondition directives' selectionSet' location)
    | Just typeCondition <- maybeCondition = do
        transformedSelections <- selectionSet selectionSet'
        transformedDirectives <- directives directives'
        maybeFragmentType <- asks
            $ Type.lookupTypeCondition typeCondition
            . types
        pure $ case transformedDirectives >> maybeFragmentType of
            Just fragmentType -> Right
                $ Fragment fragmentType transformedSelections location
            Nothing -> Left Seq.empty
    | otherwise = do
        transformedSelections <- selectionSet selectionSet'
        transformedDirectives <- directives directives'
        pure $ if isJust transformedDirectives
            then Left transformedSelections
            else Left Seq.empty

fragmentSpread :: Monad m => Full.FragmentSpread -> TransformT m (Maybe (Fragment m))
fragmentSpread (Full.FragmentSpread spreadName directives' location) = do
    transformedDirectives <- directives directives'
    visitedFragment <- asks $ HashSet.member spreadName . visitedFragments
    possibleFragmentDefinition <- asks
        $ HashMap.lookup spreadName
        . fragmentDefinitions
    case transformedDirectives >> possibleFragmentDefinition of
        Just (Full.FragmentDefinition _ typeCondition _ selections _)
            | visitedFragment -> pure Nothing
            | otherwise -> do
                fragmentType <- asks
                    $ Type.lookupTypeCondition typeCondition
                    . types
                traverse (traverseSelections selections) fragmentType
        Nothing -> pure Nothing
  where
    traverseSelections selections typeCondition = do
        transformedSelections <- TransformT
            $ local fragmentInserter
            $ runTransformT
            $ selectionSet selections
        pure $ Fragment typeCondition transformedSelections location
    fragmentInserter replacement@Replacement{ visitedFragments } = replacement
        { visitedFragments = HashSet.insert spreadName visitedFragments }

field :: Monad m => Full.Field -> TransformT m (Maybe (Field m))
field (Full.Field alias' name' arguments' directives' selectionSet' location') = do
    transformedSelections <- selectionSetOpt selectionSet'
    transformedDirectives <- directives directives'
    transformedArguments <- arguments arguments'
    let transformedField = Field
            alias'
            name'
            transformedArguments
            transformedSelections
            location'
    pure $ transformedDirectives >> pure transformedField

arguments :: Monad m => [Full.Argument] -> TransformT m (HashMap Full.Name (Full.Node Input))
arguments = foldM go HashMap.empty
  where
    go accumulator (Full.Argument name' valueNode argumentLocation) = do
        let replaceLocation = flip Full.Node argumentLocation . Full.node
        argumentValue <- fmap replaceLocation <$> node valueNode
        pure $ insertIfGiven name' argumentValue accumulator

directive :: Monad m => Full.Directive -> TransformT m Definition.Directive
directive (Full.Directive name' arguments' _)
    = Definition.Directive name'
    . Type.Arguments
    <$> foldM go HashMap.empty arguments'
  where
    go accumulator (Full.Argument argumentName Full.Node{ node = node' } _) = do
        transformedValue <- directiveValue node'
        pure $ HashMap.insert argumentName transformedValue accumulator

directiveValue :: Monad m => Full.Value -> TransformT m Type.Value
directiveValue = \case
    (Full.Variable name') -> asks
        $ HashMap.lookupDefault Type.Null name'
        . variableValues
    (Full.Int integer) -> pure $ Type.Int integer
    (Full.Float double) -> pure $ Type.Float double
    (Full.String string) -> pure $ Type.String string
    (Full.Boolean boolean) -> pure $ Type.Boolean boolean
    Full.Null -> pure Type.Null
    (Full.Enum enum) -> pure $ Type.Enum enum
    (Full.List list) -> Type.List <$> traverse directiveNode list
    (Full.Object objectFields) ->
        Type.Object <$> foldM objectField HashMap.empty objectFields
  where
    directiveNode Full.Node{ node = node'} = directiveValue node'
    objectField accumulator Full.ObjectField{ name, value } = do
        transformedValue <- directiveNode value
        pure $ HashMap.insert name transformedValue accumulator

input :: Monad m => Full.Value -> TransformT m (Maybe Input)
input (Full.Variable name') =
    asks (HashMap.lookup name' . variableValues) <&> fmap Variable
input (Full.Int integer) = pure $ Just $ Int integer
input (Full.Float double) = pure $ Just $ Float double
input (Full.String string) = pure $ Just $ String string
input (Full.Boolean boolean) = pure $ Just $ Boolean boolean
input Full.Null = pure $ Just Null
input (Full.Enum enum) = pure $ Just $ Enum enum
input (Full.List list) = Just . List
    <$> traverse (fmap (fromMaybe Null) . input . Full.node) list
input (Full.Object objectFields) = Just . Object
    <$> foldM objectField HashMap.empty objectFields
  where
    objectField accumulator Full.ObjectField{..} = do
        objectFieldValue <- fmap Full.node <$> node value
        pure $ insertIfGiven name objectFieldValue accumulator

insertIfGiven :: forall a
    . Full.Name
    -> Maybe a
    -> HashMap Full.Name a
    -> HashMap Full.Name a
insertIfGiven name (Just v) = HashMap.insert name v
insertIfGiven _ _ = id

node :: Monad m => Full.Node Full.Value -> TransformT m (Maybe (Full.Node Input))
node Full.Node{node = node', ..} =
    traverse Full.Node <$> input node' <*> pure location

