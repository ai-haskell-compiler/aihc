{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains default rules defined in the GraphQL specification.
module Language.GraphQL.Validate.Rules
    ( directivesInValidLocationsRule
    , executableDefinitionsRule
    , fieldsOnCorrectTypeRule
    , fragmentsOnCompositeTypesRule
    , fragmentSpreadTargetDefinedRule
    , fragmentSpreadTypeExistenceRule
    , loneAnonymousOperationRule
    , knownArgumentNamesRule
    , knownDirectiveNamesRule
    , knownInputFieldNamesRule
    , noFragmentCyclesRule
    , noUndefinedVariablesRule
    , noUnusedFragmentsRule
    , noUnusedVariablesRule
    , overlappingFieldsCanBeMergedRule
    , possibleFragmentSpreadsRule
    , providedRequiredInputFieldsRule
    , providedRequiredArgumentsRule
    , scalarLeafsRule
    , singleFieldSubscriptionsRule
    , specifiedRules
    , uniqueArgumentNamesRule
    , uniqueDirectiveNamesRule
    , uniqueFragmentNamesRule
    , uniqueInputFieldNamesRule
    , uniqueOperationNamesRule
    , uniqueVariableNamesRule
    , valuesOfCorrectTypeRule
    , variablesInAllowedPositionRule
    , variablesAreInputTypesRule
    ) where

import Control.Monad ((>=>), foldM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks, mapReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.Bifunctor (first)
import Data.Foldable (Foldable(..), find)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Records (HasField(..))
import qualified Language.GraphQL.AST.Document as Full
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.Internal as Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Validate.Validation

-- Local help type that contains a hash set to track visited fragments.
type ValidationState m a =
    StateT (HashSet Full.Name) (ReaderT (Validation m) Seq) a

-- | Default rules given in the specification.
specifiedRules :: forall m. [Rule m]
specifiedRules =
    -- Documents.
    [ executableDefinitionsRule
    -- Operations.
    , singleFieldSubscriptionsRule
    , loneAnonymousOperationRule
    , uniqueOperationNamesRule
    -- Fields
    , fieldsOnCorrectTypeRule
    , scalarLeafsRule
    , overlappingFieldsCanBeMergedRule
    -- Arguments.
    , knownArgumentNamesRule
    , uniqueArgumentNamesRule
    , providedRequiredArgumentsRule
    -- Fragments.
    , uniqueFragmentNamesRule
    , fragmentSpreadTypeExistenceRule
    , fragmentsOnCompositeTypesRule
    , noUnusedFragmentsRule
    , fragmentSpreadTargetDefinedRule
    , noFragmentCyclesRule
    , possibleFragmentSpreadsRule
    -- Values
    , valuesOfCorrectTypeRule
    , knownInputFieldNamesRule
    , uniqueInputFieldNamesRule
    , providedRequiredInputFieldsRule
    -- Directives.
    , knownDirectiveNamesRule
    , directivesInValidLocationsRule
    , uniqueDirectiveNamesRule
    -- Variables.
    , uniqueVariableNamesRule
    , variablesAreInputTypesRule
    , noUndefinedVariablesRule
    , noUnusedVariablesRule
    , variablesInAllowedPositionRule
    ]

-- | Definition must be OperationDefinition or FragmentDefinition.
executableDefinitionsRule :: forall m. Rule m
executableDefinitionsRule = DefinitionRule $ \case
    Full.ExecutableDefinition _ -> lift mempty
    Full.TypeSystemDefinition _ location' -> pure $ error' location'
    Full.TypeSystemExtension _ location' -> pure $ error' location'
  where
    error' location' = Error
        { message =
            "Definition must be OperationDefinition or FragmentDefinition."
        , locations = [location']
        }

-- | Subscription operations must have exactly one root field.
singleFieldSubscriptionsRule :: forall m. Rule m
singleFieldSubscriptionsRule = OperationDefinitionRule $ \case
    Full.OperationDefinition Full.Subscription name' _ _ rootFields location' -> do
        groupedFieldSet <- evalStateT (collectFields rootFields) HashSet.empty
        case HashSet.toList groupedFieldSet of
            [rootName]
                | Text.isPrefixOf "__" rootName -> makeError location' name'
                    "exactly one top level field, which must not be an introspection field."
                | otherwise -> lift mempty
            [] -> makeError location' name' "exactly one top level field."
            _ -> makeError location' name' "only one top level field."
    _ -> lift mempty
  where
    makeError location' (Just operationName) errorLine = pure $ Error
        { message = concat
            [ "Subscription \""
            , Text.unpack operationName
            , "\" must select "
            , errorLine
            ]
        , locations = [location']
        }
    makeError location' Nothing errorLine = pure $ Error
        { message = "Anonymous Subscription must select " <> errorLine
        , locations = [location']
        }
    collectFields = foldM forEach HashSet.empty
    forEach accumulator = \case
        Full.FieldSelection fieldSelection -> forField accumulator fieldSelection
        Full.FragmentSpreadSelection fragmentSelection ->
            forSpread accumulator fragmentSelection
        Full.InlineFragmentSelection fragmentSelection ->
            forInline accumulator fragmentSelection
    forField accumulator (Full.Field alias name _ directives' _ _)
        | any skip directives' = pure accumulator
        | Just aliasedName <- alias = pure
            $ HashSet.insert aliasedName accumulator
        | otherwise = pure $ HashSet.insert name accumulator
    forSpread accumulator (Full.FragmentSpread fragmentName directives' _)
        | any skip directives' = pure accumulator
        | otherwise = do
            inVisitetFragments <- gets $ HashSet.member fragmentName
            if inVisitetFragments
               then pure accumulator
               else collectFromSpread fragmentName accumulator
    forInline accumulator (Full.InlineFragment maybeType directives' selections _)
        | any skip directives' = pure accumulator
        | Just typeCondition <- maybeType =
            collectFromFragment typeCondition selections accumulator
        | otherwise = HashSet.union accumulator
            <$> collectFields selections
    skip (Full.Directive "skip" [Full.Argument "if" (Full.Node argumentValue _) _] _) =
        Full.Boolean True == argumentValue
    skip (Full.Directive "include" [Full.Argument "if" (Full.Node argumentValue _) _] _) =
        Full.Boolean False == argumentValue
    skip _ = False
    collectFromFragment typeCondition selectionSet accumulator = do
        types' <- lift $ asks $ Schema.types . schema
        schema' <- lift $ asks schema
        case Type.lookupTypeCondition typeCondition types' of
            Nothing -> pure accumulator
            Just compositeType
                | Just objectType <- Schema.subscription schema'
                , True <- Type.doesFragmentTypeApply compositeType objectType ->
                    HashSet.union accumulator <$> collectFields selectionSet
                | otherwise -> pure accumulator
    collectFromSpread fragmentName accumulator = do
        modify $ HashSet.insert fragmentName
        ast' <- lift $ asks ast
        case findFragmentDefinition fragmentName ast' of
            Nothing -> pure accumulator
            Just (Full.FragmentDefinition _ typeCondition _ selectionSet _) ->
                collectFromFragment typeCondition selectionSet accumulator

-- | GraphQL allows a short‐hand form for defining query operations when only
-- that one operation exists in the document.
loneAnonymousOperationRule :: forall m. Rule m
loneAnonymousOperationRule = OperationDefinitionRule $ \case
      Full.SelectionSet _ thisLocation -> check thisLocation
      Full.OperationDefinition _ Nothing _ _ _ thisLocation ->
          check thisLocation
      _ -> lift mempty
    where
      check thisLocation = asks ast
          >>= lift . foldr (filterAnonymousOperations thisLocation) mempty
      filterAnonymousOperations thisLocation definition Empty
          | (viewOperation -> Just operationDefinition) <- definition =
              compareAnonymousOperations thisLocation operationDefinition
      filterAnonymousOperations _ _ accumulator = accumulator
      compareAnonymousOperations thisLocation = \case
          Full.OperationDefinition _ _ _ _ _ thatLocation
              | thisLocation /= thatLocation -> pure $ error' thisLocation
          Full.SelectionSet _ thatLocation
              | thisLocation /= thatLocation -> pure $ error' thisLocation
          _ -> mempty
      error' location' = Error
          { message =
              "This anonymous operation must be the only defined operation."
          , locations = [location']
          }

-- | Each named operation definition must be unique within a document when
-- referred to by its name.
uniqueOperationNamesRule :: forall m. Rule m
uniqueOperationNamesRule = OperationDefinitionRule $ \case
    Full.OperationDefinition _ (Just thisName) _ _ _ thisLocation ->
        findDuplicates (filterByName thisName) thisLocation (error' thisName)
    _ -> lift mempty
  where
    error' operationName = concat
        [ "There can be only one operation named \""
        , Text.unpack operationName
        , "\"."
        ]
    filterByName thisName definition' accumulator
        | (viewOperation -> Just operationDefinition) <- definition'
        , Full.OperationDefinition _ (Just thatName) _ _ _ thatLocation <- operationDefinition
        , thisName == thatName = thatLocation : accumulator
        | otherwise = accumulator

findDuplicates :: (Full.Definition -> [Full.Location] -> [Full.Location])
    -> Full.Location
    -> String
    -> RuleT m
findDuplicates filterByName thisLocation errorMessage =
    asks ast >>= go . foldr filterByName []
  where
    go locations' =
        case locations' of
            headLocation : otherLocations -- length locations' > 1
                | not $ null otherLocations
                , headLocation == thisLocation -> pure $ makeError locations'
            _ -> lift mempty
    makeError locations' = Error
        { message = errorMessage
        , locations = locations'
        }

viewOperation :: Full.Definition -> Maybe Full.OperationDefinition
viewOperation definition
    | Full.ExecutableDefinition executableDefinition <- definition
    , Full.DefinitionOperation operationDefinition <- executableDefinition =
        Just operationDefinition
viewOperation _ = Nothing

viewFragment :: Full.Definition -> Maybe Full.FragmentDefinition
viewFragment definition
    | Full.ExecutableDefinition executableDefinition <- definition
    , Full.DefinitionFragment fragmentDefinition <- executableDefinition =
        Just fragmentDefinition
viewFragment _ = Nothing

-- | Fragment definitions are referenced in fragment spreads by name. To avoid
-- ambiguity, each fragment’s name must be unique within a document.
--
-- Inline fragments are not considered fragment definitions, and are unaffected
-- by this validation rule.
uniqueFragmentNamesRule :: forall m. Rule m
uniqueFragmentNamesRule = FragmentDefinitionRule $ \case
    Full.FragmentDefinition thisName _ _ _ thisLocation ->
        findDuplicates (filterByName thisName) thisLocation (error' thisName)
  where
    error' fragmentName = concat
        [ "There can be only one fragment named \""
        , Text.unpack fragmentName
        , "\"."
        ]
    filterByName thisName definition accumulator
        | Just fragmentDefinition <- viewFragment definition
        , Full.FragmentDefinition thatName _ _ _ thatLocation <- fragmentDefinition
        , thisName == thatName = thatLocation : accumulator
        | otherwise = accumulator

-- | Named fragment spreads must refer to fragments defined within the document.
-- It is a validation error if the target of a spread is not defined.
fragmentSpreadTargetDefinedRule :: forall m. Rule m
fragmentSpreadTargetDefinedRule = FragmentSpreadRule $ \case
    Full.FragmentSpread fragmentName _ location' -> do
        ast' <- asks ast
        case find (isSpreadTarget fragmentName) ast' of
            Nothing -> pure $ Error
                { message = error' fragmentName
                , locations = [location']
                }
            Just _ -> lift mempty
  where
    error' fragmentName = concat
        [ "Fragment target \""
        , Text.unpack fragmentName
        , "\" is undefined."
        ]

isSpreadTarget :: Text -> Full.Definition -> Bool
isSpreadTarget thisName (viewFragment -> Just fragmentDefinition)
    | Full.FragmentDefinition thatName _ _ _ _ <- fragmentDefinition
    , thisName == thatName = True
isSpreadTarget _ _ = False

-- | Fragments must be specified on types that exist in the schema. This applies
-- for both named and inline fragments. If they are not defined in the schema,
-- the query does not validate.
fragmentSpreadTypeExistenceRule :: forall m. Rule m
fragmentSpreadTypeExistenceRule = SelectionRule $ const $ \case
    Full.FragmentSpreadSelection fragmentSelection
        | Full.FragmentSpread fragmentName _ location' <- fragmentSelection -> do
            types' <- asks $ Schema.types . schema
            typeCondition <- findSpreadTarget fragmentName
            case HashMap.lookup typeCondition types' of
                Nothing -> pure $ Error
                    { message = spreadError fragmentName typeCondition
                    , locations = [location']
                    }
                Just _ -> lift mempty
    Full.InlineFragmentSelection fragmentSelection
        | Full.InlineFragment maybeType _ _ location' <- fragmentSelection
        , Just typeCondition <- maybeType -> do
            types' <- asks $ Schema.types . schema
            case HashMap.lookup typeCondition types' of
                Nothing -> pure $ Error
                    { message = inlineError typeCondition
                    , locations = [location']
                    }
                Just _ -> lift mempty
    _ -> lift mempty
  where
    spreadError fragmentName typeCondition = concat
        [ "Fragment \""
        , Text.unpack fragmentName
        , "\" is specified on type \""
        , Text.unpack typeCondition
        , "\" which doesn't exist in the schema."
        ]
    inlineError typeCondition = concat
        [ "Inline fragment is specified on type \""
        , Text.unpack typeCondition
        , "\" which doesn't exist in the schema."
        ]

maybeToSeq :: forall a. Maybe a -> Seq a
maybeToSeq (Just x) = pure x
maybeToSeq Nothing = mempty

-- | Fragments can only be declared on unions, interfaces, and objects. They are
-- invalid on scalars. They can only be applied on non‐leaf fields. This rule
-- applies to both inline and named fragments.
fragmentsOnCompositeTypesRule :: forall m. Rule m
fragmentsOnCompositeTypesRule = FragmentRule definitionRule inlineRule
  where
    inlineRule (Full.InlineFragment (Just typeCondition) _ _ location') =
        check typeCondition location'
    inlineRule _ = lift mempty
    definitionRule (Full.FragmentDefinition _ typeCondition _ _ location') =
        check typeCondition location'
    check typeCondition location' = do
        types' <- asks $ Schema.types . schema
        -- Skip unknown types, they are checked by another rule.
        _ <- lift $ maybeToSeq $ HashMap.lookup typeCondition types'
        case Type.lookupTypeCondition typeCondition types' of
            Nothing -> pure $ Error
                { message = errorMessage typeCondition
                , locations = [location']
                }
            Just _ -> lift mempty
    errorMessage typeCondition = concat
        [ "Fragment cannot condition on non composite type \""
        , Text.unpack typeCondition,
        "\"."
        ]

-- | Defined fragments must be used within a document.
noUnusedFragmentsRule :: forall m. Rule m
noUnusedFragmentsRule = FragmentDefinitionRule $ \fragment -> do
    let Full.FragmentDefinition fragmentName _ _ _ location' = fragment
     in mapReaderT (checkFragmentName fragmentName location')
        $ asks ast
        >>= flip evalStateT HashSet.empty
        . filterSelections evaluateSelection
        . foldMap definitionSelections
  where
    checkFragmentName fragmentName location' elements
        | fragmentName `elem` elements = mempty
        | otherwise = pure $ makeError fragmentName location'
    makeError fragName location' = Error
        { message = errorMessage fragName
        , locations = [location']
        }
    errorMessage fragName = concat
        [ "Fragment \""
        , Text.unpack fragName
        , "\" is never used."
        ]
    evaluateSelection selection
        | Full.FragmentSpreadSelection spreadSelection <- selection
        , Full.FragmentSpread spreadName _ _ <- spreadSelection =
            lift $ pure spreadName
    evaluateSelection _ = lift $ lift mempty

definitionSelections :: Full.Definition -> Full.SelectionSetOpt
definitionSelections (viewOperation -> Just operation)
    | Full.OperationDefinition _ _ _ _ selections _ <- operation =
        toList selections
    | Full.SelectionSet selections _ <- operation = toList selections
definitionSelections (viewFragment -> Just fragment)
    | Full.FragmentDefinition _ _ _ selections _ <- fragment = toList selections
definitionSelections _ = []

filterSelections :: Foldable t
    => forall a m
    . (Full.Selection -> ValidationState m a)
    -> t Full.Selection
    -> ValidationState m a
filterSelections applyFilter selections
    = (lift . lift) (Seq.fromList $ foldr evaluateSelection mempty selections)
    >>= applyFilter
  where
    evaluateSelection selection accumulator
        | Full.FragmentSpreadSelection{} <- selection = selection : accumulator
        | Full.FieldSelection fieldSelection <- selection
        , Full.Field _ _ _ _ subselections _ <- fieldSelection =
            selection : foldr evaluateSelection accumulator subselections
        | Full.InlineFragmentSelection inlineSelection <- selection
        , Full.InlineFragment _ _ subselections _ <- inlineSelection =
            selection : foldr evaluateSelection accumulator subselections

-- | The graph of fragment spreads must not form any cycles including spreading
-- itself. Otherwise an operation could infinitely spread or infinitely execute
-- on cycles in the underlying data.
noFragmentCyclesRule :: forall m. Rule m
noFragmentCyclesRule = FragmentDefinitionRule $ \case
    Full.FragmentDefinition fragmentName _ _ selections location' -> do
        state <- evalStateT (collectCycles selections) (0, fragmentName)
        let spreadPath = fst <$> sortBy (comparing snd) (HashMap.toList state)
        case reverse spreadPath of
            x : _ | x == fragmentName -> pure $ Error
                { message = concat
                    [ "Cannot spread fragment \""
                    , Text.unpack fragmentName
                    , "\" within itself (via "
                    , Text.unpack $ Text.intercalate " -> " $ fragmentName : spreadPath
                    , ")."
                    ]
                , locations = [location']
                }
            _ -> lift mempty
  where
    collectCycles :: Traversable t
        => t Full.Selection
        -> StateT (Int, Full.Name) (ReaderT (Validation m) Seq) (HashMap Full.Name Int)
    collectCycles = foldM forEach HashMap.empty
    forEach accumulator = \case
        Full.FieldSelection fieldSelection -> forField accumulator fieldSelection
        Full.InlineFragmentSelection fragmentSelection ->
            forInline accumulator fragmentSelection
        Full.FragmentSpreadSelection fragmentSelection ->
            forSpread accumulator fragmentSelection
    forSpread accumulator (Full.FragmentSpread fragmentName _ _) = do
        firstFragmentName <- gets snd
        modify $ first (+ 1)
        lastIndex <- gets fst
        let newAccumulator = HashMap.insert fragmentName lastIndex accumulator
        let inVisitetFragment = HashMap.member fragmentName accumulator
        if fragmentName == firstFragmentName || inVisitetFragment
            then pure newAccumulator
            else collectFromSpread fragmentName newAccumulator
    forInline accumulator (Full.InlineFragment _ _ selections _) =
        (accumulator <>) <$> collectCycles selections
    forField accumulator (Full.Field _ _ _ _ selections _) =
        (accumulator <>) <$> collectCycles selections
    collectFromSpread fragmentName accumulator = do
        ast' <- lift $ asks ast
        case findFragmentDefinition fragmentName ast' of
            Nothing -> pure accumulator
            Just (Full.FragmentDefinition _ _ _ selections _) ->
                (accumulator <>) <$> collectCycles selections

findFragmentDefinition :: Text
    -> NonEmpty Full.Definition
    -> Maybe Full.FragmentDefinition
findFragmentDefinition fragmentName = foldr compareDefinition Nothing
  where
    compareDefinition (Full.ExecutableDefinition executableDefinition) Nothing
        | Full.DefinitionFragment fragmentDefinition <- executableDefinition
        , Full.FragmentDefinition anotherName _ _ _ _ <- fragmentDefinition
        , anotherName == fragmentName = Just fragmentDefinition
    compareDefinition _ accumulator = accumulator

-- | Fields and directives treat arguments as a mapping of argument name to
-- value. More than one argument with the same name in an argument set is
-- ambiguous and invalid.
uniqueArgumentNamesRule :: forall m. Rule m
uniqueArgumentNamesRule = ArgumentsRule fieldRule directiveRule
  where
    fieldRule _ (Full.Field _ _ arguments _ _ _) =
        lift $ filterDuplicates extract "argument" arguments
    directiveRule (Full.Directive _ arguments _) =
        lift $ filterDuplicates extract "argument" arguments
    extract (Full.Argument argumentName _ location') = (argumentName, location')

-- | Directives are used to describe some metadata or behavioral change on the
-- definition they apply to. When more than one directive of the same name is
-- used, the expected metadata or behavior becomes ambiguous, therefore only one
-- of each directive is allowed per location.
uniqueDirectiveNamesRule :: forall m. Rule m
uniqueDirectiveNamesRule = DirectivesRule $ const $ \directives' -> do
    definitions' <- asks $ Schema.directives . schema
    let filterNonRepeatable = flip HashSet.member nonRepeatableSet
            . getField @"name"
        nonRepeatableSet =
            HashMap.foldlWithKey foldNonRepeatable HashSet.empty definitions'
    lift $ filterDuplicates extract "directive"
        $ filter filterNonRepeatable directives'
  where
    foldNonRepeatable  hashSet directiveName' (Schema.Directive _ _ False _) =
        HashSet.insert directiveName' hashSet
    foldNonRepeatable hashSet _ _ = hashSet
    extract (Full.Directive directiveName' _ location') =
        (directiveName', location')

filterDuplicates :: forall a
    . (a -> (Text, Full.Location))
    -> String
    -> [a]
    -> Seq Error
filterDuplicates extract nodeType = Seq.fromList
    . fmap makeError
    . filter ((> 1) . length)
    . NonEmpty.groupAllWith getName
  where
    getName = fst . extract
    makeError directives' = Error
        { message = makeMessage $ NonEmpty.head directives'
        , locations = snd . extract <$> toList directives'
        }
    makeMessage directive = concat
        [ "There can be only one "
        , nodeType
        , " named \""
        , Text.unpack $ fst $ extract directive
        , "\"."
        ]

-- | If any operation defines more than one variable with the same name, it is
-- ambiguous and invalid. It is invalid even if the type of the duplicate
-- variable is the same.
uniqueVariableNamesRule :: forall m. Rule m
uniqueVariableNamesRule = VariablesRule
    $ lift . filterDuplicates extract "variable"
  where
    extract (Full.VariableDefinition variableName _ _ location') =
        (variableName, location')

-- | Variables can only be input types. Objects, unions and interfaces cannot be
-- used as inputs.
variablesAreInputTypesRule :: forall m. Rule m
variablesAreInputTypesRule = VariablesRule
    $ (traverse check . Seq.fromList) >=> lift
  where
    check (Full.VariableDefinition name typeName _ location')
        = asks (Schema.types . schema)
        >>= lift
        . maybe (makeError name typeName location') (const mempty)
        . Type.lookupInputType typeName
    makeError name typeName location' = pure $ Error
        { message = concat
            [ "Variable \"$"
            , Text.unpack name
            , "\" cannot be non-input type \""
            , Text.unpack $ getTypeName typeName
            , "\"."
            ]
        , locations = [location']
        }
    getTypeName (Full.TypeNamed name) = name
    getTypeName (Full.TypeList name) = getTypeName name
    getTypeName (Full.TypeNonNull (Full.NonNullTypeNamed nonNull)) = nonNull
    getTypeName (Full.TypeNonNull (Full.NonNullTypeList nonNull)) =
        getTypeName nonNull

-- | Variables are scoped on a per‐operation basis. That means that any variable
-- used within the context of an operation must be defined at the top level of
-- that operation.
noUndefinedVariablesRule :: forall m. Rule m
noUndefinedVariablesRule =
    variableUsageDifference (flip HashMap.difference) errorMessage
  where
    errorMessage Nothing variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is not defined."
        ]
    errorMessage (Just operationName) variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is not defined by operation \""
        , Text.unpack operationName
        , "\"."
        ]

-- Used to find the difference between defined and used variables. The first
-- argument are variables defined in the operation, the second argument are
-- variables used in the query. It should return the difference between these
-- 2 sets.
type UsageDifference
    = HashMap Full.Name [Full.Location]
    -> HashMap Full.Name [Full.Location]
    -> HashMap Full.Name [Full.Location]

variableUsageDifference :: forall m. UsageDifference
    -> (Maybe Full.Name -> Full.Name -> String)
    -> Rule m
variableUsageDifference difference errorMessage = OperationDefinitionRule $ \case
    Full.SelectionSet _ _ -> lift mempty
    Full.OperationDefinition _ operationName variables _ selections _ ->
        let variableNames = HashMap.fromList $ getVariableName <$> variables
         in mapReaderT (readerMapper operationName variableNames)
            $ flip evalStateT HashSet.empty
            $ filterSelections'
            $ toList selections
  where
    readerMapper operationName variableNames' = Seq.fromList
        . fmap (makeError operationName)
        . HashMap.toList
        . difference variableNames'
        . HashMap.fromListWith (++)
        . toList
    getVariableName (Full.VariableDefinition variableName _ _ location') =
        (variableName, [location'])
    filterSelections' :: Foldable t
        => t Full.Selection
        -> ValidationState m (Full.Name, [Full.Location])
    filterSelections' = filterSelections variableFilter
    variableFilter :: Full.Selection -> ValidationState m (Full.Name, [Full.Location])
    variableFilter (Full.InlineFragmentSelection inline)
        | Full.InlineFragment _ directives' _ _ <- inline =
            lift $ lift $ mapDirectives directives'
    variableFilter (Full.FieldSelection fieldSelection)
        | Full.Field _ _ arguments directives' _ _ <- fieldSelection =
            lift $ lift $ mapArguments arguments <> mapDirectives directives'
    variableFilter (Full.FragmentSpreadSelection spread)
        | Full.FragmentSpread fragmentName _ _ <- spread = do
            nonVisitedFragmentDefinition <- visitFragmentDefinition fragmentName
            case nonVisitedFragmentDefinition of
                Just fragmentDefinition -> diveIntoSpread fragmentDefinition
                _ -> lift $ lift mempty
    diveIntoSpread (Full.FragmentDefinition _ _ directives' selections _)
        = filterSelections' selections
        >>= lift . mapReaderT (<> mapDirectives directives') . pure
    findDirectiveVariables (Full.Directive _ arguments _) = mapArguments arguments
    mapArguments = Seq.fromList . (>>= findArgumentVariables)
    mapDirectives = foldMap findDirectiveVariables

    findArgumentVariables (Full.Argument _ value _) = findNodeVariables value
    findNodeVariables Full.Node{ node = value, ..} = findValueVariables location value

    findValueVariables location (Full.Variable value') = [(value', [location])]
    findValueVariables _ (Full.List values) = values >>= findNodeVariables
    findValueVariables _ (Full.Object fields) = fields
        >>= findNodeVariables . getField @"value"
    findValueVariables _ _ = []
    makeError operationName (variableName, locations') = Error
        { message = errorMessage operationName variableName
        , locations = locations'
        }

-- | All variables defined by an operation must be used in that operation or a
-- fragment transitively included by that operation. Unused variables cause a
-- validation error.
noUnusedVariablesRule :: forall m. Rule m
noUnusedVariablesRule = variableUsageDifference HashMap.difference errorMessage
  where
    errorMessage Nothing variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is never used."
        ]
    errorMessage (Just operationName) variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is never used in operation \""
        , Text.unpack operationName
        , "\"."
        ]

-- | Input objects must not contain more than one field of the same name,
-- otherwise an ambiguity would exist which includes an ignored portion of
-- syntax.
uniqueInputFieldNamesRule :: forall m. Rule m
uniqueInputFieldNamesRule =
    ValueRule (const $ lift . go) (const $ lift . constGo)
  where
    go (Full.Node (Full.Object fields) _) = filterFieldDuplicates fields
    go _ = mempty
    filterFieldDuplicates = filterDuplicates getFieldName "input field"
    getFieldName (Full.ObjectField fieldName _ location') = (fieldName, location')
    constGo (Full.Node (Full.ConstObject fields) _) = filterFieldDuplicates fields
    constGo _ = mempty

-- | The target field of a field selection must be defined on the scoped type of
-- the selection set. There are no limitations on alias names.
fieldsOnCorrectTypeRule :: forall m. Rule m
fieldsOnCorrectTypeRule = FieldRule fieldRule
  where
    fieldRule parentType (Full.Field _ fieldName _ _ _ location')
        | Just objectType <- parentType
        , Nothing <- Type.lookupTypeField fieldName objectType
        , Just typeName <- typeNameIfComposite objectType = pure $ Error
            { message = errorMessage fieldName typeName
            , locations = [location']
            }
        | otherwise = lift mempty
    errorMessage fieldName typeName = concat
        [ "Cannot query field \""
        , Text.unpack fieldName
        , "\" on type \""
        , Text.unpack typeName
        , "\"."
        ]

compositeTypeName :: forall m. Type.CompositeType m -> Full.Name
compositeTypeName (Type.CompositeObjectType (Out.ObjectType typeName _ _ _)) =
    typeName
compositeTypeName (Type.CompositeInterfaceType interfaceType) =
    let Out.InterfaceType typeName _ _ _ = interfaceType
     in typeName
compositeTypeName (Type.CompositeUnionType (Out.UnionType typeName _ _)) =
    typeName

typeNameIfComposite :: forall m. Out.Type m -> Maybe Full.Name
typeNameIfComposite = fmap compositeTypeName . Type.outToComposite

-- | Field selections on scalars or enums are never allowed, because they are
-- the leaf nodes of any GraphQL query.
scalarLeafsRule :: forall m. Rule m
scalarLeafsRule = FieldRule fieldRule
  where
    fieldRule parentType selectionField@(Full.Field _ fieldName _ _ _ _)
        | Just objectType <- parentType
        , Just field <- Type.lookupTypeField fieldName objectType =
            let Out.Field _ fieldType _ = field
             in lift $ check fieldType selectionField
        | otherwise = lift mempty
    check (Out.ObjectBaseType (Out.ObjectType typeName _ _ _)) =
        checkNotEmpty typeName
    check (Out.InterfaceBaseType (Out.InterfaceType typeName _ _ _)) =
        checkNotEmpty typeName
    check (Out.UnionBaseType (Out.UnionType typeName _ _)) =
        checkNotEmpty typeName
    check (Out.ScalarBaseType (Definition.ScalarType typeName _)) =
        checkEmpty typeName
    check (Out.EnumBaseType (Definition.EnumType typeName _ _)) =
        checkEmpty typeName
    check (Out.ListBaseType wrappedType) = check wrappedType
    checkNotEmpty typeName (Full.Field _ fieldName _ _ [] location') =
        let fieldName' = Text.unpack fieldName
         in makeError location' $ concat
            [ "Field \""
            , fieldName'
            , "\" of type \""
            , Text.unpack typeName
            , "\" must have a selection of subfields. Did you mean \""
            , fieldName'
            , " { ... }\"?"
            ]
    checkNotEmpty _ _ = mempty
    checkEmpty _ (Full.Field _ _ _ _ [] _) = mempty
    checkEmpty typeName field' =
        let Full.Field _ fieldName _ _ _ location' = field'
         in makeError location' $ concat
            [ "Field \""
            , Text.unpack fieldName
            , "\" must not have a selection since type \""
            , Text.unpack typeName
            , "\" has no subfields."
            ]
    makeError location' errorMessage = pure $ Error
        { message = errorMessage
        , locations = [location']
        }

-- | Every argument provided to a field or directive must be defined in the set
-- of possible arguments of that field or directive.
knownArgumentNamesRule :: forall m. Rule m
knownArgumentNamesRule = ArgumentsRule fieldRule directiveRule
  where
    fieldRule (Just objectType) (Full.Field _ fieldName arguments  _ _ _)
        | Just typeField <- Type.lookupTypeField fieldName objectType
        , Just typeName <- typeNameIfComposite objectType =
            lift $ foldr (go typeName fieldName typeField) Seq.empty arguments
    fieldRule _ _ = lift mempty
    go typeName fieldName fieldDefinition (Full.Argument argumentName _ location') errors
        | Out.Field _ _ definitions <- fieldDefinition
        , Just _ <- HashMap.lookup argumentName definitions = errors
        | otherwise = errors |> Error
            { message = fieldMessage argumentName fieldName typeName
            , locations = [location']
            }
    fieldMessage argumentName fieldName typeName = concat
        [ "Unknown argument \""
        , Text.unpack argumentName
        , "\" on field \""
        , Text.unpack typeName
        , "."
        , Text.unpack fieldName
        , "\"."
        ]
    directiveRule (Full.Directive directiveName arguments _) = do
        available <- asks $ HashMap.lookup directiveName
            . Schema.directives . schema
        Full.Argument argumentName _ location' <- lift $ Seq.fromList arguments
        case available of
            Just (Schema.Directive _ definitions _ _)
                | not $ HashMap.member argumentName definitions ->
                    pure $ makeError argumentName directiveName location'
            _ -> lift mempty
    makeError argumentName directiveName location' = Error
        { message = directiveMessage argumentName directiveName
        , locations = [location']
        }
    directiveMessage argumentName directiveName = concat
        [ "Unknown argument \""
        , Text.unpack argumentName
        , "\" on directive \"@"
        , Text.unpack directiveName
        , "\"."
        ]

-- | GraphQL services define what directives they support. For each usage of a
-- directive, the directive must be available on that service.
knownDirectiveNamesRule :: Rule m
knownDirectiveNamesRule = DirectivesRule $ const $ \directives' -> do
    definitions' <- asks $ Schema.directives . schema
    let directiveSet = HashSet.fromList $ fmap (getField @"name") directives'
        definitionSet = HashSet.fromList $ HashMap.keys definitions'
        difference = HashSet.difference directiveSet definitionSet
        undefined' = filter (definitionFilter difference) directives'
    lift $ Seq.fromList $ makeError <$> undefined'
  where
    definitionFilter :: HashSet Full.Name -> Full.Directive -> Bool
    definitionFilter difference = flip HashSet.member difference
        . getField @"name"
    makeError Full.Directive{..} = Error
        { message = errorMessage name
        , locations = [location]
        }
    errorMessage directiveName' = concat
        [ "Unknown directive \"@"
        , Text.unpack directiveName'
        , "\"."
        ]

-- | Every input field provided in an input object value must be defined in the
-- set of possible fields of that input object’s expected type.
knownInputFieldNamesRule :: Rule m
knownInputFieldNamesRule = ValueRule go constGo
  where
    go (Just valueType) (Full.Node (Full.Object inputFields) _)
        | In.InputObjectBaseType objectType <- valueType =
             lift $ Seq.fromList $ mapMaybe (forEach objectType) inputFields
    go _ _ = lift mempty
    constGo (Just valueType) (Full.Node (Full.ConstObject inputFields) _)
        | In.InputObjectBaseType objectType <- valueType =
             lift $ Seq.fromList $ mapMaybe (forEach objectType) inputFields
    constGo  _ _ = lift mempty
    forEach objectType (Full.ObjectField inputFieldName _ location')
        | In.InputObjectType _ _ fieldTypes <- objectType
        , Just _ <- HashMap.lookup inputFieldName fieldTypes = Nothing
        | otherwise
        , In.InputObjectType typeName _ _ <- objectType = pure $ Error
            { message = errorMessage inputFieldName typeName
            , locations = [location']
            }
    errorMessage fieldName typeName = concat
        [ "Field \""
        , Text.unpack fieldName
        , "\" is not defined by type \""
        , Text.unpack typeName
        , "\"."
        ]

-- | GraphQL services define what directives they support and where they support
-- them. For each usage of a directive, the directive must be used in a location
-- that the service has declared support for.
directivesInValidLocationsRule :: Rule m
directivesInValidLocationsRule = DirectivesRule directivesRule
  where
    directivesRule directiveLocation directives' = do
        Full.Directive directiveName _ location <- lift $ Seq.fromList directives'
        maybeDefinition <- asks
            $ HashMap.lookup directiveName . Schema.directives . schema
        case maybeDefinition of
            Just (Schema.Directive _ _ _ allowedLocations)
                | directiveLocation `notElem` allowedLocations -> pure $ Error
                    { message = errorMessage directiveName directiveLocation
                    , locations = [location]
                    }
            _ -> lift mempty
    errorMessage directiveName directiveLocation = concat
        [ "Directive \"@"
        , Text.unpack directiveName
        , "\" may not be used on "
        , show directiveLocation
        , "."
        ]

-- | Arguments can be required. An argument is required if the argument type is
-- non‐null and does not have a default value. Otherwise, the argument is
-- optional.
providedRequiredArgumentsRule :: Rule m
providedRequiredArgumentsRule = ArgumentsRule fieldRule directiveRule
  where
    fieldRule (Just objectType) (Full.Field _ fieldName arguments  _ _ location')
        | Just typeField <- Type.lookupTypeField fieldName objectType
        , Out.Field _ _ definitions <- typeField =
            let forEach = go (fieldMessage fieldName) arguments location'
             in lift $ HashMap.foldrWithKey forEach Seq.empty definitions
    fieldRule _ _ = lift mempty
    directiveRule (Full.Directive directiveName arguments location') = do
        available <- asks
            $ HashMap.lookup directiveName . Schema.directives . schema
        case available of
            Just (Schema.Directive _ definitions _ _) ->
                let forEach = go (directiveMessage directiveName) arguments location'
                 in lift $ HashMap.foldrWithKey forEach Seq.empty definitions
            _ -> lift mempty
    go makeMessage arguments location' argumentName argumentType errors
        | In.Argument _ type' optionalValue <- argumentType
        , In.isNonNullType type'
        , typeName <- inputTypeName type'
        , isNothing optionalValue
        , isNothingOrNull $ find (lookupArgument argumentName) arguments
            = errors
            |> makeError (makeMessage argumentName typeName) location'
        | otherwise = errors
    makeError errorMessage location' = Error
        { message = errorMessage
        , locations = [location']
        }
    isNothingOrNull (Just (Full.Argument _ (Full.Node Full.Null _) _)) = True
    isNothingOrNull x = isNothing x
    lookupArgument needle (Full.Argument argumentName _ _) =
        needle == argumentName
    fieldMessage fieldName argumentName typeName = concat
        [ "Field \""
        , Text.unpack fieldName
        , "\" argument \""
        , Text.unpack argumentName
        , "\" of type \""
        , Text.unpack typeName
        , "\" is required, but it was not provided."
        ]
    directiveMessage directiveName argumentName typeName = concat
        [ "Directive \"@"
        , Text.unpack directiveName
        , "\" argument \""
        , Text.unpack argumentName
        , "\" of type \""
        , Text.unpack typeName
        , "\" is required, but it was not provided."
        ]

inputTypeName :: In.Type -> Text
inputTypeName (In.ScalarBaseType (Definition.ScalarType typeName _)) = typeName
inputTypeName (In.EnumBaseType (Definition.EnumType typeName _ _)) = typeName
inputTypeName (In.InputObjectBaseType (In.InputObjectType typeName _ _)) =
    typeName
inputTypeName (In.ListBaseType listType) = inputTypeName listType

-- | Input object fields may be required. Much like a field may have required
-- arguments, an input object may have required fields. An input field is
-- required if it has a non‐null type and does not have a default value.
-- Otherwise, the input object field is optional.
providedRequiredInputFieldsRule :: Rule m
providedRequiredInputFieldsRule = ValueRule go constGo
  where
    go (Just valueType) (Full.Node (Full.Object inputFields) location')
        | In.InputObjectBaseType objectType <- valueType
        , In.InputObjectType objectTypeName _ fieldDefinitions <- objectType
            = lift
            $ Seq.fromList
            $ HashMap.elems
            $ flip HashMap.mapMaybeWithKey fieldDefinitions
            $ forEach inputFields objectTypeName location'
    go _ _ = lift mempty
    constGo  _ _ = lift mempty
    forEach inputFields typeName location' definitionName fieldDefinition
        | In.InputField _ inputType optionalValue <- fieldDefinition
        , In.isNonNullType inputType
        , isNothing optionalValue
        , isNothingOrNull $ find (lookupField definitionName) inputFields =
            Just $ makeError definitionName typeName location'
        | otherwise = Nothing
    isNothingOrNull (Just (Full.ObjectField _ (Full.Node Full.Null _) _)) = True
    isNothingOrNull x = isNothing x
    lookupField needle (Full.ObjectField fieldName _ _) = needle == fieldName
    makeError fieldName typeName location' = Error
        { message = errorMessage fieldName typeName
        , locations = [location']
        }
    errorMessage fieldName typeName = concat
        [ "Input field \""
        , Text.unpack fieldName
        , "\" of type \""
        , Text.unpack typeName
        , "\" is required, but it was not provided."
        ]

-- | If multiple field selections with the same response names are encountered
-- during execution, the field and arguments to execute and the resulting value
-- should be unambiguous. Therefore any two field selections which might both be
-- encountered for the same object are only valid if they are equivalent.
--
-- For simple hand‐written GraphQL, this rule is obviously a clear developer
-- error, however nested fragments can make this difficult to detect manually.
overlappingFieldsCanBeMergedRule :: Rule m
overlappingFieldsCanBeMergedRule = OperationDefinitionRule $ \case
    Full.SelectionSet selectionSet _ -> do
        schema' <- asks schema
        go (toList selectionSet)
            $ Type.CompositeObjectType
            $ Schema.query schema'
    Full.OperationDefinition operationType _ _ _ selectionSet _ -> do
        schema' <- asks schema
        let root = go (toList selectionSet) . Type.CompositeObjectType
        case operationType of
            Full.Query -> root $ Schema.query schema'
            Full.Mutation
                | Just objectType <- Schema.mutation schema' -> root objectType
            Full.Subscription
                | Just objectType <- Schema.mutation schema' -> root objectType
            _ -> lift mempty
  where
    go selectionSet selectionType = do
        fieldTuples <- evalStateT (collectFields selectionType selectionSet) HashSet.empty
        fieldsInSetCanMerge fieldTuples
    fieldsInSetCanMerge fieldTuples = do
        validation <- ask
        let (lonely, paired) = flattenPairs fieldTuples
        let reader = flip runReaderT validation
        lift $ foldMap (reader . visitLonelyFields) lonely
            <> foldMap (reader . forEachFieldTuple) paired
    forEachFieldTuple (fieldA, fieldB) =
        case (parent fieldA, parent fieldB) of
            (parentA@Type.CompositeObjectType{}, parentB@Type.CompositeObjectType{})
                | parentA /= parentB -> sameResponseShape fieldA fieldB
            _ -> mapReaderT (checkEquality (node fieldA) (node fieldB))
                $ sameResponseShape fieldA fieldB
    checkEquality fieldA fieldB Seq.Empty
        | Full.Field _ fieldNameA _ _ _ _ <- fieldA
        , Full.Field _ fieldNameB _ _ _ _ <- fieldB
        , fieldNameA /= fieldNameB = pure $ makeError fieldA fieldB
        | Full.Field _ fieldNameA argumentsA _ _ locationA <- fieldA
        , Full.Field _ _ argumentsB _ _ locationB <- fieldB
        , argumentsA /= argumentsB =
            let message = concat
                    [ "Fields \""
                    , Text.unpack fieldNameA
                    , "\" conflict because they have different arguments. Use "
                    , "different aliases on the fields to fetch both if this "
                    , "was intentional."
                    ]
             in pure $ Error message [locationB, locationA]
    checkEquality _ _ previousErrors = previousErrors
    visitLonelyFields FieldInfo{..} =
        let Full.Field _ _ _ _ subSelections _ = node
            compositeFieldType = Type.outToComposite type'
         in maybe (lift Seq.empty) (go subSelections) compositeFieldType
    sameResponseShape fieldA fieldB =
        let Full.Field _ _ _ _ selectionsA _ = node fieldA
            Full.Field _ _ _ _ selectionsB _ = node fieldB
         in case unwrapTypes (type' fieldA) (type' fieldB) of
            Left True -> lift mempty
            Right (compositeA, compositeB) -> do
                validation <- ask
                let collectFields' composite = flip runReaderT validation
                        . flip evalStateT HashSet.empty
                        . collectFields composite
                let collectA = collectFields' compositeA selectionsA
                let collectB = collectFields' compositeB selectionsB
                fieldsInSetCanMerge
                    $ foldl' (HashMap.unionWith (<>)) HashMap.empty
                    $ collectA <> collectB
            _ -> pure $ makeError (node fieldA) (node fieldB)
    makeError fieldA fieldB =
        let Full.Field aliasA fieldNameA _ _ _ locationA = fieldA
            Full.Field _ fieldNameB _ _ _ locationB = fieldB
            message = concat
                [ "Fields \""
                , Text.unpack (fromMaybe fieldNameA aliasA)
                , "\" conflict because \""
                , Text.unpack fieldNameB
                , "\" and \""
                , Text.unpack fieldNameA
                , "\" are different fields. Use different aliases on the fields "
                , "to fetch both if this was intentional."
                ]
             in Error message [locationB, locationA]
    unwrapTypes typeA@Out.ScalarBaseType{} typeB@Out.ScalarBaseType{} =
        Left $ typeA == typeB
    unwrapTypes typeA@Out.EnumBaseType{} typeB@Out.EnumBaseType{} =
        Left $ typeA == typeB
    unwrapTypes (Out.ListType listA) (Out.ListType listB) =
        unwrapTypes listA listB
    unwrapTypes (Out.NonNullListType listA) (Out.NonNullListType listB) =
        unwrapTypes listA listB
    unwrapTypes typeA typeB
        | Out.isNonNullType typeA == Out.isNonNullType typeB
        , Just compositeA <- Type.outToComposite typeA
        , Just compositeB <- Type.outToComposite typeB =
            Right (compositeA, compositeB)
        | otherwise = Left False
    flattenPairs :: forall m
        . HashMap Full.Name (NonEmpty (Full.Field, Type.CompositeType m))
        -> (Seq (FieldInfo m), Seq (FieldInfo m, FieldInfo m))
    flattenPairs xs = HashMap.foldr splitSingleFields (Seq.empty, Seq.empty)
        $ foldr lookupTypeField [] <$> xs
    splitSingleFields :: forall m
        . [FieldInfo m]
        -> (Seq (FieldInfo m), Seq (FieldInfo m, FieldInfo m))
        -> (Seq (FieldInfo m), Seq (FieldInfo m, FieldInfo m))
    splitSingleFields [head'] (fields, pairList) = (fields |> head', pairList)
    splitSingleFields xs (fields, pairList) = (fields, pairs pairList xs)
    lookupTypeField (field, parentType) accumulator =
        let Full.Field _ fieldName _ _ _ _ = field
         in case Type.lookupCompositeField fieldName parentType of
            Nothing -> accumulator
            Just (Out.Field _ typeField _) ->
                FieldInfo field typeField parentType : accumulator
    pairs :: forall m
        . Seq (FieldInfo m, FieldInfo m)
        -> [FieldInfo m]
        -> Seq (FieldInfo m, FieldInfo m)
    pairs accumulator [] = accumulator
    pairs accumulator (fieldA : fields) =
        pair fieldA (pairs accumulator fields) fields
    pair _ accumulator [] = accumulator
    pair field accumulator (fieldA : fields) =
        pair field accumulator fields |> (field, fieldA)
    collectFields objectType = accumulateFields objectType mempty
    accumulateFields = foldM . forEach
    forEach parentType accumulator = \case
        Full.FieldSelection fieldSelection ->
            forField parentType accumulator fieldSelection
        Full.FragmentSpreadSelection fragmentSelection ->
            forSpread accumulator fragmentSelection
        Full.InlineFragmentSelection fragmentSelection ->
            forInline parentType accumulator fragmentSelection
    forField parentType accumulator field@(Full.Field alias fieldName _ _ _ _) =
        let key = fromMaybe fieldName alias
            value = (field, parentType) :| []
         in pure $ HashMap.insertWith (<>) key value accumulator
    forSpread accumulator (Full.FragmentSpread fragmentName _ _) = do
        inVisitetFragments <- gets $ HashSet.member fragmentName
        if inVisitetFragments
            then pure accumulator
            else collectFromSpread fragmentName accumulator
    forInline parentType accumulator = \case
        Full.InlineFragment maybeType _ selections _
            | Just typeCondition <- maybeType ->
                collectFromFragment typeCondition selections accumulator
            | otherwise -> accumulateFields parentType accumulator $ toList selections
    collectFromFragment typeCondition selectionSet' accumulator = do
        types' <- lift $ asks $ Schema.types . schema
        case Type.lookupTypeCondition typeCondition types' of
            Nothing -> pure accumulator
            Just compositeType ->
                accumulateFields compositeType accumulator $ toList selectionSet'
    collectFromSpread fragmentName accumulator = do
        modify $ HashSet.insert fragmentName
        ast' <- lift $ asks ast
        case findFragmentDefinition fragmentName ast' of
            Nothing -> pure accumulator
            Just (Full.FragmentDefinition _ typeCondition _ selectionSet' _) ->
                collectFromFragment typeCondition selectionSet' accumulator

data FieldInfo m = FieldInfo
    { node :: Full.Field
    , type' :: Out.Type m
    , parent :: Type.CompositeType m
    }

-- | Fragments are declared on a type and will only apply when the runtime
-- object type matches the type condition. They also are spread within the
-- context of a parent type. A fragment spread is only valid if its type
-- condition could ever apply within the parent type.
possibleFragmentSpreadsRule :: forall m. Rule m
possibleFragmentSpreadsRule = SelectionRule go
  where
    go (Just parentType) (Full.InlineFragmentSelection fragmentSelection)
        | Full.InlineFragment maybeType _ _ location' <- fragmentSelection
        , Just typeCondition <- maybeType = do
            (fragmentTypeName, parentTypeName) <-
                compareTypes typeCondition parentType
            pure $ Error
                { message = concat
                    [ "Fragment cannot be spread here as objects of type \""
                    , Text.unpack parentTypeName
                    , "\" can never be of type \""
                    , Text.unpack fragmentTypeName
                    , "\"."
                    ]
                , locations = [location']
                }
    go (Just parentType) (Full.FragmentSpreadSelection fragmentSelection)
        | Full.FragmentSpread fragmentName _ location' <- fragmentSelection = do
            typeCondition <- findSpreadTarget fragmentName
            (fragmentTypeName, parentTypeName) <-
                compareTypes typeCondition parentType
            pure $ Error
                { message = concat
                    [ "Fragment \""
                    , Text.unpack fragmentName
                    , "\" cannot be spread here as objects of type \""
                    , Text.unpack parentTypeName
                    , "\" can never be of type \""
                    , Text.unpack fragmentTypeName
                    , "\"."
                    ]
                , locations = [location']
                }
    go _ _ = lift mempty
    compareTypes typeCondition parentType = do
        types' <- asks $ Schema.types . schema
        fragmentType <- lift
            $ maybeToSeq
            $ Type.lookupTypeCondition typeCondition types'
        parentComposite <- lift
            $ maybeToSeq
            $ Type.outToComposite parentType
        possibleFragments <- getPossibleTypes fragmentType
        possibleParents <- getPossibleTypes parentComposite
        let fragmentTypeName = compositeTypeName fragmentType
        let parentTypeName = compositeTypeName parentComposite
        if HashSet.null $ HashSet.intersection possibleFragments possibleParents
            then pure (fragmentTypeName, parentTypeName)
            else lift mempty
    getPossibleTypeList (Type.CompositeObjectType objectType) =
        pure [Schema.ObjectType objectType]
    getPossibleTypeList (Type.CompositeUnionType unionType) =
        let Out.UnionType _ _ members = unionType
         in pure $ Schema.ObjectType <$> members
    getPossibleTypeList (Type.CompositeInterfaceType interfaceType) =
        let Out.InterfaceType typeName _ _ _ = interfaceType
         in HashMap.lookupDefault [] typeName
        <$> asks (Schema.implementations . schema)
    getPossibleTypes compositeType
        = foldr (HashSet.insert . internalTypeName) HashSet.empty
        <$> getPossibleTypeList compositeType

internalTypeName :: forall m. Schema.Type m -> Full.Name
internalTypeName (Schema.ScalarType (Definition.ScalarType typeName _)) =
    typeName
internalTypeName (Schema.EnumType (Definition.EnumType typeName _ _)) = typeName
internalTypeName (Schema.ObjectType (Out.ObjectType typeName _ _ _)) = typeName
internalTypeName (Schema.InputObjectType (In.InputObjectType typeName _ _)) =
    typeName
internalTypeName (Schema.InterfaceType (Out.InterfaceType typeName _ _ _)) =
    typeName
internalTypeName (Schema.UnionType (Out.UnionType typeName _ _)) = typeName

findSpreadTarget :: Full.Name -> ReaderT (Validation m1) Seq Full.TypeCondition
findSpreadTarget fragmentName = do
    ast' <- asks ast
    let target = find (isSpreadTarget fragmentName) ast'
    lift $ maybeToSeq $ target >>= extractTypeCondition
  where
    extractTypeCondition (viewFragment -> Just fragmentDefinition) =
        let Full.FragmentDefinition _ typeCondition _ _ _ = fragmentDefinition
            in Just typeCondition
    extractTypeCondition _ = Nothing

visitFragmentDefinition :: forall m
    . Text
    -> ValidationState m (Maybe Full.FragmentDefinition)
visitFragmentDefinition fragmentName = do
    definitions <- lift $ asks ast
    visited <- gets (HashSet.member fragmentName)
    modify (HashSet.insert fragmentName)
    case find (isSpreadTarget fragmentName) definitions of
        Just (viewFragment -> Just fragmentDefinition)
            | not visited -> pure $ Just fragmentDefinition
        _ -> pure Nothing

-- | Variable usages must be compatible with the arguments they are passed to.
--
-- Validation failures occur when variables are used in the context of types
-- that are complete mismatches, or if a nullable type in a variable is passed
-- to a non‐null argument type.
variablesInAllowedPositionRule :: forall m. Rule m
variablesInAllowedPositionRule = OperationDefinitionRule $ \case
    Full.OperationDefinition operationType _ variables _ selectionSet _ -> do
        schema' <- asks schema
        let root = go variables (toList selectionSet) . Type.CompositeObjectType
        case operationType of
            Full.Query -> root $ Schema.query schema'
            Full.Mutation
                | Just objectType <- Schema.mutation schema' -> root objectType
            Full.Subscription
                | Just objectType <- Schema.mutation schema' -> root objectType
            _ -> lift mempty
    _ -> lift mempty
  where
    go variables selections selectionType = mapReaderT (foldr (<>) Seq.empty)
        $ flip evalStateT HashSet.empty
        $ visitSelectionSet variables selectionType
        $ toList selections
    visitSelectionSet :: Foldable t
        => [Full.VariableDefinition]
        -> Type.CompositeType m
        -> t Full.Selection
        -> ValidationState m (Seq Error)
    visitSelectionSet variables selectionType =
        foldM (evaluateSelection variables selectionType) mempty
    evaluateFieldSelection variables selections accumulator = \case
        Just newParentType -> do
            let folder = evaluateSelection variables newParentType
            selectionErrors <- foldM folder accumulator selections
            pure $ accumulator <> selectionErrors
        Nothing -> pure accumulator
    evaluateSelection :: [Full.VariableDefinition]
        -> Type.CompositeType m
        -> Seq Error
        -> Full.Selection
        -> ValidationState m (Seq Error)
    evaluateSelection variables selectionType accumulator selection
        | Full.FragmentSpreadSelection spread <- selection
        , Full.FragmentSpread fragmentName _ _ <- spread = do
            types' <- lift $ asks $ Schema.types . schema
            nonVisitedFragmentDefinition <- visitFragmentDefinition fragmentName
            case nonVisitedFragmentDefinition of
                Just fragmentDefinition
                    | Full.FragmentDefinition _ typeCondition _ _ _ <- fragmentDefinition
                    , Just spreadType <- Type.lookupTypeCondition typeCondition types' -> do
                        spreadErrors <- spreadVariables variables spread
                        selectionErrors <- diveIntoSpread variables spreadType fragmentDefinition
                        pure $ accumulator <> spreadErrors <> selectionErrors
                _ -> lift $ lift mempty
        | Full.FieldSelection fieldSelection <- selection
        , Full.Field _ fieldName _ _ subselections _ <- fieldSelection =
            case Type.lookupCompositeField fieldName selectionType of
                Just (Out.Field _ typeField argumentTypes) -> do
                    fieldErrors <- fieldVariables variables argumentTypes fieldSelection
                    selectionErrors <- evaluateFieldSelection variables subselections accumulator
                            $ Type.outToComposite typeField
                    pure $ selectionErrors <> fieldErrors
                Nothing -> pure accumulator
        | Full.InlineFragmentSelection inlineSelection <- selection
        , Full.InlineFragment typeCondition _ subselections _ <- inlineSelection = do
            types' <- lift $ asks $ Schema.types . schema
            let inlineType = fromMaybe selectionType
                    $ typeCondition >>= flip Type.lookupTypeCondition types'
            fragmentErrors <- inlineVariables variables inlineSelection
            let folder = evaluateSelection variables inlineType
            selectionErrors <- foldM folder accumulator subselections
            pure $ accumulator <> fragmentErrors <> selectionErrors
    inlineVariables variables inline
        | Full.InlineFragment _ directives' _ _ <- inline =
            mapDirectives variables directives'
    fieldVariables :: [Full.VariableDefinition]
        -> In.Arguments
        -> Full.Field
        -> ValidationState m (Seq Error)
    fieldVariables variables argumentTypes fieldSelection = do
        let Full.Field _ _ arguments directives' _ _ = fieldSelection
        argumentErrors <- mapArguments variables argumentTypes arguments
        directiveErrors <- mapDirectives variables directives'
        pure $ argumentErrors <> directiveErrors
    spreadVariables variables (Full.FragmentSpread _ directives' _) =
        mapDirectives variables directives'
    diveIntoSpread variables fieldType fragmentDefinition = do
        let Full.FragmentDefinition _ _ directives' selections _ =
                fragmentDefinition
        selectionErrors <- visitSelectionSet variables fieldType selections
        directiveErrors <- mapDirectives variables directives'
        pure $ selectionErrors <> directiveErrors
    findDirectiveVariables variables directive = do
        let Full.Directive directiveName arguments _ = directive
        directiveDefinitions <- lift $ asks $ Schema.directives . schema
        case HashMap.lookup directiveName directiveDefinitions of
            Just (Schema.Directive _ directiveArguments _ _) ->
                mapArguments variables directiveArguments arguments
            Nothing -> pure mempty
    mapArguments variables argumentTypes = fmap fold
        . traverse (findArgumentVariables variables argumentTypes)
    mapDirectives variables = fmap fold
        <$> traverse (findDirectiveVariables variables)
    lookupInputObject variables objectFieldValue locationInfo
        | Full.Node{ node = Full.Object objectFields } <- objectFieldValue
        , Just (expectedType, _) <- locationInfo
        , In.InputObjectBaseType inputObjectType <- expectedType
        , In.InputObjectType _ _ fieldTypes' <- inputObjectType =
            fold <$> traverse (traverseObjectField variables fieldTypes') objectFields
        | otherwise = pure mempty
    maybeUsageAllowed variableName variables locationInfo
        | Just (locationType, locationValue) <- locationInfo
        , findVariableDefinition' <- findVariableDefinition variableName
        , Just variableDefinition <- find findVariableDefinition' variables
            = maybeToSeq
            <$> isVariableUsageAllowed locationType locationValue variableDefinition
        | otherwise = pure mempty
    findArgumentVariables :: [Full.VariableDefinition]
        -> HashMap Full.Name In.Argument
        -> Full.Argument
        -> ValidationState m (Seq Error)
    findArgumentVariables variables argumentTypes argument
        | Full.Argument argumentName argumentValue _ <- argument
        , Full.Node{ node = Full.Variable variableName } <- argumentValue
            = maybeUsageAllowed variableName variables
            $ locationPair extractArgument argumentTypes argumentName
        | Full.Argument argumentName argumentValue _ <- argument
            = lookupInputObject variables argumentValue
            $ locationPair extractArgument argumentTypes argumentName
    extractField (In.InputField _ locationType locationValue) =
        (locationType, locationValue)
    extractArgument (In.Argument _ locationType locationValue) =
        (locationType, locationValue)
    locationPair extract fieldTypes name =
        extract <$> HashMap.lookup name fieldTypes
    traverseObjectField variables fieldTypes Full.ObjectField{..}
        | Full.Node{ node = Full.Variable variableName } <- value
            = maybeUsageAllowed variableName variables
            $ locationPair extractField fieldTypes name
        | otherwise = lookupInputObject variables value
            $  locationPair extractField fieldTypes name
    findVariableDefinition variableName variableDefinition =
        let Full.VariableDefinition variableName' _ _ _ = variableDefinition
         in variableName == variableName'
    isVariableUsageAllowed locationType locationDefaultValue variableDefinition
        | Full.VariableDefinition _ variableType _ _ <- variableDefinition
        , Full.TypeNonNull _ <- variableType =
            typesCompatibleOrError variableDefinition locationType
        | Just nullableLocationType <- unwrapInType locationType
        , Full.VariableDefinition _ variableType variableDefaultValue _ <-
            variableDefinition
        , hasNonNullVariableDefaultValue' <-
            hasNonNullVariableDefaultValue variableDefaultValue
        , hasLocationDefaultValue <- isJust locationDefaultValue =
            if (hasNonNullVariableDefaultValue' || hasLocationDefaultValue)
                && areTypesCompatible variableType nullableLocationType
                then pure Nothing
                else pure $ makeError variableDefinition locationType
        | otherwise = typesCompatibleOrError variableDefinition locationType
    typesCompatibleOrError variableDefinition locationType
        | Full.VariableDefinition _ variableType _ _ <- variableDefinition
        , areTypesCompatible variableType locationType = pure Nothing
        | otherwise = pure $ makeError variableDefinition locationType
    areTypesCompatible nonNullType (unwrapInType -> Just nullableLocationType)
        | Full.TypeNonNull (Full.NonNullTypeNamed namedType) <- nonNullType =
            areTypesCompatible (Full.TypeNamed namedType) nullableLocationType
        | Full.TypeNonNull (Full.NonNullTypeList namedList) <- nonNullType =
            areTypesCompatible (Full.TypeList namedList) nullableLocationType
    areTypesCompatible _ (In.isNonNullType -> True) = False
    areTypesCompatible (Full.TypeNonNull nonNullType) locationType
        | Full.NonNullTypeNamed namedType <- nonNullType =
            areTypesCompatible (Full.TypeNamed namedType) locationType
        | Full.NonNullTypeList namedType <- nonNullType =
            areTypesCompatible (Full.TypeList namedType) locationType
    areTypesCompatible variableType locationType
        | Full.TypeList itemVariableType <- variableType
        , In.ListType itemLocationType <- locationType =
            areTypesCompatible itemVariableType itemLocationType
        | areIdentical variableType locationType = True
        | otherwise = False
    areIdentical (Full.TypeList typeList) (In.ListType itemLocationType) =
        areIdentical typeList itemLocationType
    areIdentical (Full.TypeNonNull nonNullType) locationType
        | Full.NonNullTypeList nonNullList <- nonNullType
        , In.NonNullListType  itemLocationType <- locationType =
            areIdentical nonNullList itemLocationType
        | Full.NonNullTypeNamed _ <- nonNullType
        , In.ListBaseType _ <- locationType = False
        | Full.NonNullTypeNamed nonNullList <- nonNullType
        , In.isNonNullType locationType =
            nonNullList == inputTypeName locationType
    areIdentical (Full.TypeNamed _) (In.ListBaseType _) = False
    areIdentical (Full.TypeNamed typeNamed) locationType
        | not $ In.isNonNullType locationType =
            typeNamed == inputTypeName locationType
    areIdentical _ _ = False
    hasNonNullVariableDefaultValue (Just (Full.Node Full.ConstNull _)) = False
    hasNonNullVariableDefaultValue Nothing = False
    hasNonNullVariableDefaultValue _ = True
    makeError variableDefinition expectedType =
        let Full.VariableDefinition variableName variableType _ location' =
                variableDefinition
         in Just $ Error
            { message = concat
                [ "Variable \"$"
                , Text.unpack variableName
                , "\" of type \""
                , show variableType
                , "\" used in position expecting type \""
                , show expectedType
                , "\"."
                ]
            , locations = [location']
            }

unwrapInType :: In.Type -> Maybe In.Type
unwrapInType (In.NonNullScalarType nonNullType) =
    Just $ In.NamedScalarType nonNullType
unwrapInType (In.NonNullEnumType nonNullType) =
    Just $ In.NamedEnumType nonNullType
unwrapInType (In.NonNullInputObjectType nonNullType) =
    Just $ In.NamedInputObjectType nonNullType
unwrapInType (In.NonNullListType nonNullType) =
    Just $ In.ListType nonNullType
unwrapInType _ = Nothing

-- | Literal values must be compatible with the type expected in the position
-- they are found as per the coercion rules.
--
-- The type expected in a position include the type defined by the argument a
-- value is provided for, the type defined by an input object field a value is
-- provided for, and the type of a variable definition a default value is
-- provided for.
valuesOfCorrectTypeRule :: forall m. Rule m
valuesOfCorrectTypeRule = ValueRule go constGo
  where
    go (Just inputType) value
        | Just constValue <- toConstNode value =
            lift $ check inputType constValue
    go _ _ = lift mempty -- This rule checks only literals.
    toConstNode Full.Node{..} = flip Full.Node location <$> toConst node
    toConst (Full.Variable _) = Nothing
    toConst (Full.Int integer) = Just $ Full.ConstInt integer
    toConst (Full.Float double) = Just $ Full.ConstFloat double
    toConst (Full.String string) = Just $ Full.ConstString string
    toConst (Full.Boolean boolean) = Just $ Full.ConstBoolean boolean
    toConst Full.Null = Just Full.ConstNull
    toConst (Full.Enum enum) = Just $ Full.ConstEnum enum
    toConst (Full.List values) =
        Just $ Full.ConstList $ mapMaybe toConstNode values
    toConst (Full.Object fields) = Just $ Full.ConstObject
        $ mapMaybe constObjectField fields
    constObjectField Full.ObjectField{..}
        | Just constValue <- toConstNode value =
            Just $ Full.ObjectField name constValue location
        | otherwise = Nothing
    constGo Nothing = const $ lift mempty
    constGo (Just inputType) = lift . check inputType
    check :: In.Type -> Full.Node Full.ConstValue -> Seq Error
    check _ Full.Node{ node = Full.ConstNull } =
        mempty -- Ignore, required fields are checked elsewhere.
    check (In.ScalarBaseType scalarType) Full.Node{ node }
        | Definition.ScalarType "Int" _ <- scalarType
        , Full.ConstInt _ <- node = mempty
        | Definition.ScalarType "Boolean" _ <- scalarType
        , Full.ConstBoolean _ <- node = mempty
        | Definition.ScalarType "String" _ <- scalarType
        , Full.ConstString _ <- node = mempty
        | Definition.ScalarType "ID" _ <- scalarType
        , Full.ConstString _ <- node = mempty
        | Definition.ScalarType "ID" _ <- scalarType
        , Full.ConstInt _ <- node = mempty
        | Definition.ScalarType "Float" _ <- scalarType
        , Full.ConstFloat _ <- node = mempty
        | Definition.ScalarType "Float" _ <- scalarType
        , Full.ConstInt _ <- node = mempty
    check (In.EnumBaseType enumType) Full.Node{ node }
        | Definition.EnumType _ _ members <- enumType
        , Full.ConstEnum memberValue <- node
        , HashMap.member memberValue members = mempty
    check (In.InputObjectBaseType objectType) Full.Node{ node }
        -- Skip, objects are checked recursively by the validation traverser.
        | In.InputObjectType{}  <- objectType
        , Full.ConstObject{} <- node = mempty
    check (In.ListBaseType listType) constValue@Full.Node{ .. }
        | Full.ConstList values <- node =
            foldMap (checkNull listType) values
        | otherwise = check listType constValue
    check inputType Full.Node{ .. } = pure $ Error
        { message = concat
            [ "Value "
            , show node
            , " cannot be coerced to type \""
            , show inputType
            , "\"."
            ]
        , locations = [location]
        }
    checkNull inputType constValue =
        let checkResult = check inputType constValue
         in case null checkResult of
            True
                | Just unwrappedType <- unwrapInType inputType
                , Full.Node{ node = Full.ConstNull, .. } <- constValue ->
                    pure $ Error
                        { message = concat
                            [ "List of non-null values of type \""
                            , show unwrappedType
                            , "\" cannot contain null values."
                            ]
                        , locations = [location]
                        }
                | otherwise -> mempty
            _ -> checkResult
