{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , module Language.GraphQL.Execute.Coerce
    ) where

import Conduit (mapMC, (.|))
import Control.Arrow (left)
import Control.Monad.Catch
     ( Exception(..)
     , Handler(..)
     , MonadCatch(..)
     , MonadThrow(..)
     , SomeException(..)
     , catches
     )
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT)
import qualified Control.Monad.Trans.Writer as Writer
import Control.Monad (foldM)
import qualified Language.GraphQL.AST.Document as Full
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (cast)
import GHC.Records (HasField(..))
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import qualified Language.GraphQL.Execute.Transform as Transform
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Internal as Type.Internal
import Language.GraphQL.Type.Schema (Schema, Type)
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Error
    ( Error(..)
    , Response(..)
    , Path(..)
    , ResolverException(..)
    , ResponseEventStream
    )
import Prelude hiding (null)
import Language.GraphQL.AST.Document (showVariableName)

newtype ExecutorT m a = ExecutorT
    { runExecutorT :: ReaderT (HashMap Full.Name (Type m)) (WriterT (Seq Error) m) a
    }

instance Functor m => Functor (ExecutorT m) where
    fmap f = ExecutorT . fmap f . runExecutorT

instance Applicative m => Applicative (ExecutorT m) where
    pure = ExecutorT . pure
    ExecutorT f <*> ExecutorT x = ExecutorT $ f <*> x

instance Monad m => Monad (ExecutorT m) where
    ExecutorT x >>= f = ExecutorT $ x >>= runExecutorT . f

instance MonadTrans ExecutorT where
    lift = ExecutorT . lift . lift

instance MonadThrow m => MonadThrow (ExecutorT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (ExecutorT m) where
  catch (ExecutorT stack) handler =
      ExecutorT $ catch stack $ runExecutorT . handler

data GraphQLException = forall e. Exception e => GraphQLException e

instance Show GraphQLException where
    show (GraphQLException e) = show e

instance Exception GraphQLException

graphQLExceptionToException :: Exception e => e -> SomeException
graphQLExceptionToException = toException . GraphQLException

graphQLExceptionFromException :: Exception e => SomeException -> Maybe e
graphQLExceptionFromException e = do
    GraphQLException graphqlException <- fromException e
    cast graphqlException

data ResultException = forall e. Exception e => ResultException e

instance Show ResultException where
    show (ResultException e) = show e

instance Exception ResultException where
    toException = graphQLExceptionToException
    fromException = graphQLExceptionFromException

resultExceptionToException :: Exception e => e -> SomeException
resultExceptionToException = toException . ResultException

resultExceptionFromException :: Exception e => SomeException -> Maybe e
resultExceptionFromException e = do
    ResultException resultException <- fromException e
    cast resultException

data FieldException = forall e. Exception e => FieldException Full.Location [Path] e

instance Show FieldException where
    show (FieldException _ _ e) = displayException e

instance Exception FieldException where
    toException = graphQLExceptionToException
    fromException = graphQLExceptionFromException

data ValueCompletionException = ValueCompletionException String Type.Value

instance Show ValueCompletionException where
    show (ValueCompletionException typeRepresentation found) = concat
        [ "Value completion error. Expected type "
        , typeRepresentation
        , ", found: "
        , show found
        , "."
        ]

instance Exception ValueCompletionException where
    toException = resultExceptionToException
    fromException = resultExceptionFromException

data InputCoercionException =
    InputCoercionException String In.Type (Maybe (Full.Node Transform.Input))

instance Show InputCoercionException where
    show (InputCoercionException argumentName argumentType Nothing) = concat
        [ "Required argument \""
        , argumentName
        , "\" of type "
        , show argumentType
        , " not specified."
        ]
    show (InputCoercionException argumentName argumentType (Just givenValue)) = concat
        [ "Argument \""
        , argumentName
        , "\" has invalid type. Expected type "
        , show argumentType
        , ", found: "
        , show givenValue
        , "."
        ]

instance Exception InputCoercionException where
    toException = graphQLExceptionToException
    fromException = graphQLExceptionFromException

newtype ResultCoercionException = ResultCoercionException String

instance Show ResultCoercionException where
    show (ResultCoercionException typeRepresentation) = concat
        [ "Unable to coerce result to "
        , typeRepresentation
        , "."
        ]

instance Exception ResultCoercionException where
    toException = resultExceptionToException
    fromException = resultExceptionFromException

-- | Query error types.
data QueryError
    = OperationNameRequired
    | OperationNotFound String
    | CoercionError Full.VariableDefinition
    | UnknownInputType Full.VariableDefinition

type ExecuteHandler m a e = e -> ExecutorT m a

tell :: Monad m => Seq Error -> ExecutorT m ()
tell = ExecutorT . lift . Writer.tell

operationNameErrorText :: Text
operationNameErrorText = Text.unlines
    [ "Named operations must be provided with the name of the desired operation."
    , "See https://spec.graphql.org/June2018/#sec-Language.Document description."
    ]

queryError :: QueryError -> Error
queryError OperationNameRequired =
    let queryErrorMessage = "Operation name is required. " <> operationNameErrorText
    in Error{ message = queryErrorMessage, locations = [], path = [] }
queryError (OperationNotFound operationName) =
    let queryErrorMessage = Text.unlines
            [ Text.concat
              [ "Operation \""
              , Text.pack operationName
              , "\" is not found in the named operations you've provided. "
              ]
            , operationNameErrorText
            ]
     in Error{ message = queryErrorMessage, locations = [], path = [] }
queryError (CoercionError variableDefinition) =
    let (Full.VariableDefinition _ _ _ location) = variableDefinition
        queryErrorMessage = Text.concat
            [ "Failed to coerce the variable "
            , Text.pack $ Full.showVariable variableDefinition
            , "."
            ]
     in Error{ message = queryErrorMessage, locations = [location], path = [] }
queryError (UnknownInputType variableDefinition) =
    let Full.VariableDefinition _ variableTypeName _ location = variableDefinition
        queryErrorMessage = Text.concat
            [ "Variable "
            , Text.pack $ showVariableName variableDefinition
            , " has unknown type "
            , Text.pack $ show variableTypeName
            , "."
            ]
     in Error{ message = queryErrorMessage, locations = [location], path = [] }

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields. The operation name can be used if the document
-- defines multiple root operations.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
execute :: (MonadCatch m, VariableValue a, Serialize b)
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> HashMap Full.Name a -- ^ Variable substitution function.
    -> Full.Document -- @GraphQL@ document.
    -> m (Either (ResponseEventStream m b) (Response b))
execute schema' operationName subs document' =
    executeRequest schema' document' (Text.unpack <$> operationName) subs

executeRequest :: (MonadCatch m, Serialize a, VariableValue b)
    => Schema m
    -> Full.Document
    -> Maybe String
    -> HashMap Full.Name b
    -> m (Either (ResponseEventStream m a) (Response a))
executeRequest schema sourceDocument operationName variableValues = do
    operationAndVariables <- sequence buildOperation
    case operationAndVariables of
        Left queryError' -> pure
            $ Right
            $ Response null $ pure $ queryError queryError'
        Right operation
            | Transform.Operation Full.Query topSelections _operationLocation <- operation ->
                 Right <$> executeQuery topSelections schema
            | Transform.Operation Full.Mutation topSelections operationLocation <- operation ->
                Right <$> executeMutation topSelections schema operationLocation
            | Transform.Operation Full.Subscription topSelections operationLocation <- operation ->
                either rightErrorResponse Left <$> subscribe topSelections schema operationLocation
  where
    schemaTypes = Schema.types schema
    (operationDefinitions, fragmentDefinitions') =
        Transform.document sourceDocument
    buildOperation = do
        operationDefinition <- getOperation operationDefinitions operationName
        coercedVariableValues <- coerceVariableValues
            schemaTypes
            operationDefinition
            variableValues
        let replacement = Transform.Replacement
                { variableValues = coercedVariableValues
                , fragmentDefinitions = fragmentDefinitions'
                , visitedFragments = mempty
                , types = schemaTypes
                }
        pure $ flip runReaderT replacement
            $ Transform.runTransformT
            $ Transform.transform operationDefinition

rightErrorResponse :: Serialize b => forall a. Error -> Either a (Response b)
rightErrorResponse = Right . Response null . pure

getOperation :: [Full.OperationDefinition] -> Maybe String -> Either QueryError Full.OperationDefinition
getOperation [operation] Nothing = Right operation
getOperation operations (Just givenOperationName)
    = maybe (Left $ OperationNotFound givenOperationName) Right
    $ find findOperationByName operations
  where
    findOperationByName (Full.OperationDefinition _ (Just operationName) _ _ _ _) =
        givenOperationName == Text.unpack operationName
    findOperationByName _ = False
getOperation _ _ = Left OperationNameRequired

executeQuery :: (MonadCatch m, Serialize a)
    => Seq (Transform.Selection m)
    -> Schema m
    -> m (Response a)
executeQuery topSelections schema = do
    let queryType = Schema.query schema
    (data', errors) <- runWriterT
        $ flip runReaderT (Schema.types schema)
        $ runExecutorT
        $ catch (executeSelectionSet topSelections queryType Type.Null [])
        handleException
    pure $ Response data' errors

handleException :: (MonadCatch m, Serialize a)
    => ExecuteHandler m a FieldException
handleException (FieldException fieldLocation errorPath next) =
    let newError = constructError next fieldLocation errorPath
     in tell (Seq.singleton newError) >> pure null

constructError :: Exception e => e -> Full.Location -> [Path] -> Error
constructError e fieldLocation errorPath = Error
    { message = Text.pack (displayException e)
    , path = reverse errorPath
    , locations = [fieldLocation]
    }

executeMutation :: (MonadCatch m, Serialize a)
    => Seq (Transform.Selection m)
    -> Schema m
    -> Full.Location
    -> m (Response a)
executeMutation topSelections schema operationLocation
    | Just mutationType <- Schema.mutation schema = do
        (data', errors) <- runWriterT
            $ flip runReaderT (Schema.types schema)
            $ runExecutorT
            $ catch (executeSelectionSet topSelections mutationType Type.Null [])
            handleException
        pure $ Response data' errors
    | otherwise = pure
        $ Response null
        $ Seq.singleton
        $ Error "Schema doesn't support mutations." [operationLocation] []

executeSelectionSet :: (MonadCatch m, Serialize a)
    => Seq (Transform.Selection m)
    -> Out.ObjectType m
    -> Type.Value
    -> [Path]
    -> ExecutorT m a
executeSelectionSet selections objectType objectValue errorPath = do
    let groupedFieldSet = collectFields objectType selections
    resolvedValues <- OrderedMap.traverseMaybe go groupedFieldSet
    coerceResult (Out.NonNullObjectType objectType) $ Object resolvedValues
  where
    executeField' fields resolver =
        executeField objectValue fields resolver errorPath
    Out.ObjectType _ _ _ resolvers = objectType
    go fields@(Transform.Field _ fieldName _ _ _ :| _) =
        traverse (executeField' fields) $ HashMap.lookup fieldName resolvers

fieldsSegment :: forall m. NonEmpty (Transform.Field m) -> Path
fieldsSegment (Transform.Field alias fieldName _ _ _ :| _) =
    Segment (fromMaybe fieldName alias)

viewResolver :: Out.Resolver m -> (Out.Field m, Out.Resolve m)
viewResolver (Out.ValueResolver resolverField' resolveFunction) =
    (resolverField', resolveFunction)
viewResolver (Out.EventStreamResolver resolverField' resolveFunction _) =
    (resolverField', resolveFunction)

executeField :: forall m a
    . (MonadCatch m, Serialize a)
    => Type.Value
    -> NonEmpty (Transform.Field m)
    -> Out.Resolver m
    -> [Path]
    -> ExecutorT m a
executeField objectValue fields (viewResolver -> resolverPair) errorPath =
    let Transform.Field _ fieldName inputArguments _ fieldLocation :| _ = fields
     in catches (go fieldName inputArguments)
        [ Handler nullResultHandler
        , Handler (inputCoercionHandler fieldLocation)
        , Handler (resultHandler fieldLocation)
        , Handler (resolverHandler fieldLocation)
        ]
  where
    fieldErrorPath = fieldsSegment fields : errorPath
    inputCoercionHandler :: (MonadCatch m, Serialize a)
        => Full.Location
        -> ExecuteHandler m a InputCoercionException
    inputCoercionHandler _ e@(InputCoercionException _ _ (Just valueNode)) =
        let argumentLocation = getField @"location" valueNode
         in exceptionHandler argumentLocation e
    inputCoercionHandler fieldLocation e = exceptionHandler fieldLocation e
    resultHandler :: (MonadCatch m, Serialize a)
        => Full.Location
        -> ExecuteHandler m a ResultException
    resultHandler = exceptionHandler
    resolverHandler :: (MonadCatch m, Serialize a)
        => Full.Location
        -> ExecuteHandler m a ResolverException
    resolverHandler = exceptionHandler
    nullResultHandler :: (MonadCatch m, Serialize a) => ExecuteHandler m a FieldException
    nullResultHandler e@(FieldException fieldLocation errorPath' next) =
        let newError = constructError next fieldLocation errorPath'
         in if Out.isNonNullType fieldType
             then throwM e
             else returnError newError
    exceptionHandler :: (Exception e, MonadCatch m, Serialize a)
        => Full.Location
        -> ExecuteHandler m a e
    exceptionHandler errorLocation e =
        let newError = constructError e errorLocation fieldErrorPath
         in if Out.isNonNullType fieldType
             then throwM $ FieldException errorLocation fieldErrorPath e
             else returnError newError
    returnError newError = tell (Seq.singleton newError) >> pure null
    go fieldName inputArguments = do
        argumentValues <- coerceArgumentValues argumentTypes inputArguments
        resolvedValue <-
           resolveFieldValue resolveFunction objectValue fieldName argumentValues
        completeValue fieldType fields fieldErrorPath resolvedValue
    (resolverField, resolveFunction) = resolverPair
    Out.Field _ fieldType argumentTypes = resolverField

resolveFieldValue :: MonadCatch m
    => Out.Resolve m
    -> Type.Value
    -> Full.Name
    -> Type.Subs
    -> ExecutorT m Type.Value
resolveFieldValue resolver objectValue _fieldName argumentValues =
    lift $ runReaderT resolver context
  where
    context = Type.Context
        { Type.arguments = Type.Arguments argumentValues
        , Type.values = objectValue
        }

resolveAbstractType :: Monad m
    => Type.Internal.AbstractType m
    -> Type.Subs
    -> ExecutorT m (Maybe (Out.ObjectType m))
resolveAbstractType abstractType values'
    | Just (Type.String typeName) <- HashMap.lookup "__typename" values' = do
        types' <- ExecutorT ask
        case HashMap.lookup typeName types' of
            Just (Type.Internal.ObjectType objectType) ->
                if Type.Internal.instanceOf objectType abstractType
                    then pure $ Just objectType
                    else pure Nothing
            _ -> pure Nothing
    | otherwise = pure Nothing

-- https://spec.graphql.org/October2021/#sec-Value-Completion
completeValue :: (MonadCatch m, Serialize a)
    => Out.Type m
    -> NonEmpty (Transform.Field m)
    -> [Path]
    -> Type.Value
    -> ExecutorT m a
completeValue (Out.isNonNullType -> False) _ _ Type.Null =
    pure null
completeValue outputType@(Out.ListBaseType listType) fields errorPath (Type.List list)
    = foldM go Vector.empty list >>= coerceResult outputType . List . Vector.toList
  where
    go accumulator listItem =
        let updatedPath = Index (Vector.length accumulator) : errorPath
         in Vector.snoc accumulator
            <$> completeValue listType fields updatedPath listItem
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.Int int) =
    coerceResult outputType $ Int int
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.Boolean boolean) =
    coerceResult outputType $ Boolean boolean
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.Float float) =
    coerceResult outputType $ Float float
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.String string) =
    coerceResult outputType $ String string
completeValue outputType@(Out.EnumBaseType enumType) _ _ (Type.Enum enum) =
    let Type.EnumType _ _ enumMembers = enumType
     in if HashMap.member enum enumMembers
        then coerceResult outputType $ Enum enum
        else throwM
            $ ValueCompletionException (show outputType)
            $ Type.Enum enum
completeValue (Out.ObjectBaseType objectType) fields errorPath result
    = executeSelectionSet (mergeSelectionSets fields) objectType result errorPath
completeValue outputType@(Out.InterfaceBaseType interfaceType) fields errorPath result
    | Type.Object objectMap <- result = do
        let abstractType = Type.Internal.AbstractInterfaceType interfaceType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType
                -> executeSelectionSet (mergeSelectionSets fields) objectType result
                $ fieldsSegment fields : errorPath
            Nothing -> throwM
                $ ValueCompletionException (show outputType) result
completeValue outputType@(Out.UnionBaseType unionType) fields errorPath result
    | Type.Object objectMap <- result = do
        let abstractType = Type.Internal.AbstractUnionType unionType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType
                -> executeSelectionSet (mergeSelectionSets fields) objectType result
                $ fieldsSegment fields : errorPath
            Nothing -> throwM
                $ ValueCompletionException (show outputType) result
completeValue outputType _ _ result =
    throwM $ ValueCompletionException (show outputType) result

coerceResult :: (MonadCatch m, Serialize a)
    => Out.Type m
    -> Output a
    -> ExecutorT m a
coerceResult outputType result
    | Just serialized <- serialize outputType result = pure serialized
    | otherwise = throwM $ ResultCoercionException $ show outputType

mergeSelectionSets :: MonadCatch m
    => NonEmpty (Transform.Field m)
    -> Seq (Transform.Selection m)
mergeSelectionSets = foldr forEach mempty
  where
    forEach (Transform.Field _ _ _ fieldSelectionSet _) selectionSet' =
        selectionSet' <> fieldSelectionSet

coerceArgumentValues :: MonadCatch m
    => HashMap Full.Name In.Argument
    -> HashMap Full.Name (Full.Node Transform.Input)
    -> m Type.Subs
coerceArgumentValues argumentDefinitions argumentValues =
    HashMap.foldrWithKey c pure argumentDefinitions mempty
  where
    c argumentName argumentType pure' resultMap =
        forEach argumentName argumentType resultMap >>= pure'
    forEach :: MonadCatch m
         => Full.Name
         -> In.Argument
         -> Type.Subs
         -> m Type.Subs
    forEach argumentName (In.Argument _ variableType defaultValue) resultMap = do
        let matchedMap
                = matchFieldValues' argumentName variableType defaultValue
                $ Just resultMap
         in case matchedMap of
            Just matchedValues -> pure matchedValues
            Nothing
                | Just inputValue <- HashMap.lookup argumentName argumentValues
                    -> throwM
                    $ InputCoercionException (Text.unpack argumentName) variableType
                    $ Just inputValue
                | otherwise -> throwM
                    $ InputCoercionException (Text.unpack argumentName) variableType Nothing

    matchFieldValues' = matchFieldValues coerceArgumentValue
        $ Full.node <$> argumentValues

    coerceArgumentValue inputType transform =
      coerceInputLiteral inputType $ extractArgumentValue transform

    extractArgumentValue (Transform.Int integer) = Type.Int integer
    extractArgumentValue (Transform.Boolean boolean) = Type.Boolean boolean
    extractArgumentValue (Transform.String string) = Type.String string
    extractArgumentValue (Transform.Float float) = Type.Float float
    extractArgumentValue (Transform.Enum enum) = Type.Enum enum
    extractArgumentValue Transform.Null = Type.Null
    extractArgumentValue (Transform.List list) =
      Type.List $ extractArgumentValue <$> list
    extractArgumentValue (Transform.Object object) =
      Type.Object $ extractArgumentValue <$> object
    extractArgumentValue (Transform.Variable variable) = variable

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> OrderedMap (NonEmpty (Transform.Field m))
collectFields objectType = foldl forEach OrderedMap.empty
  where
    forEach groupedFields (Transform.FieldSelection fieldSelection) =
        let Transform.Field maybeAlias fieldName _ _ _ = fieldSelection
            responseKey = fromMaybe fieldName maybeAlias
         in OrderedMap.insert responseKey (fieldSelection :| []) groupedFields
    forEach groupedFields (Transform.FragmentSelection selectionFragment)
        | Transform.Fragment fragmentType fragmentSelectionSet _ <- selectionFragment
        , Type.Internal.doesFragmentTypeApply fragmentType objectType =
            let fragmentGroupedFieldSet =
                    collectFields objectType fragmentSelectionSet
             in groupedFields <> fragmentGroupedFieldSet
        | otherwise = groupedFields

coerceVariableValues :: (Monad m, VariableValue b)
    => HashMap Full.Name (Schema.Type m)
    -> Full.OperationDefinition
    -> HashMap Full.Name b
    -> Either QueryError Type.Subs
coerceVariableValues types operationDefinition' variableValues
    | Full.OperationDefinition _ _ variableDefinitions _ _ _ <-
        operationDefinition'
    = foldr forEach (Right HashMap.empty) variableDefinitions
    | otherwise = pure mempty
  where
    forEach variableDefinition (Right coercedValues) =
        let Full.VariableDefinition variableName variableTypeName defaultValue _ =
                variableDefinition
            defaultValue' = constValue . Full.node <$> defaultValue
         in case Type.Internal.lookupInputType variableTypeName types of
            Just variableType ->
                maybe (Left $ CoercionError variableDefinition) Right
                    $ matchFieldValues
                        coerceVariableValue'
                        variableValues
                        variableName
                        variableType
                        defaultValue'
                    $ Just coercedValues
            Nothing -> Left $ UnknownInputType variableDefinition
    forEach _ coercedValuesOrError = coercedValuesOrError
    coerceVariableValue' variableType value'
        = coerceVariableValue variableType value'
        >>= coerceInputLiteral variableType

constValue :: Full.ConstValue -> Type.Value
constValue (Full.ConstInt i) = Type.Int i
constValue (Full.ConstFloat f) = Type.Float f
constValue (Full.ConstString x) = Type.String x
constValue (Full.ConstBoolean b) = Type.Boolean b
constValue Full.ConstNull = Type.Null
constValue (Full.ConstEnum e) = Type.Enum e
constValue (Full.ConstList list) = Type.List $ constValue . Full.node <$> list
constValue (Full.ConstObject o) =
    Type.Object $ HashMap.fromList $ constObjectField <$> o
  where
    constObjectField Full.ObjectField{value = value', ..} =
        (name, constValue $ Full.node value')

subscribe :: (MonadCatch m, Serialize a)
    => Seq (Transform.Selection m)
    -> Schema m
    -> Full.Location
    -> m (Either Error (ResponseEventStream m a))
subscribe fields schema objectLocation
    | Just objectType <- Schema.subscription schema = do
        let types' = Schema.types schema
        sourceStream <-
            createSourceEventStream types' objectType objectLocation fields
        let traverser =
                mapSourceToResponseEvent types' objectType fields
        traverse traverser sourceStream
    | otherwise = pure $ Left
        $ Error "Schema doesn't support subscriptions." [] []

mapSourceToResponseEvent :: (MonadCatch m, Serialize a)
    => HashMap Full.Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> Out.SourceEventStream m
    -> m (ResponseEventStream m a)
mapSourceToResponseEvent types' subscriptionType fields sourceStream
    = pure
    $ sourceStream
    .| mapMC (executeSubscriptionEvent types' subscriptionType fields)

createSourceEventStream :: MonadCatch m
    => HashMap Full.Name (Type m)
    -> Out.ObjectType m
    -> Full.Location
    -> Seq (Transform.Selection m)
    -> m (Either Error (Out.SourceEventStream m))
createSourceEventStream _types subscriptionType objectLocation fields
    | [fieldGroup] <- OrderedMap.elems groupedFieldSet
    , Transform.Field _ fieldName arguments' _ errorLocation <-
        NonEmpty.head fieldGroup
    , Out.ObjectType _ _ _ fieldTypes <- subscriptionType
    , resolverT <- fieldTypes HashMap.! fieldName
    , Out.EventStreamResolver fieldDefinition _ resolver <- resolverT
    , Out.Field _ _fieldType argumentDefinitions <- fieldDefinition =
        case coerceArgumentValues argumentDefinitions arguments' of
            Left _ -> pure
                $ Left
                $ Error "Argument coercion failed." [errorLocation] []
            Right  argumentValues -> left (singleError [errorLocation])
                <$> resolveFieldEventStream Type.Null argumentValues resolver
    | otherwise = pure
        $ Left
        $ Error "Subscription contains more than one field." [objectLocation] []
  where
    groupedFieldSet = collectFields subscriptionType fields
    singleError :: [Full.Location] -> String -> Error
    singleError errorLocations message = Error (Text.pack message) errorLocations []

resolveFieldEventStream :: MonadCatch m
    => Type.Value
    -> Type.Subs
    -> Out.Subscribe m
    -> m (Either String (Out.SourceEventStream m))
resolveFieldEventStream result args resolver =
    catch (Right <$> runReaderT resolver context) handleEventStreamError
  where
    handleEventStreamError :: MonadCatch m
        => ResolverException
        -> m (Either String (Out.SourceEventStream m))
    handleEventStreamError = pure . Left . displayException
    context = Type.Context
        { Type.arguments = Type.Arguments args
        , Type.values = result
        }

executeSubscriptionEvent :: (MonadCatch m, Serialize a)
    => HashMap Full.Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> Type.Value
    -> m (Response a)
executeSubscriptionEvent types' objectType fields initialValue = do
    (data', errors) <- runWriterT
        $ flip runReaderT types'
        $ runExecutorT
        $ catch (executeSelectionSet fields objectType initialValue [])
        handleException
    pure $ Response data' errors
