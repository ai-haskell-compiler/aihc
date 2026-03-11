{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TupleSections              #-}

-- | Check that a datatype is deeply strict, ie, it recursively only has strict fields.
module Language.Haskell.TH.DeepStrict
  (
  -- * DeepStrict
    DeepStrict(..)
  , DeepStrictReason(..)
  , DeepStrictWithReason
  -- * Checking data types
  , isDeepStrict
  , isDeepStrictWith
  , assertDeepStrict
  , assertDeepStrictWith
  -- * Context
  , Context(..)
  , Strictness(..)
  , emptyContext
  , FieldKey
  ) where

import Data.Maybe                    (mapMaybe, fromMaybe)
import Data.List                     (foldl')
import Control.Monad                 (when)
import Control.Monad.IO.Class        (MonadIO)
import Control.Monad.Reader          (MonadReader (ask, local), ReaderT (..), asks)
import Control.Monad.Trans           (lift)
import Data.Bifunctor                (first)
import Data.IORef                    (IORef, modifyIORef', newIORef, readIORef)
import Data.Traversable              (for)
import GHC.Stack                     (HasCallStack)
import Language.Haskell.TH           (Q)
import Language.Haskell.TH.Instances ()

import qualified Data.Map                     as ML
import qualified Data.Set                     as S
import qualified Data.Map.Strict              as M
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Datatype as TH
import qualified Language.Haskell.TH.Datatype.TyVarBndr as TH
import qualified Language.Haskell.TH.Ppr      as Ppr
import qualified Language.Haskell.TH.PprLib   as Ppr
import qualified Language.Haskell.TH.Syntax   as TH

newtype DeepStrictM a = DeepStrictM { runDeepStrictM :: ReaderT Context Q a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader Context)
  deriving (TH.Quote, TH.Quasi) via (ReaderT Context Q)

-- | Allow overriding various setting that determine what types we consider deep strict.
data Context = Context
  { contextSpine          :: !(S.Set TH.Type) -- ^ The types we are recursively checking. By the inductive hypothesis, we assume they are DeepStrict.
  , contextCache          :: !(IORef (M.Map TH.Type DeepStrictWithReason))
  , contextOverride       :: !(M.Map TH.Name (Maybe [Strictness])) -- ^ Maps names of types to whether they can be deep strict and if they can which arguments need to be strict
  , contextRecursionDepth :: !Int -- ^ A recursion depth to avoid infinite loops.
  }

-- | The default t'Context'.
emptyContext :: Q Context
emptyContext = do
  emptyCache <- TH.runIO $ newIORef M.empty
  pure $
    Context
      { contextSpine = S.empty
      , contextCache = emptyCache
      , contextOverride = M.empty
      , contextRecursionDepth = 1000
      }

-- | A type is deep strict if and only if for each constructor:
--
--   - All of its fields are strict, ie, they have a @!@ if possible.
--   - The type of of each field is deep strict.
--
-- The Monoid instance allows us to gather up reasons why a type fails to be deep strict.
--
-- === Examples
--
-- @()@ is deep strict because its single constructor doesn't have any fields so it is vacuously deep strict.
--
-- 'Int', 'Char', etc are all deep strict because they are wrappers around unlifted types that cannot be lazy.
--
-- @Maybe Int@ is not deep strict.
-- It has a 'Nothing' constructor, which is fine.
-- But, the 'Just' constructor has a lazy field, which means it's not deep strict.
data DeepStrict reason =
    DeepStrict
  | NotDeepStrict !reason
  deriving (Eq, Ord, Show, TH.Lift, Functor)

type DeepStrictWithReason = DeepStrict [DeepStrictReason]

instance Semigroup reason => Semigroup (DeepStrict reason) where
  DeepStrict <> DeepStrict                       = DeepStrict
  NotDeepStrict reason <> DeepStrict             = NotDeepStrict reason
  DeepStrict <> NotDeepStrict reason             = NotDeepStrict reason
  NotDeepStrict reason1 <> NotDeepStrict reason2 = NotDeepStrict $ reason1 <> reason2

instance Semigroup reason => Monoid (DeepStrict reason) where
  mempty = DeepStrict

-- | Reasons why a type fails to be deep strict.
data DeepStrictReason =
    LazyType !TH.Type ![DeepStrictReason]
  -- ^ The type is lazy.
  | LazyConstructor !TH.Name ![DeepStrictReason]
  -- ^ The type has a lazy constructor.
  | FieldReason !FieldKey ![DeepStrictReason]
  -- ^ One of the fields of the constructor fails to be deep strict.
  | LazyField !FieldKey
  -- ^ One of the fields of the constructor is lazy, ie, doesn't have a @!@.
  | LazyOther !String
  deriving (Eq, Ord, Show, TH.Lift)

instance Ppr.Ppr reason => Ppr.Ppr (DeepStrict reason) where
  ppr DeepStrict             = Ppr.text "DeepStrict" Ppr.$+$ Ppr.text ""
  ppr (NotDeepStrict reason) = Ppr.text "NotDeepStrict" Ppr.$$ Ppr.ppr reason Ppr.$+$ Ppr.text ""

instance Ppr.Ppr DeepStrictReason where
  ppr (LazyType typ rest) = Ppr.hang (Ppr.ppr typ) 2 (Ppr.vcat (map Ppr.ppr rest))
  ppr (LazyConstructor name rest) = Ppr.hang (Ppr.text "con" Ppr.<+> Ppr.ppr name) 2 $ Ppr.vcat $ map Ppr.ppr rest
  ppr (FieldReason (Left ix) rest) = Ppr.hang (Ppr.text "field" Ppr.<+> Ppr.int ix) 2  $ Ppr.vcat $ map Ppr.ppr rest
  ppr (FieldReason (Right name) rest) = Ppr.hang (Ppr.ppr name) 2  $ Ppr.vcat $ map Ppr.ppr rest
  ppr (LazyField (Left ix)) = Ppr.text "field" Ppr.<+> Ppr.int ix Ppr.<+> Ppr.text "is lazy"
  ppr (LazyField (Right name)) = Ppr.text "field" Ppr.<+>  Ppr.ppr name Ppr.<+> Ppr.text "is lazy"
  ppr (LazyOther txt) = Ppr.text txt

giveReasonContext :: ([DeepStrictReason] -> DeepStrictReason) -> DeepStrictWithReason  -> DeepStrictWithReason
giveReasonContext f =  fmap (pure . f)

prettyPanic :: (HasCallStack, Ppr.Ppr x, Show x) => String -> x -> a
prettyPanic context x = error $ context <> ": " <> Ppr.pprint x

data Levity = Lifted | Unlifted
  deriving (Eq, Ord, Show)

-- | Whether a type is used strictly by a data type.
-- We use these to annotate types with deep strictness overrides.
-- Types that have fields labelled as 'Language.Haskell.TH.DeepStrict.Strict' require those types to be deep strict.
-- Types that have fields labelled as 'Language.Haskell.TH.DeepStrict.Lazy' will never be deep strict, but this can be helpful for nicer messages.
data Strictness = Strict | Lazy
  deriving (Eq, Ord, Show)

-- | A function/constructor is weak strict either iff it is strict and the argument isn't unlifted
-- So, it is like strictness but functions/constructors with unlifted/newtype args are WeakLazy
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21380
data WeakStrictness = WeakStrict | WeakLazy
  deriving (Eq, Ord, Show)

data HasBang = HasBang | NoBang
  deriving (Eq, Ord, Show)

type FieldKey = Either Int TH.Name

data FieldInfo =
  FieldInfo
  { fieldInfoName :: FieldKey -- ^ either the index of the field or the name
  , fieldInfoBang :: WeakStrictness
  , fieldInfoType :: TH.Type -- ^ May contain variables bound by datatype args
  } deriving (Eq, Ord, Show)

type Env = ML.Map TH.Name TH.Type

prepareDatatypeInfoEnv :: HasCallStack => [TH.Type] -> [TH.Name] -> (Env, [TH.Type])
prepareDatatypeInfoEnv args argNames = first makeEnv $ splitAt (length argNames) args
  where
    makeEnv = ML.fromList . zip argNames

substituteDatatypeInfoEnv :: HasCallStack => [TH.Type] -> TH.DatatypeInfo -> (TH.DatatypeInfo, [TH.Type])
substituteDatatypeInfoEnv typeArgs datatypeInfo =
  (datatypeInfo { TH.datatypeCons = TH.applySubstitution env (TH.datatypeCons datatypeInfo)
                }
  , typeArgs')
  where
    getVariable :: TH.Type -> Maybe TH.Name
    getVariable (TH.SigT t _k) = getVariable t
    getVariable (TH.VarT v) = Just v
    getVariable _ = Nothing
    freeVars = mapMaybe getVariable $ TH.datatypeInstTypes datatypeInfo
    (env, typeArgs') = prepareDatatypeInfoEnv typeArgs freeVars

decodeDecidedStrictness :: TH.DecidedStrictness -> WeakStrictness
decodeDecidedStrictness TH.DecidedStrict = WeakStrict
decodeDecidedStrictness TH.DecidedUnpack = WeakStrict
decodeDecidedStrictness TH.DecidedLazy   = WeakLazy

reifyLevityType :: HasCallStack => TH.Type -> DeepStrictM Levity
reifyLevityType (TH.ConT name) = reifyLevityName name
reifyLevityType (TH.AppT x _)  = reifyLevityType x
reifyLevityType (TH.ListT{})   = pure Lifted
reifyLevityType (TH.TupleT{})  = pure Lifted
reifyLevityType (TH.ArrowT{})  = pure Lifted
reifyLevityType (TH.UnboxedTupleT{})  = pure Unlifted
reifyLevityType (TH.UnboxedSumT{})  = pure Unlifted
reifyLevityType typ            = prettyPanic "unexpected type" typ

-- | precondtion: name is a type
reifyLevityName :: HasCallStack => TH.Name -> DeepStrictM Levity
reifyLevityName name = do
  kind <- TH.qReifyType name
  pure $ classifyKindLevity kind

-- | Figure out the levity of a type from its kind.
--   If it has type arguments the kind will have arrows, we want to know the final return type.
--   Eg, for (x -> (y -> z)), we care about z
classifyKindLevity :: TH.Kind -> Levity
classifyKindLevity (TH.AppT _ x) = classifyKindLevity x
classifyKindLevity TH.StarT      = Lifted
classifyKindLevity _             = Unlifted


isDatatypeDeepStrict :: HasCallStack => TH.DatatypeInfo -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isDatatypeDeepStrict dt args = isDatatypeDeepStrict' dt' args'
  where
    (dt', args') = substituteDatatypeInfoEnv args dt

isDatatypeDeepStrict' :: HasCallStack => TH.DatatypeInfo -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isDatatypeDeepStrict' datatypeInfo args = do
  consDeepStrict <- traverse (\c -> isConDeepStrict c (TH.datatypeVariant datatypeInfo) args) $ TH.datatypeCons datatypeInfo
  pure $ mconcat consDeepStrict

-- | Figure out the field names for a constructor.
-- We have names for records, we use indices for everything else.
extractFieldNames :: TH.ConstructorVariant -> [FieldKey]
extractFieldNames (TH.RecordConstructor fieldNames) = map Right fieldNames
extractFieldNames _                                 = map Left [0..]

isConDeepStrict :: HasCallStack => TH.ConstructorInfo -> TH.DatatypeVariant -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isConDeepStrict conInfo@(TH.ConstructorInfo { TH.constructorName = conName, TH.constructorFields = fieldTypes }) variant args = do
  fieldBangs <-
    if isNewtype variant
    then pure $ repeat WeakStrict -- newtypes are strict
    else map decodeDecidedStrictness <$> TH.qReifyConStrictness conName
  let fieldNames = extractFieldNames $ TH.constructorVariant conInfo
  let conFields = zipWith3 FieldInfo fieldNames fieldBangs fieldTypes
  fieldDeepStrict <- traverse (`isFieldDeepStrict` args) conFields
  pure $ giveReasonContext (LazyConstructor conName) $ mconcat fieldDeepStrict

isNewtype :: TH.DatatypeVariant -> Bool
isNewtype TH.Newtype         = True
isNewtype TH.NewtypeInstance = True
isNewtype _                  = False

isFieldDeepStrict :: HasCallStack => FieldInfo -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isFieldDeepStrict (FieldInfo fieldName fieldWeakStrictness fieldType) args = do
  fieldTypeRecStrict <- isTypeDeepStrict fieldType args
  fieldLevity <- reifyLevityType fieldType
  case (fieldWeakStrictness, fieldTypeRecStrict, fieldLevity) of
    (WeakStrict, DeepStrict, _) -> pure DeepStrict
    (WeakLazy, DeepStrict, Unlifted) -> pure DeepStrict
    (WeakLazy, strictness, Lifted) -> pure $ NotDeepStrict [LazyField fieldName] <> inField strictness
    (_, strictness, _) -> pure $ inField strictness
  where
    inField = giveReasonContext (FieldReason fieldName)

getCachedDeepStrict :: HasCallStack => TH.Type -> DeepStrictM (Maybe DeepStrictWithReason)
getCachedDeepStrict typ = do
  cacheRef <- asks contextCache
  cache <- TH.qRunIO $ readIORef cacheRef
  pure $ M.lookup typ cache

putCachedDeepStrict :: HasCallStack => TH.Type -> DeepStrictWithReason  -> DeepStrictM ()
putCachedDeepStrict typ val = do
  cacheRef <- asks contextCache
  TH.qRunIO . modifyIORef' cacheRef $ M.insert typ (const [LazyOther $ Ppr.pprint typ <> " is lazy see above"] <$> val)

isTypeDeepStrict :: HasCallStack => TH.Type -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isTypeDeepStrict typ args = do
  ctxt <- ask
  cachedVal <- getCachedDeepStrict typ
  when (contextRecursionDepth ctxt <= 0) . fail $ "Recursion depth reached. Try adding an override for this type: " <> take 1000 (show typ)
  case (cachedVal, S.member typ $ contextSpine ctxt) of
    (Just val, _) -> pure val
    (_, True) -> pure DeepStrict -- by inductive hypothesis
    _ ->
      local (\_ctxt ->
        ctxt {contextSpine = S.insert typ (contextSpine ctxt), contextRecursionDepth = contextRecursionDepth ctxt - 1}) $ do
          ret <- inType <$> isTypeDeepStrict' typ args
          putCachedDeepStrict typ ret
          pure ret
  where
    inType = giveReasonContext (LazyType typ)

isTypeDeepStrict' :: HasCallStack => TH.Type -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isTypeDeepStrict' (TH.ConT typeName) args = isNameDeepStrict typeName args
isTypeDeepStrict' (TH.AppT func arg) args = isTypeDeepStrict' func (arg:args)
isTypeDeepStrict' (TH.TupleT 0) _         = pure DeepStrict -- () is DeepStrict
isTypeDeepStrict' (TH.TupleT n) args      = isNameDeepStrict (TH.tupleTypeName n) args
isTypeDeepStrict' (TH.ArrowT{}) _         = pure $ NotDeepStrict [LazyOther "Functions are lazy"]
isTypeDeepStrict' (TH.ListT{}) args       = isNameDeepStrict ''[] args
isTypeDeepStrict' (TH.UnboxedTupleT arity) args = isNameDeepStrict (TH.unboxedTupleTypeName arity) args
isTypeDeepStrict' (TH.UnboxedSumT arity) args  = isNameDeepStrict (TH.unboxedSumTypeName arity) args
isTypeDeepStrict' typ _                   = prettyPanic "Unexpected type" typ

-- | figure out whether a newtype/data family is deep strict
isDataFamilyDeepStrict
  :: (p1 -> TH.Name -> [TH.TyVarBndr TH.BndrVis] -> p2 -> p3 -> p4 -> TH.Dec)
  -> TH.Name
  -> [TH.Type]
  -> p1
  -> Maybe [TH.TyVarBndr ()]
  -> TH.Type
  -> p2
  -> p3
  -> p4
  -> DeepStrictM DeepStrictWithReason
isDataFamilyDeepStrict dConstr typeName args cxt mTyVarBndrs typ kind con deriv =  do
  let tyVarBndrs = fromMaybe [] mTyVarBndrs
  let
    mkRequiredVis (TH.PlainTV x _) = TH.plainTVFlag x TH.BndrReq
    mkRequiredVis (TH.KindedTV x _ k) = TH.kindedTVFlag x TH.BndrReq k
  let appliedArgs = foldl' TH.AppT (TH.ConT typeName) args
  let d = dConstr cxt (TH.mkName $ TH.pprint $ appliedArgs)  (map mkRequiredVis tyVarBndrs) kind con deriv
  datatypeInfo <- DeepStrictM $ lift $ TH.normalizeDec d
  -- figure out the mapping from the input to the free variables in the family instance
  unified <- DeepStrictM . lift $ TH.unifyTypes [appliedArgs, typ]
  let tyVarNotFound = error "unmatched type variable in a data family definition"
  -- now our args are those free variables with the mapping
  let args' = map (fromMaybe tyVarNotFound . flip M.lookup unified . TH.tvName) tyVarBndrs
  isDatatypeDeepStrict datatypeInfo args'

-- | Is this type constructor applied to these arguments deep strict
isNameDeepStrict :: HasCallStack => TH.Name -> [TH.Type] -> DeepStrictM DeepStrictWithReason
isNameDeepStrict typeName args = do
  ctxt <- ask
  case M.lookup typeName $ contextOverride ctxt of
    Nothing -> do
      info <- DeepStrictM $ lift $ TH.reify typeName
      case info of
        -- th-abstraction can't handle type synonyms.
        -- let's treat a type synonym as just the RHS
        TH.TyConI (TH.TySynD _name tyvarbndrs rhs) -> do
          let (env, args') = prepareDatatypeInfoEnv args (map TH.tvName tyvarbndrs)
          isTypeDeepStrict (TH.applySubstitution env rhs) args'
        -- handle type/data families
        TH.FamilyI{} -> do
          instances <- DeepStrictM $ lift $ TH.reifyInstances typeName args
          case instances of
            -- a type synonym instance is handled like a type synonym:
            -- just treat it as the RHS.
            (TH.TySynInstD (TH.TySynEqn _ lhs rhs)):_ -> do
              let (env, args') = prepareDatatypeInfoEnv args (TH.freeVariables lhs)
              isTypeDeepStrict (TH.applySubstitution env rhs) args'
            -- let's construct a dummy datatype decleration
            (TH.DataInstD cxt mTyVarBndrs typ kind con deriv):_ -> do
              isDataFamilyDeepStrict TH.DataD typeName args cxt mTyVarBndrs typ kind con deriv
            (TH.NewtypeInstD cxt mTyVarBndrs typ kind con deriv):_ -> do
              isDataFamilyDeepStrict TH.NewtypeD typeName args cxt mTyVarBndrs typ kind con deriv
            _ -> error "Unsupported/ambiguous data/type family"
        _ -> do
          datatypeInfo <- DeepStrictM $ lift $ TH.normalizeInfo info
          isDatatypeDeepStrict datatypeInfo args
    Just Nothing -> pure $ NotDeepStrict [LazyOther "This type is marked as lazy"]
    Just (Just strictnessReqs) ->
      fmap mconcat . for (zip strictnessReqs args) $ \case
        (Lazy, _)     -> pure DeepStrict
        (Strict, typ) -> isTypeDeepStrict typ []

-- | Determine if a type is deep strict
-- Invariant: The type doesn't contain any free variables, eg, @Maybe a@ will fail.
isDeepStrict :: TH.Type -> Q DeepStrictWithReason
isDeepStrict typ = do
  emptyC <- emptyContext
  isDeepStrictWith emptyC typ

isDeepStrictWith :: Context -> TH.Type -> Q DeepStrictWithReason
isDeepStrictWith context typ = do
  typRes <- TH.resolveTypeSynonyms typ
  runReaderT (runDeepStrictM $ isTypeDeepStrict typRes []) context


-- | Assert that a type is deep strict.
-- If the type isn't deep strict then this will produce an error with the reasons why.
assertDeepStrict :: TH.Type -> Q [TH.Dec]
assertDeepStrict typ = do
  emptyC <- emptyContext
  assertDeepStrictWith emptyC typ

data DeepStrictAssertionFailed = DeepStrictAssertionFailed TH.Type [DeepStrictReason]

instance Ppr.Ppr DeepStrictAssertionFailed where
  ppr (DeepStrictAssertionFailed typ reason) =
   Ppr.ppr typ Ppr.$+$ Ppr.text "is not Deep Strict, because: "
   Ppr.$$ Ppr.ppr reason

assertDeepStrictWith :: Context -> TH.Type -> Q [TH.Dec]
assertDeepStrictWith context typ = do
  result <- isDeepStrictWith context typ
  case result of
    DeepStrict -> pure []
    NotDeepStrict reason ->
      fail $ Ppr.pprint $ DeepStrictAssertionFailed typ reason
