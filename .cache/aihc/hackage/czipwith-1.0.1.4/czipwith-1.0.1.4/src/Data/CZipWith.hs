{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Lifted versions of the @'Functor'@, @'Pointed'@, @'Apply'@
-- and @'Traversable'@ classes, plus template-haskell magic to automatically
-- derive instances.
-- \"Lifted\" because these classes are about datatypes parameterized over a
-- constructor (i.e. of kind @(* -> *) -> *@). For example
-- @fmap :: (a -> b) -> f a -> f b@ becomes
-- @cMap :: (forall a . f a -> g a) -> c f -> c g@.
--
-- For the lifted version of @Applicative@, we focus on 'liftA2' instead of
-- '\<*\>' as this is the only way to make the lifted version work. As a
-- consequence, the class and method are named after 'zipWith' because of
-- the similarity of the signatures and the semantics.
--
-- @
-- liftA2   :: Applicative f =>         (g   -> h   -> i  ) -> f g -> f h -> f i
-- zipWith  ::                          (g   -> h   -> i  ) -> [g] -> [h] -> [i]
-- cZipWith :: CZipWith k => (forall a . g a -> h a -> i a) -> k g -> k h -> k i
-- @
--
-- Types of the corresponding kind occur for example when handling program
-- configuration: When we define our an example configuration type like
--
-- @
-- data MyConfig f = MyConfig
--   { flag_foo       :: f Bool
--   , flag_bar       :: f Bool
--   , flag_someLimit :: f Int
--   }
-- @
--
-- then
--
-- * @MyConfig Maybe@ can be used as the result-type of parsing the
--   commandline or a configuration file; it includes the option that some
--   field was not specified;
-- * @MyConfig Identity@ can be used to represent both the default
--   configuration and the actual configuration derived from
--   defaults and the user input;
-- * @MyConfig (Const Text)@ type to represent documentation for our config,
--   to be displayed to the user.
--
-- This has the advantage that our configuration is defined in one place only,
-- so that changes are easy to make and we do not ever run into any internal
-- desynchonization of different datatypes. And once we obtained the final
-- config @:: MyConfig Identity@, we don't have to think about @Nothing@ cases
-- anymore.
--
-- @'cPointed'@ can initialize such polymorphic containers, and @'CZipWith'@
-- further helps with this use-case, more specifically the merging of
-- input and default config: we can express the merging of user/default config
-- @:: MyConfig Maybe -> MyConfig Identity -> MyConfig Identity@ in terms of
-- @'cZipWith'@. The instances are simple boilerplate and thus can be realized
-- using the provided template-haskell.
--
-- As an example for such usage, the
-- <https://github.com/lspitzner/brittany brittany> package uses this approach
-- together with using automatically-derived Semigroup-instances that allow
-- merging of config values (for example when commandline args do not override,
-- but are added to those settings read from config file). See
-- <https://github.com/lspitzner/brittany/blob/master/src/Language/Haskell/Brittany/Config/Types.hs the module containing the config type>.
module Data.CZipWith
  ( CFunctor(..)
  , CPointed(..)
  , CZipWith(..)
  , CZipWithM(..)
  , cSequence
  , deriveCPointed
  , deriveCZipWith
  , deriveCZipWithM
  )
where



import           Data.Kind (Type)
import           Data.Functor.Compose
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax hiding (Type)


-- | The "lifted Apply" class
class CPointed c where
  cPoint :: (forall a . f a) -> c f


-- | The "lifted Functor" class
class CFunctor c where
  cMap :: (forall a . f a -> g a) -> c f -> c g
  default cMap :: CZipWith c => (forall a . f a -> g a) -> c f -> c g
  cMap f k = cZipWith (\x _ -> f x) k k


-- | laws:
--
-- * @'cZipWith' (\\x _ -> x) g _ = g@
-- * @'cZipWith' (\\_ y -> y) _ h = h@
--
-- This class seems to be some kind of "lifted" version of 'Applicative'
-- (or rather: of @'Apply'@),
-- but it also seems to share an important property with the
-- <https://hackage.haskell.org/package/distributive-0.5.2/docs/Data-Distributive.html#t:Distributive Distributive>
-- class from the
-- <https://hackage.haskell.org/package/distributive distributive> package,
-- even when @'Distributive'@ and @'CZipWith'@ methods don't appear all that
-- similar. From the corresponding docs:
--
-- > To be distributable a container will need to have a way to consistently
-- > zip a potentially infinite number of copies of itself. This effectively
-- > means that the holes in all values of that type, must have the same
-- > cardinality, fixed sized vectors, infinite streams, functions, etc.
-- > and no extra information to try to merge together.
--
-- Especially "all values of that type must have the same cardinality" is
-- true for instances of CZipWith, the only difference being that the "holes"
-- are instantiations of the @f :: * -> *@ to some type, where they are simply
-- @a :: *@ for @'Distributive'@.
--
-- For many @'Distributive'@ instances there are corresponding datatypes that
-- are instances of @'CZipWith'@ (although they do not seem particularly
-- useful..), for example:
--
-- @
-- newtype CUnit a f = CUnit (f a)      -- corresponding to 'Identity'
-- data CPair a b f = CPair (f a) (f b) -- corresponding to 'data MonoPair a = MonoPair a a'
--                                      -- (Pair being a trivial fixed-size vector example)
-- data CStream a f = CStream (f a) (CStream a f) -- corresponding to an infinite stream
-- @
class CZipWith (k :: (Type -> Type) -> Type) where
  -- | zipWith on constructors instead of values.
  cZipWith :: (forall a . g a -> h a -> i a) -> k g -> k h -> k i


-- | Where 'CZipWith' is a "lifted @Apply@", this is a "lifted 'Traversable'".
--
-- laws:
--
-- [/naturality/]
--   @t . 'cTraverse' f = 'cTraverse' (t . f)@
--   for every applicative transformation @t@
--
-- [/identity/]
--   @'cTraverse' Identity = Identity@
--
-- [/composition/]
--   @'cTraverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('cTraverse' g) . 'cTraverse' f@
--
-- and @cZipWithM f k l@ must behave like
-- @cTraverse getCompose (cZipWith (\x y -> Compose (f x y)) k l)@
-- 
class CZipWith c => CZipWithM c where
  {-# MINIMAL cTraverse | cZipWithM #-}

  cTraverse :: Applicative m => (forall a . f a -> m (g a)) -> c f -> m (c g)
  cTraverse f k = cZipWithM (\x _ -> f x) k k
  cZipWithM :: Applicative m => (forall a . f a -> g a -> m (h a)) -> c f -> c g -> m (c h)
  cZipWithM f k l =
    cTraverse getCompose $ cZipWith (\x y -> Compose (f x y)) k l

-- | The equivalent of @'Traversable'@'s @'sequence'@/@'sequenceA'@
cSequence :: Applicative m => CZipWithM c => (c (Compose m f)) -> m (c f)
cSequence = cTraverse getCompose


-- | Derives a 'cPointed' instance for a datatype of kind @(* -> *) -> *@.
--
-- Requires that for this datatype (we shall call its argument @f :: * -> *@ here)
--
-- * there is exactly one constructor;
-- * all fields in the one constructor are either of the form @f x@ for some
--   @x@ or of the form @X f@ for some type @X@ where there is an
--   @instance cPointed X@.
--
-- For example, the following would be valid usage:
--
-- @
-- data A f = A
--   { a_str  :: f String
--   , a_bool :: f Bool
--   }
--
-- data B f = B
--   { b_int   :: f Int
--   , b_float :: f Float
--   , b_a     :: A f
--   }
--
-- derivecPointed ''A
-- derivecPointed ''B
-- @
--
-- This produces the following instances:
--
-- @
-- instance cPointed A where
--   cPoint f = A f f
--
-- instance cPointed B where
--   cPoint f = B f f (cPoint f f)
-- @
deriveCPointed :: Name -> DecsQ
deriveCPointed name = do
  info <- reify name
  case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD _ _ [_tyvarbnd] _ [con] []) -> do
#else
    TyConI (DataD _ _ [_tyvarbnd] [con] []) -> do
#endif
      let (cons, elemTys) = case con of
            NormalC c tys -> (c, tys <&> \(_, t) -> t)
            RecC    c tys -> (c, tys <&> \(_, _, t) -> t)
            _ ->
              error
                $  "Deriving requires non-GADT, non-infix data type/record!"
                ++ " (Found: "
                ++ show con
                ++ ")"
      let tyvar = case _tyvarbnd of
#if MIN_VERSION_template_haskell(2,17,0)
            PlainTV n _    -> n
            KindedTV n _ _ -> n
#else
            PlainTV n    -> n
            KindedTV n _ -> n
#endif
      let fQ   = mkName "f"
      let pats = [varP fQ]
      let
        params = elemTys <&> \ty -> case ty of
          AppT (VarT a1) _ | a1 == tyvar      -> varE fQ
          AppT ConT{} (VarT a2) | a2 == tyvar -> [|$(varE 'cPoint) $(varE fQ)|]
          _ ->
            error
              $ "All constructor arguments must have either type k a for some a or C k for some C (with instance CZip C)!"
              ++ " (Found: "
              ++ show ty
              ++ ")"
      let body = normalB $ appsE $ conE cons : params
      let funQ = funD 'cPoint [clause pats body []]
      sequence [instanceD (cxt []) [t|CPointed $(conT name)|] [funQ]]
    TyConI (DataD{}) ->
      error
        $  "datatype must have kind (* -> *) -> *!"
        ++ " (Found: "
        ++ show info
        ++ ")"
    _ ->
      error
        $  "name does not refer to a datatype!"
        ++ " (Found: "
        ++ show info
        ++ ")"


-- | Derives a 'CZipWith' instance for a datatype of kind @(* -> *) -> *@.
--
-- Requires that for this datatype (we shall call its argument @f :: * -> *@ here)
--
-- * there is exactly one constructor;
-- * all fields in the one constructor are either of the form @f x@ for some
--   @x@ or of the form @X f@ for some type @X@ where there is an
--   @instance CZipWith X@.
--
-- For example, the following would be valid usage:
--
-- @
-- data A f = A
--   { a_str  :: f String
--   , a_bool :: f Bool
--   }
--
-- data B f = B
--   { b_int   :: f Int
--   , b_float :: f Float
--   , b_a     :: A f
--   }
--
-- deriveCZipWith ''An
-- deriveCZipWith ''B
-- @
--
-- This produces the following instances:
--
-- @
-- instance CZipWith A where
--   cZipWith f (A x1 x2) (A y1 y2) = A (f x1 y1) (f x2 y2)
--
-- instance CZipWith B wheren
--   cZipWith f (B x1 x2 x3) (B y1 y2 y3) =
--     B (f x1 y1) (f x2 y2) (cZipWith f x3 y3)
-- @
deriveCZipWith :: Name -> DecsQ
deriveCZipWith name = do
  info <- reify name
  case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD _ _ [tyvarbnd] _ [con] []) -> do
#else
    TyConI (DataD _ _ [tyvarbnd] [con] []) -> do
#endif
      let (cons, elemTys) = case con of
            NormalC c tys -> (c, tys <&> \(_, t) -> t)
            RecC    c tys -> (c, tys <&> \(_, _, t) -> t)
            _ ->
              error
                $  "Deriving requires non-GADT, non-infix data type/record!"
                ++ " (Found: "
                ++ show con
                ++ ")"
      let tyvar = case tyvarbnd of
#if MIN_VERSION_template_haskell(2,17,0)
            PlainTV n _    -> n
            KindedTV n _ _ -> n
#else
            PlainTV n    -> n
            KindedTV n _ -> n
#endif
      let fQ       = mkName "f"
      let indexTys = zip [1 ..] elemTys
      let indexTysVars = indexTys <&> \(i :: Int, ty) ->
            (ty, mkName $ "x" ++ show i, mkName $ "y" ++ show i)
      let dPat1     = conP cons $ indexTysVars <&> \(_, x, _) -> varP x
      let dPat2     = conP cons $ indexTysVars <&> \(_, _, x) -> varP x
      let pats      = [varP fQ, dPat1, dPat2]
      let
        params = indexTysVars <&> \(ty, x, y) -> case ty of
          AppT (VarT a1) _ | a1 == tyvar -> [|$(varE fQ) $(varE x) $(varE y)|]
          AppT ConT{} (VarT a2) | a2 == tyvar ->
            [|cZipWith $(varE fQ) $(varE x) $(varE y)|]
          _ ->
            error
              $ "All constructor arguments must have either type k a for some a or C k for some C (with instance CZip C)!"
              ++ " (Found: "
              ++ show ty
              ++ ")"
      let body = normalB $ appsE $ conE cons : params
      let funQ = funD 'cZipWith [clause pats body []]
      sequence [instanceD (cxt []) [t|CZipWith $(conT name)|] [funQ]]
    TyConI (DataD{}) ->
      error
        $  "datatype must have kind (* -> *) -> *!"
        ++ " (Found: "
        ++ show info
        ++ ")"
    _ ->
      error
        $  "name does not refer to a datatype!"
        ++ " (Found: "
        ++ show info
        ++ ")"


-- | Derives a 'CZipWithM' instance for a datatype of kind @(* -> *) -> *@.
--
-- Requires that for this datatype (we shall call its argument @f :: * -> *@ here)
--
-- * there is exactly one constructor;
-- * all fields in the one constructor are either of the form @f x@ for some
--   @x@ or of the form @X f@ for some type @X@ where there is an
--   @instance CZipWithM X@.
--
-- For example, the following would be valid usage:
--
-- @
-- data A f = A
--   { a_str  :: f String
--   , a_bool :: f Bool
--   }
--
-- data B f = B
--   { b_int   :: f Int
--   , b_float :: f Float
--   , b_a     :: A f
--   }
--
-- deriveCZipWithM ''A
-- deriveCZipWithM ''B
-- @
--
-- This produces the following instances:
--
-- @
-- instance CZipWithM A where
--   cZipWithM f (A x1 x2) (A y1 y2) = A \<$\> f x1 y1 \<*\> f x2 y2
--
-- instance CZipWith B where
--   cZipWithM f (B x1 x2 x3) (B y1 y2 y3) =
--     B \<$\> f x1 y1 \<*\> f x2 y2 \<*\> cZipWithM f x3 y3
-- @
deriveCZipWithM :: Name -> DecsQ
deriveCZipWithM name = do
  info <- reify name
  case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD _ _ [tyvarbnd] _ [con] []) -> do
#else
    TyConI (DataD _ _ [tyvarbnd] [con] []) -> do
#endif
      let (cons, elemTys) = case con of
            NormalC c tys -> (c, tys <&> \(_, t) -> t)
            RecC    c tys -> (c, tys <&> \(_, _, t) -> t)
            _ ->
              error
                $  "Deriving requires non-GADT, non-infix data type/record!"
                ++ " (Found: "
                ++ show con
                ++ ")"
      let tyvar = case tyvarbnd of
#if MIN_VERSION_template_haskell(2,17,0)
            PlainTV n _    -> n
            KindedTV n _ _ -> n
#else
            PlainTV n    -> n
            KindedTV n _ -> n
#endif
      let fQ       = mkName "f"
      let indexTys = zip [1 ..] elemTys
      let indexTysVars = indexTys <&> \(i :: Int, ty) ->
            (ty, mkName $ "x" ++ show i, mkName $ "y" ++ show i)
      let dPat1     = conP cons $ indexTysVars <&> \(_, x, _) -> varP x
      let dPat2     = conP cons $ indexTysVars <&> \(_, _, x) -> varP x
      let pats      = [varP fQ, dPat1, dPat2]
      let
        params = indexTysVars <&> \(ty, x, y) -> case ty of
          AppT (VarT a1) _ | a1 == tyvar -> [|$(varE fQ) $(varE x) $(varE y)|]
          AppT ConT{} (VarT a2) | a2 == tyvar ->
            [|cZipWithM $(varE fQ) $(varE x) $(varE y)|]
          _ ->
            error
              $ "All constructor arguments must have either type k a for some a or C k for some C (with instance CZip C)!"
              ++ " (Found: "
              ++ show ty
              ++ ")"
      let body = normalB $ case params of
            [] -> [|pure $(conE cons)|]
            (p1:pr) -> foldl (\x p -> [|$x <*> $p|]) [|$(conE cons) <$> $p1|] pr
      let funQ = funD 'cZipWithM [clause pats body []]
      sequence [instanceD (cxt []) [t|CZipWithM $(conT name)|] [funQ]]
    TyConI (DataD{}) ->
      error
        $  "datatype must have kind (* -> *) -> *!"
        ++ " (Found: "
        ++ show info
        ++ ")"
    _ ->
      error
        $  "name does not refer to a datatype!"
        ++ " (Found: "
        ++ show info
        ++ ")"


-- local utility, not worth an extra dependency
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

