{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Data.Url
-- Copyright   :  (c) Athan L. Clark
-- License     :  MIT
--
-- Maintainer  :  Athan L. Clark <athan.clark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- This library helps us distinguish how we present URLs - we might show them
-- relatively, absolutely (with the URI authority - scheme, port, hostname, etc.),
-- or /grounded/ - where the path begins with @/@.
--
-- We leverage Chris Done's <https://hackage.haskell.org/package/path path>
-- library to distinguish relative vs. grounded paths at compile time, and provide
-- some additional features like a file extension and query parameters in
-- <https://hackage.haskell.org/package/path-extra path-extra>.

module Data.Url where

import           Path                                (Abs, Path, Rel, absdir,
                                                      parseAbsDir, parseRelDir,
                                                      parseRelFile, toFilePath,
                                                      (</>))
import           Path.Extended                       (Location (..),
                                                      LocationPath,
                                                      printLocation,
                                                      setFragment, (<&>))
import qualified Path.Extended                       as Path

import           Control.Applicative                 (Alternative (empty, (<|>)))
import           Control.Monad                       (MonadPlus)
import           Control.Monad.Base                  (MonadBase (liftBase),
                                                      liftBaseDefault)
import           Control.Monad.Catch                 (ExitCase (ExitCaseSuccess),
                                                      MonadCatch (catch),
                                                      MonadMask (generalBracket, mask, uninterruptibleMask),
                                                      MonadThrow (throwM))
import           Control.Monad.Cont                  (ContT, MonadCont (callCC))
import           Control.Monad.Except                (ExceptT,
                                                      MonadError (catchError, throwError))
import           Control.Monad.Fix                   (MonadFix)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
#if MIN_VERSION_mtl(2,3,1)
#else
import           Control.Monad.List                  (ListT)
#endif
import           Control.Monad.Logger                (LoggingT,
                                                      MonadLogger (monadLoggerLog),
                                                      NoLoggingT)
import           Control.Monad.Morph                 (MFunctor (hoist),
                                                      MMonad (embed))
import           Control.Monad.Reader                (MonadReader (ask, local),
                                                      ReaderT)
import           Control.Monad.RWS                   (MonadRWS, RWST)
import           Control.Monad.State                 (MonadState (get, put),
                                                      StateT)
import           Control.Monad.Trans                 (MonadTrans (lift))
import           Control.Monad.Trans.Control         (ComposeSt,
                                                      MonadBaseControl (StM, liftBaseWith, restoreM),
                                                      MonadTransControl (StT, liftWith, restoreT),
                                                      defaultLiftBaseWith,
                                                      defaultRestoreM)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
#if MIN_VERSION_transformers(0,6,0)
#else
import           Control.Monad.Trans.Error           (Error, ErrorT)
#endif
import           Control.Monad.Trans.Identity        (IdentityT)
import           Control.Monad.Trans.Maybe           (MaybeT)
import           Control.Monad.Trans.Resource        (MonadResource (liftResourceT),
                                                      ResourceT)
import           Control.Monad.Writer                (MonadWriter (listen, pass, tell),
                                                      WriterT)
import           Data.Functor.Compose                (Compose)
import           Data.Functor.Identity               (Identity (..))
import           Data.List.Split                     (splitOn)
import qualified Data.Strict.Maybe                   as Strict
import qualified Data.Strict.Tuple                   as Strict
import qualified Data.Text                           as T
import           Data.URI                            (URI (..), printURI)
import qualified Data.URI                            as URI
import           Data.URI.Auth                       (URIAuth (..))
import           Data.URI.Auth.Host                  (URIAuthHost (Localhost))
import qualified Data.Vector                         as V
import           System.IO.Unsafe                    (unsafePerformIO)
import           Unsafe.Coerce                       (unsafeCoerce)



-- * Classes

-- | Turns a 'Path' or 'Location' into a 'String', where the rendering behavior
-- (relative, grounded and absolute) is encoded in the monad you use, much like
-- @LoggingT@ and @NoLoggingT@ from <https://hackage.haskell.org/package/monad-logger monad-logger>.
class MonadUrl base (m :: * -> *) | m -> base where
  -- | Create a 'URL' from a 'Location' - either a directory or file, and can include query strings
  -- & fragments.
  locToUrl :: Location base -> m URL

-- | Treated as relative urls
instance MonadUrl Rel IO where
  locToUrl = pure . RelURL

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (MaybeT m) where
  locToUrl     = lift . locToUrl

#if MIN_VERSION_mtl(2,3,1)
#else
instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (ListT m) where
  locToUrl     = lift . locToUrl
#endif

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (ResourceT m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (IdentityT m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (LoggingT m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (NoLoggingT m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (ReaderT r m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         , Monoid w
         ) => MonadUrl base (WriterT w m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (StateT s m) where
  locToUrl     = lift . locToUrl

#if MIN_VERSION_mtl(0,6,0)
#else
instance ( MonadUrl base m
         , Monad m
         , Error e
         ) => MonadUrl base (ErrorT e m) where
  locToUrl     = lift . locToUrl
#endif

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (ContT r m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         ) => MonadUrl base (ExceptT e m) where
  locToUrl     = lift . locToUrl

instance ( MonadUrl base m
         , Monad m
         , Monoid w
         ) => MonadUrl base (RWST r w s m) where
  locToUrl     = lift . locToUrl


-- * Types

-- ** URL

-- | Either a URI (which could include a hostname), or a relative url.
data URL = AbsURL URI | RelURL (Location Rel)

printURL :: URL -> T.Text
printURL x = case x of
  AbsURL y -> printURI y
  RelURL y -> printLocation y


-- ** Relative Urls

-- | When printing a 'URL' generated by a 'Location' in this context,
-- they will always omit the hostname information and print path references relatively
-- (without @./@).
newtype RelativeUrlT m a = RelativeUrlT
  { runRelativeUrlT :: m a
  } deriving ( Show, Eq, Ord, Functor, Applicative, Alternative, Monad, MonadFix
             , MonadPlus, MonadIO, MonadReader r, MonadWriter w, MonadState s
             , MonadRWS r w s, MonadCont, MonadError e, MonadBase b, MonadThrow
             , MonadCatch, MonadMask, MonadLogger)

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (RelativeUrlT m)

type RelativeUrl = RelativeUrlT Identity

instance MonadTrans RelativeUrlT where
  lift = RelativeUrlT

instance MonadTransControl RelativeUrlT where
  type StT RelativeUrlT a = a
  liftWith f = RelativeUrlT (f runRelativeUrlT)
  restoreT = RelativeUrlT

instance Aligned.MonadTransControl RelativeUrlT Identity where
  liftWith f = RelativeUrlT (f (\x -> Identity <$> runRelativeUrlT x))
  restoreT x = RelativeUrlT (runIdentity <$> x)

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (RelativeUrlT m) where
  type StM (RelativeUrlT m) a = ComposeSt RelativeUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b (RelativeUrlT m) (Compose stM Identity) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

instance MFunctor RelativeUrlT where
  hoist f (RelativeUrlT x) = RelativeUrlT (f x)

instance MMonad RelativeUrlT where
  embed f x = f (runRelativeUrlT x)


instance ( Applicative m
         ) => MonadUrl Rel (RelativeUrlT m) where
  locToUrl = pure . RelURL


-- ** Grounded Urls

-- | "Grounded" urls mean that, while omiting host information, paths start with
-- a @/@, like @/foo@.
newtype GroundedUrlT m a = GroundedUrlT
  { runGroundedUrlT :: m a
  } deriving ( Show, Eq, Ord, Functor, Applicative, Alternative, Monad, MonadFix
             , MonadPlus, MonadIO, MonadReader r, MonadWriter w, MonadState s
             , MonadRWS r w s, MonadCont, MonadError e, MonadBase b, MonadThrow
             , MonadCatch, MonadMask, MonadLogger)

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (GroundedUrlT m)

type GroundedUrl = GroundedUrlT Identity

instance MonadTrans GroundedUrlT where
  lift = GroundedUrlT

instance MonadTransControl GroundedUrlT where
  type StT GroundedUrlT a = a
  liftWith f = GroundedUrlT (f runGroundedUrlT)
  restoreT = GroundedUrlT

instance Aligned.MonadTransControl GroundedUrlT Identity where
  liftWith f = GroundedUrlT (f (\x -> Identity <$> runGroundedUrlT x))
  restoreT x = GroundedUrlT (runIdentity <$> x)

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (GroundedUrlT m) where
  type StM (GroundedUrlT m) a = ComposeSt GroundedUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b (GroundedUrlT m) (Compose stM Identity) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

instance MFunctor GroundedUrlT where
  hoist f (GroundedUrlT x) = GroundedUrlT (f x)

instance MMonad GroundedUrlT where
  embed f x = f (runGroundedUrlT x)

instance ( Applicative m
         ) => MonadUrl Abs (GroundedUrlT m) where
  locToUrl     = pure . AbsURL . mkUriLocEmpty


-- ** Absolute Urls

-- | Given a means to take an absolute location and turn it into an URI, make a monad used to
-- construct urls. You can use 'packLocation' to create the @Location Abs -> URI@ function.
newtype AbsoluteUrlT m a = AbsoluteUrlT
  { runAbsoluteUrlT :: (Location Abs -> URI) -> m a
  } deriving Functor

type AbsoluteUrl = AbsoluteUrlT Identity

instance ( Applicative m
         ) => MonadUrl Abs (AbsoluteUrlT m) where
  locToUrl x = AbsoluteUrlT (\f -> pure . AbsURL $ f x)

instance Applicative m => Applicative (AbsoluteUrlT m) where
  pure x = AbsoluteUrlT $ const (pure x)
  f <*> x = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT f r <*> runAbsoluteUrlT x r

instance Alternative m => Alternative (AbsoluteUrlT m) where
  empty = AbsoluteUrlT (const empty)
  (AbsoluteUrlT f) <|> (AbsoluteUrlT g) = AbsoluteUrlT $ \h -> f h <|> g h

instance Monad m => Monad (AbsoluteUrlT m) where
  m >>= f = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT m r >>= (\x -> runAbsoluteUrlT (f x) r)

instance MonadTrans AbsoluteUrlT where
  lift = AbsoluteUrlT . const

instance MonadIO m => MonadIO (AbsoluteUrlT m) where
  liftIO = lift . liftIO

instance ( MonadReader r m
         ) => MonadReader r (AbsoluteUrlT m) where
  ask       = lift ask
  local f (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    local f (x r)

instance ( MonadWriter w m
         ) => MonadWriter w (AbsoluteUrlT m) where
  tell w = lift (tell w)
  listen (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    listen (x r)
  pass (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    pass (x r)

instance ( MonadState s m
         ) => MonadState s (AbsoluteUrlT m) where
  get   = lift get
  put x = lift (put x)

instance ( MonadRWS r w s m
         ) => MonadRWS r w s (AbsoluteUrlT m) where

instance ( MonadCont m
         ) => MonadCont (AbsoluteUrlT m) where
  callCC f = AbsoluteUrlT $ \r ->
    callCC $ \c -> runAbsoluteUrlT (f (AbsoluteUrlT . const . c)) r

instance ( MonadError e m
         ) => MonadError e (AbsoluteUrlT m) where
  throwError = lift . throwError
  catchError (AbsoluteUrlT x) f = AbsoluteUrlT $ \r ->
    catchError (x r) (flip runAbsoluteUrlT r . f)

instance ( MonadBase b m
         ) => MonadBase b (AbsoluteUrlT m) where
  liftBase = liftBaseDefault

instance MonadTransControl AbsoluteUrlT where
  type StT AbsoluteUrlT a = a
  liftWith f = AbsoluteUrlT $ \r ->
    f $ \t -> runAbsoluteUrlT t r
  restoreT = AbsoluteUrlT . const

instance Aligned.MonadTransControl AbsoluteUrlT Identity where
  liftWith f = AbsoluteUrlT $ \r ->
    f $ \x -> Identity <$> runAbsoluteUrlT x r
  restoreT x = AbsoluteUrlT $ \_ -> runIdentity <$> x

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (AbsoluteUrlT m) where
  type StM (AbsoluteUrlT m) a = ComposeSt AbsoluteUrlT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b (AbsoluteUrlT m) (Compose stM Identity) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

instance ( MonadThrow m
         ) => MonadThrow (AbsoluteUrlT m) where
  throwM = lift . throwM

instance ( MonadCatch m
         ) => MonadCatch (AbsoluteUrlT m) where
  catch (AbsoluteUrlT x) f = AbsoluteUrlT $ \r ->
    catch (x r) (flip runAbsoluteUrlT r . f)

instance ( MonadMask m
         ) => MonadMask (AbsoluteUrlT m) where
  mask a = AbsoluteUrlT $ \r ->
    mask $ \u -> runAbsoluteUrlT (a $ q u) r
    where q u (AbsoluteUrlT x) = AbsoluteUrlT (u . x)
  uninterruptibleMask a = AbsoluteUrlT $ \r ->
    uninterruptibleMask $ \u -> runAbsoluteUrlT (a $ q u) r
    where q u (AbsoluteUrlT x) = AbsoluteUrlT (u . x)
  generalBracket acq rel f = do
    a <- acq
    b <- f a
    c <- rel a (ExitCaseSuccess b)
    pure (b, c)

instance ( MonadLogger m
         ) => MonadLogger (AbsoluteUrlT m) where
  monadLoggerLog a b c d = lift (monadLoggerLog a b c d)

instance ( MonadResource m
         ) => MonadResource (AbsoluteUrlT m) where
  liftResourceT = lift . liftResourceT

instance MFunctor AbsoluteUrlT where
  hoist f (AbsoluteUrlT x) = AbsoluteUrlT $ \r ->
    f (x r)

instance MMonad AbsoluteUrlT where
  embed f x = AbsoluteUrlT $ \r ->
    runAbsoluteUrlT (f (runAbsoluteUrlT x r)) r


mkUriLocEmpty :: Location Abs -> URI
mkUriLocEmpty =
  packLocation Strict.Nothing False (URIAuth Strict.Nothing Strict.Nothing Localhost Strict.Nothing)


getPathChunks :: Path base type' -> V.Vector T.Text
getPathChunks = V.fromList . fmap T.pack . splitOn "/" . dropWhile (== '/') . toFilePath



packLocation :: Strict.Maybe T.Text -> Bool -> URIAuth -> Location Abs -> URI
packLocation scheme slashes auth Location{..} =
  URI scheme slashes auth
    (Strict.Just $ case locPath of
        Path.Dir xs  -> (getPathChunks xs, URI.Dir)
        Path.File xs -> (getPathChunks xs, URI.File)
    )
    (V.fromList
        $ map (\(l,r) ->
            T.pack l Strict.:!: maybe Strict.Nothing (Strict.Just . T.pack) r)
            locQueryParams
    )
    (maybe Strict.Nothing (Strict.Just . T.pack) locFragment)


unpackLocation :: URI -> (Strict.Maybe T.Text, Bool, URIAuth, Location Abs)
unpackLocation (URI scheme slashes auth xs qs mFrag) =
  ( scheme
  , slashes
  , auth
  , let path :: LocationPath Abs
        path = case xs of
          Strict.Nothing -> Path.Dir [absdir|/|]
          Strict.Just (xs', dirOrFile)
            | xs' == [] -> Path.Dir [absdir|/|]
            | otherwise ->
              case dirOrFile of
                URI.File -> Path.File $
                  foldl (\acc x -> unsafeCoerce acc </> unsafePerformIO (parseRelFile $ T.unpack x))
                        (unsafePerformIO $ unsafeCoerce <$> parseAbsDir "/")
                        xs'
                URI.Dir -> Path.Dir $
                  foldl (\acc x -> unsafeCoerce acc </> unsafePerformIO (parseRelDir $ T.unpack x <> "/"))
                        (unsafePerformIO $ unsafeCoerce <$> parseAbsDir "/")
                        xs'
        withQs :: Location Abs
        withQs =
          foldl
            (\acc (k Strict.:!: mV) ->
               acc <&> (T.unpack k, Strict.maybe Nothing (Just . T.unpack) mV)
            )
            (Location path [] Nothing)
            qs
    in  setFragment (Strict.maybe Nothing (Just . T.unpack) mFrag) withQs
  )
