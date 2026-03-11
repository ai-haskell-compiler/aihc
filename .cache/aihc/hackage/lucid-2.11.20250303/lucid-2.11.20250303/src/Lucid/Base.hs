{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- Search for UndecidableInstances to see why this is needed
{-# LANGUAGE UndecidableInstances #-}

-- | Base types and combinators.

module Lucid.Base
  (-- * Rendering
   renderText
  ,renderBS
  ,renderTextT
  ,renderBST
  ,renderToFile
   -- * Running
  ,execHtmlT
  ,evalHtmlT
  ,runHtmlT
  ,relaxHtmlT
  ,commuteHtmlT
  -- * Combinators
  ,makeElement
  ,makeElementNoEnd
  ,makeXmlElementNoEnd
  ,makeAttribute
   -- * Types
  ,Html
  ,HtmlT(HtmlT)
  ,Attribute(..)
   -- * Classes
  ,Term(..)
  ,TermRaw(..)
  ,ToHtml(..)
  ,With(..))
  where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Fix (MonadFix(..))
import           Control.Monad.Trans (MonadTrans(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Hashable (Hashable(..))
import           Data.Semigroup (Semigroup (..))
import           Data.Monoid (Monoid (..))
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as T
import           Prelude
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Foldable (toList)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Types

-- | A simple attribute. Don't use the constructor, use
-- 'makeAttribute'.  Attributes are case sensitive, so if you want
-- attributes to be merged properly, use a single case representation.
data Attribute = Attribute !Text !Text
  deriving (Show,Eq)

instance Hashable Attribute where
  hashWithSalt salt (Attribute a b) = salt `hashWithSalt` a `hashWithSalt` b

-- | Simple HTML builder type. Defined in terms of 'HtmlT'. Check out
-- that type for instance information.
--
-- Simple use-cases will just use this type. But if you want to
-- transformer over Reader or something, you can go and use 'HtmlT'.
type Html = HtmlT Identity

-- | A monad transformer that generates HTML. Use the simpler 'Html'
-- type if you don't want to transform over some other monad.
--
-- Don't rely on the internal representation of this type. Use the
-- monad and functor classes.
newtype HtmlT m a =
  HtmlT {runHtmlT :: m (Seq Attribute -> Builder,a)
         -- ^ This is the low-level way to run the HTML transformer,
         -- finally returning an element builder and a value. You can
         -- pass 'mempty' for this argument for a top-level call. See
         -- 'evalHtmlT' and 'execHtmlT' for easier to use functions.
         }

-- | @since 2.9.5
instance MFunctor HtmlT where
  hoist f (HtmlT xs) = HtmlT (f xs)

-- | @since 2.9.7
instance (a ~ (),Applicative m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

-- | Monoid is right-associative, a la the 'Builder' in it.
instance (a ~ (),Applicative m) => Monoid (HtmlT m a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

-- | Based on the monad instance.
instance Applicative m => Applicative (HtmlT m) where
  pure a = HtmlT (pure (mempty,a))
  {-# INLINE pure #-}

  f <*> x = HtmlT $ mk <$> runHtmlT f <*> runHtmlT x
    where mk ~(g, f') ~(h, x') = (g <> h, f' x')
  {-# INLINE (<*>) #-}

  m *> n = HtmlT $ mk <$> runHtmlT m <*> runHtmlT n
    where mk ~(g, _) ~(h, b) = (g <> h, b)
  {-# INLINE (*>) #-}

  m <* n = HtmlT $ mk <$> runHtmlT m <*> runHtmlT n
    where mk ~(g, a) ~(h, _) = (g <> h, a)
  {-# INLINE (<*) #-}

-- | Just re-uses Monad.
instance Functor m => Functor (HtmlT m) where
  fmap f = HtmlT . fmap (fmap f) . runHtmlT

  (<$) = fmap . const
  {-# INLINE (<$) #-}

-- | Basically acts like Writer.
instance Monad m => Monad (HtmlT m) where
  return a = HtmlT (return (mempty,a))
  {-# INLINE return #-}

  m >>= f = HtmlT $ do
    ~(g,a) <- runHtmlT m
    ~(h,b) <- runHtmlT (f a)
    return (g <> h,b)
  {-# INLINE (>>=) #-}

  m >> n = HtmlT $ do
    ~(g, _) <- runHtmlT m
    ~(h, b) <- runHtmlT n
    return (g <> h, b)
  {-# INLINE (>>) #-}

-- | Used for 'lift'.
instance MonadTrans HtmlT where
  lift m =
    HtmlT (do a <- m
              return (\_ -> mempty,a))

instance MonadFix m => MonadFix (HtmlT m) where
  mfix m = HtmlT $ mfix $ \ ~(_, a) -> runHtmlT $ m a

-- MonadReader, MonadState etc instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

-- | @since 2.9.7
instance MonadReader r m => MonadReader r (HtmlT m) where
  ask = lift ask
  local f (HtmlT a) = HtmlT (local f a)

-- | @since 2.9.7
instance MonadState s m => MonadState s (HtmlT m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | @since 2.9.9
instance MonadError e m => MonadError e (HtmlT m) where
    throwError = lift . throwError
    catchError (HtmlT m) h = HtmlT $ catchError m (runHtmlT . h)

-- | @since 2.9.9
instance MonadWriter w m => MonadWriter w (HtmlT m) where
    tell             = lift . tell
    listen (HtmlT x) = HtmlT $ fmap reassoc $ listen x
      where reassoc ((a, b), c) = (a, (b, c))
    pass (HtmlT p)   = HtmlT $ pass $ fmap assoc p
      where assoc (a, (b, c)) = ((a, b), c)

-- | If you want to use IO in your HTML generation.
instance MonadIO m => MonadIO (HtmlT m) where
  liftIO = lift . liftIO

-- | We pack it via string. Could possibly encode straight into a
-- builder. That might be faster.
instance (Monad m,a ~ ()) => IsString (HtmlT m a) where
  fromString = toHtml

-- | Just calls 'renderText'.
instance (m ~ Identity) => Show (HtmlT m a) where
  show = LT.unpack . renderText

-- | Can be converted to HTML.
class ToHtml a where
  -- | Convert to HTML, doing HTML escaping.
  toHtml :: Monad m => a -> HtmlT m ()
  -- | Convert to HTML without any escaping.
  toHtmlRaw :: Monad m => a -> HtmlT m ()

-- | @since 2.9.8
instance (a ~ (), m ~ Identity) => ToHtml (HtmlT m a) where
  toHtml = relaxHtmlT
  toHtmlRaw = relaxHtmlT

instance ToHtml String where
  toHtml    = build . Blaze.fromHtmlEscapedString
  toHtmlRaw = build . Blaze.fromString

instance ToHtml Text where
  toHtml    = build . Blaze.fromHtmlEscapedText
  toHtmlRaw = build . Blaze.fromText

instance ToHtml LT.Text where
  toHtml    = build . Blaze.fromHtmlEscapedLazyText
  toHtmlRaw = build . Blaze.fromLazyText

-- | This instance requires the ByteString to contain UTF-8 encoded
-- text, for the 'toHtml' method. The 'toHtmlRaw' method doesn't care,
-- but the overall HTML rendering methods in this module assume UTF-8.
--
-- @since 2.9.5
instance ToHtml S.ByteString where
  toHtml    = build . Blaze.fromHtmlEscapedText . T.decodeUtf8
  toHtmlRaw = build . Blaze.fromByteString

-- | This instance requires the ByteString to contain UTF-8 encoded
-- text, for the 'toHtml' method. The 'toHtmlRaw' method doesn't care,
-- but the overall HTML rendering methods in this module assume UTF-8.
--
-- @since 2.9.5
instance ToHtml L.ByteString where
  toHtml    = build . Blaze.fromHtmlEscapedLazyText . LT.decodeUtf8
  toHtmlRaw = build . Blaze.fromLazyByteString

-- | Create an 'HtmlT' directly from a 'Builder'.
build :: Monad m => Builder -> HtmlT m ()
build b = HtmlT (return (const b,()))
{-# INLINE build #-}

-- | Used to construct HTML terms.
--
-- Simplest use: p_ = term "p" yields 'Lucid.Html5.p_'.
--
-- Very overloaded for three cases:
--
-- * The first case is the basic @arg@ of @[(Text,Text)]@ which will
--   return a function that wants children.
-- * The second is an @arg@ which is @HtmlT m ()@, in which case the
--   term accepts no attributes and just the children are used for the
--   element.
-- * Finally, this is also used for overloaded attributes, like
--   `Lucid.Html5.style_` or `Lucid.Html5.title_`. If a return type of @(Text,Text)@ is inferred
--   then an attribute will be made.
--
-- The instances look intimidating but actually the constraints make
-- it very general so that type inference works well even in the
-- presence of things like @OverloadedLists@ and such.
class Term arg result | result -> arg where
  -- | Used for constructing elements e.g. @term "p"@ yields 'Lucid.Html5.p_'.
  term :: Text   -- ^ Name of the element or attribute.
       -> arg    -- ^ Either an attribute list or children.
       -> result -- ^ Result: either an element or an attribute.
  term = flip termWith []
  {-# INLINE term #-}

  -- | Use this if you want to make an element which inserts some
  -- pre-prepared attributes into the element.
  termWith :: Text          -- ^ Name.
           -> [Attribute]   -- ^ Attribute transformer.
           -> arg           -- ^ Some argument.
           -> result        -- ^ Result: either an element or an attribute.

-- | Given attributes, expect more child input.
instance (Applicative m,f ~ HtmlT m a) => Term [Attribute] (f -> HtmlT m a) where
  termWith name f = with (makeElement name) . (<> f)

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Applicative m) => Term (HtmlT m a) (HtmlT m a) where
  termWith name f = with (makeElement name) f
  {-# INLINE termWith #-}

-- | Some terms (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance Term Text Attribute where
  termWith key _ value = makeAttribute key value

-- | Same as the 'Term' class, but will not HTML escape its
-- children. Useful for elements like 'Lucid.Html5.style_' or
-- 'Lucid.Html5.script_'.
class TermRaw arg result | result -> arg where
  -- | Used for constructing elements e.g. @termRaw "p"@ yields 'Lucid.Html5.p_'.
  termRaw :: Text   -- ^ Name of the element or attribute.
       -> arg    -- ^ Either an attribute list or children.
       -> result -- ^ Result: either an element or an attribute.
  termRaw = flip termRawWith []
  -- | Use this if you want to make an element which inserts some
  -- pre-prepared attributes into the element.
  termRawWith :: Text          -- ^ Name.
           -> [Attribute]   -- ^ Attribute transformer.
           -> arg           -- ^ Some argument.
           -> result        -- ^ Result: either an element or an attribute.

-- | Given attributes, expect more child input.
instance (Monad m,ToHtml f, a ~ ()) => TermRaw [Attribute] (f -> HtmlT m a) where
  termRawWith name f attrs = with (makeElement name) (attrs <> f) . toHtmlRaw

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m,a ~ ()) => TermRaw Text (HtmlT m a) where
  termRawWith name f = with (makeElement name) f . toHtmlRaw

-- | Some termRaws (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance TermRaw Text Attribute where
  termRawWith key _ value = makeAttribute key value

-- | With an element use these attributes. An overloaded way of adding
-- attributes either to an element accepting attributes-and-children
-- or one that just accepts attributes. See the two instances.
class With a  where
  -- | With the given element(s), use the given attributes.
  with :: a -- ^ Some element, either @Html a@ or @Html a -> Html a@.
       -> [Attribute]
       -> a

-- | For the contentless elements: 'Lucid.Html5.br_'
instance (Functor m) => With (HtmlT m a) where
  with f = \attr -> HtmlT (mk attr <$> runHtmlT f)
    where
      mk attr ~(f',a) = (\attr' -> f' (attr' <> Seq.fromList attr)
                        ,a)

-- | For the contentful elements: 'Lucid.Html5.div_'
instance (Functor m) => With (HtmlT m a -> HtmlT m a) where
  with f = \attr inner -> HtmlT (mk attr <$> runHtmlT (f inner))
    where
      mk attr ~(f',a) = (\attr' -> f' (attr' <> Seq.fromList attr)
                        ,a)

--------------------------------------------------------------------------------
-- Running

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'Blaze.toLazyByteString'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderToFile :: FilePath -> Html a -> IO ()
renderToFile fp = L.writeFile fp . Blaze.toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'Blaze.toLazyByteString'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderBS :: Html a -> ByteString
renderBS = Blaze.toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'Text'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'Blaze.toLazyByteString', and
-- 'LT.decodeUtf8'. Check the source if you're interested in the
-- lower-level behaviour.
--
renderText :: Html a -> LT.Text
renderText = LT.decodeUtf8 . Blaze.toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'ByteString', but in a monad.
--
-- This is a convenience function defined in terms of 'execHtmlT' and
-- 'Blaze.toLazyByteString'. Check the source if you're interested in
-- the lower-level behaviour.
--
renderBST :: Monad m => HtmlT m a -> m ByteString
renderBST = fmap Blaze.toLazyByteString . execHtmlT

-- | Render the HTML to a lazy 'Text', but in a monad.
--
-- This is a convenience function defined in terms of 'execHtmlT' and
-- 'Blaze.toLazyByteString', and 'LT.decodeUtf8'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderTextT :: Monad m => HtmlT m a -> m LT.Text
renderTextT = fmap (LT.decodeUtf8 . Blaze.toLazyByteString) . execHtmlT

--------------------------------------------------------------------------------
-- Running, transformer versions

-- | Build the HTML. Analogous to @execState@.
--
-- You might want to use this is if you want to do something with the
-- raw 'Builder'. Otherwise for simple cases you can just use
-- 'renderText' or 'renderBS'.
execHtmlT :: Monad m
          => HtmlT m a  -- ^ The HTML to generate.
          -> m Builder  -- ^ The @a@ is discarded.
execHtmlT m =
  do (f,_) <- runHtmlT m
     return (f mempty)

-- | Generalize the underlying monad.
--
-- Some builders are happy to deliver results in a pure underlying
-- monad, here 'Identity', but have trouble maintaining the polymorphic
-- type. This utility generalizes from 'Identity'.
--
-- @since 2.9.6
relaxHtmlT :: Monad m
           => HtmlT Identity a  -- ^ The HTML generated purely.
           -> HtmlT m a         -- ^ Same HTML accessible in a polymorphic context.
relaxHtmlT = hoist go
  where
    go :: Monad m => Identity a -> m a
    go = return . runIdentity

-- | Commute inner @m@ to the front.
--
-- This is useful when you have impure HTML generation, e.g. using `StateT`.
-- Recall, there is `MonadState s HtmlT` instance.
--
-- @
-- exampleHtml :: MonadState Int m => HtmlT m ()
-- exampleHtml = ul_ $ replicateM_ 5 $ do
--   x <- get
--   put (x + 1)
--   li_ $ toHtml $ show x
--
-- exampleHtml' :: Monad m => HtmlT m ()
-- exampleHtml' = evalState (commuteHtmlT exampleHtml) 1
-- @
--
-- @since 2.9.9
commuteHtmlT :: (Functor m, Monad n)
             => HtmlT m a      -- ^ unpurely generated HTML
             -> m (HtmlT n a)  -- ^ Commuted monads. /Note:/ @n@ can be 'Identity'
commuteHtmlT (HtmlT xs) = fmap (HtmlT . return) xs

-- | Evaluate the HTML to its return value. Analogous to @evalState@.
--
-- Use this if you want to ignore the HTML output of an action
-- completely and just get the result.
--
-- For using with the 'Html' type, you'll need 'runIdentity' e.g.
--
-- >>> runIdentity (evalHtmlT (p_ "Hello!"))
-- ()
--
evalHtmlT :: Monad m
          => HtmlT m a -- ^ HTML monad to evaluate.
          -> m a       -- ^ Ignore the HTML output and just return the value.
evalHtmlT m =
  do (_,a) <- runHtmlT m
     return a

--------------------------------------------------------------------------------
-- Combinators

-- | Make an attribute builder.
makeAttribute :: Text -- ^ Attribute name.
              -> Text -- ^ Attribute value.
              -> Attribute
makeAttribute x y = Attribute x y

-- | Make an HTML builder.
makeElement :: Functor m
            => Text       -- ^ Name.
            -> HtmlT m a  -- ^ Children HTML.
            -> HtmlT m a -- ^ A parent element.
{-# INLINE[1] makeElement #-}
makeElement name = \m' -> HtmlT (mk <$> runHtmlT m')
  where
    mk ~(f,a) =
      (\attr ->
        s "<" <> Blaze.fromText name
        <> foldlMapWithKey buildAttr attr <> s ">"
        <> f mempty
        <> s "</" <> Blaze.fromText name <> s ">"
      ,a)

-- | Make an HTML builder for elements which have no ending tag.
makeElementNoEnd :: Applicative m
                 => Text       -- ^ Name.
                 -> HtmlT m () -- ^ A parent element.
makeElementNoEnd name =
  HtmlT (pure (\attr -> s "<" <> Blaze.fromText name
                        <> foldlMapWithKey buildAttr attr <> s ">",
                 ()))

-- | Make an XML builder for elements which have no ending tag.
makeXmlElementNoEnd :: Applicative m
                    => Text       -- ^ Name.
                    -> HtmlT m () -- ^ A parent element.
makeXmlElementNoEnd name =
  HtmlT (pure (\attr -> s "<" <> Blaze.fromText name
                        <> foldlMapWithKey buildAttr attr <> s "/>",
                 ()))

-- | Build and encode an attribute.
buildAttr :: Text -> Text -> Builder
buildAttr key val =
  s " " <>
  Blaze.fromText key <>
  if val == mempty
     then mempty
     else s "=\"" <> Blaze.fromHtmlEscapedText val <> s "\""

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: (Text -> Text -> Builder) -> Seq Attribute -> Builder
foldlMapWithKey f attributes =
  case nubOrdMaybe (map fst pairs) of
    Just keyList ->
      foldMap (\k -> fromMaybe mempty (fmap (f k) (M.lookup k values))) keyList
      where values = M.fromListWith (<>) pairs
    Nothing -> foldMap (\(Attribute k v) -> f k v) attributes
  where
    pairs = map (\(Attribute k v) -> (k,v)) (toList attributes)

-- | Do a nubOrd, but only return Maybe if it actually removes anything.
nubOrdMaybe :: Ord a => [a] -> Maybe [a]
nubOrdMaybe = go False Set.empty []
  where
    go (!removed) set acc (x:xs)
      | x `Set.member` set = go True set acc xs
      | otherwise = go removed (Set.insert x set) (x : acc) xs
    go removed _set acc [] =
      if removed
        then pure (reverse acc)
        else Nothing

-- | Convenience function for constructing builders.
s :: String -> Builder
s = Blaze.fromString
{-# INLINE s #-}
