{-# LANGUAGE CPP, OverloadedStrings #-}
{-# LANGUAGE BangPatterns, RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ,generalizeHtmlT
  ,commuteHtmlT2
  ,hoistHtmlT
  -- * Combinators
  ,makeElement
  ,makeElementNoEnd
  ,makeAttributes
  ,makeAttributesRaw
  ,Attributes
   -- * Types
  ,Html
  ,HtmlT
   -- * Classes
  ,Term(..)
  ,TermRaw(..)
  ,ToHtml(..)

   -- * Deprecated
  ,relaxHtmlT
  ,commuteHtmlT
  )
  where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix (MonadFix(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (StateT(..), modify', mapStateT)
import qualified Data.ByteString as S
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (toList)
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Tuple (swap)
import           Prelude

--------------------------------------------------------------------------------
-- Types

-- | A simple attribute. Don't use the constructor, use
-- 'makeAttributes'.  Attributes are case sensitive, so if you want
-- attributes to be merged properly, use a single case representation.
data Attribute = Attribute !Text !Builder

-- | A list of attributes.
newtype Attributes = Attributes { unAttributes :: Seq Attribute }
  deriving (Monoid, Semigroup)

-- | Simple HTML builder type. Defined in terms of 'HtmlT'. Check out
-- that type for instance information.
--
-- Simple use-cases will just use this type. But if you want to
-- transformer over Reader or something, you can go and use 'HtmlT'.
type Html = HtmlT Identity

-- | A monad transformer that generates HTML. Use the simpler 'Html'
-- type if you don't want to transform over some other monad.
newtype HtmlT m a = HtmlT { unHtmlT :: StateT Builder m a }
  deriving (Monad, Functor, Applicative, MonadTrans, MonadFix)

-- | @since 2.9.7
instance MonadReader r m => MonadReader r (HtmlT m) where
  ask = lift ask
  local f (HtmlT a) = HtmlT (local f a)

-- | @since 2.9.7
instance MonadState s m => MonadState s (HtmlT m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | Run the HtmlT transformer.
runHtmlT :: Monad m => HtmlT m a -> m (Builder, a)
runHtmlT = fmap swap . flip runStateT mempty . unHtmlT

-- | Switch the underlying monad.
hoistHtmlT :: (Monad m, Monad n) => (forall a. m a -> n a) -> HtmlT m b -> HtmlT n b
hoistHtmlT f (HtmlT xs) = HtmlT (mapStateT f xs)

-- | @since 2.9.7
instance (a ~ (),Monad m) => Semigroup (HtmlT m a) where
  (<>) = liftA2 (<>)

-- | Monoid is right-associative, a la the 'Builder' in it.
instance (a ~ (),Monad m) => Monoid (HtmlT m a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

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
  toHtml = generalizeHtmlT
  toHtmlRaw = generalizeHtmlT

instance ToHtml String where
  toHtml    = write . Blaze.fromHtmlEscapedString
  toHtmlRaw = write . Blaze.fromString

instance ToHtml Text where
  toHtml    = write . Blaze.fromHtmlEscapedText
  toHtmlRaw = write . Blaze.fromText

instance ToHtml LT.Text where
  toHtml    = write . Blaze.fromHtmlEscapedLazyText
  toHtmlRaw = write . Blaze.fromLazyText

-- | This instance requires the ByteString to contain UTF-8 encoded
-- text, for the 'toHtml' method. The 'toHtmlRaw' method doesn't care,
-- but the overall HTML rendering methods in this module assume UTF-8.
--
-- @since 2.9.5
instance ToHtml S.ByteString where
  toHtml    = write . Blaze.fromHtmlEscapedText . T.decodeUtf8
  toHtmlRaw = write . Blaze.byteString

-- | This instance requires the ByteString to contain UTF-8 encoded
-- text, for the 'toHtml' method. The 'toHtmlRaw' method doesn't care,
-- but the overall HTML rendering methods in this module assume UTF-8.
--
-- @since 2.9.5
instance ToHtml L.ByteString where
  toHtml    = write . Blaze.fromHtmlEscapedLazyText . LT.decodeUtf8
  toHtmlRaw = write . Blaze.lazyByteString

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

-- | Given attributes, expect more child input.
instance (Monad m,f ~ HtmlT m a) => Term [Attributes] (f -> HtmlT m a) where
  term name = makeElement name

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m) => Term (HtmlT m a) (HtmlT m a) where
  term name = makeElement name mempty
  {-# INLINE term #-}

-- | Some terms (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance Term Text Attributes where
  term key value = makeAttributes key value

-- | Same as the 'Term' class, but will not HTML escape its
-- children. Useful for elements like 'Lucid.Html5.style_' or
-- 'Lucid.Html5.script_'.
class TermRaw arg result | result -> arg where
  -- | Used for constructing elements e.g. @termRaw "p"@ yields 'Lucid.Html5.p_'.
  termRaw :: Text   -- ^ Name of the element or attribute.
       -> arg    -- ^ Either an attribute list or children.
       -> result -- ^ Result: either an element or an attribute.

-- | Given attributes, expect more child input.
instance (Monad m,ToHtml f, a ~ ()) => TermRaw [Attributes] (f -> HtmlT m a) where
  termRaw name attrs = makeElement name attrs . toHtmlRaw

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m,a ~ ()) => TermRaw Text (HtmlT m a) where
  termRaw name = makeElement name mempty . toHtmlRaw

-- | Some termRaws (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance TermRaw Text Attributes where
  termRaw key value = makeAttributesRaw key value

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
  do (builder,_) <- runHtmlT m
     return builder
{-# inline execHtmlT #-}

-- | Generalize the underlying monad.
--
-- Some builders are happy to deliver results in a pure underlying
-- monad, here 'Identity', but have trouble maintaining the polymorphic
-- type. This utility generalizes from 'Identity'.
--
generalizeHtmlT ::
     Monad m
  => HtmlT Identity a -- ^ The HTML generated purely.
  -> HtmlT m a -- ^ Same HTML accessible in a polymorphic context.
generalizeHtmlT = hoistHtmlT go
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
commuteHtmlT2 :: (Monad m, Monad n)
             => HtmlT m a      -- ^ unpurely generated HTML
             -> m (HtmlT n a)  -- ^ Commuted monads. /Note:/ @n@ can be 'Identity'
commuteHtmlT2 h = do
  (builder, a) <- runHtmlT h
  return . HtmlT $ modify' (<> builder) >> return a

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

-- | Make a set of attributes.
makeAttributes :: Text -- ^ Attribute name.
               -> Text -- ^ Attribute value.
               -> Attributes
makeAttributes x y = Attributes (pure (Attribute x (Blaze.fromHtmlEscapedText y)))

-- | Make a set of unescaped attributes.
makeAttributesRaw ::
     Text -- ^ Attribute name.
  -> Text -- ^ Attribute value.
  -> Attributes
makeAttributesRaw x y = Attributes (pure (Attribute x (Blaze.fromText y)))

-- | Make an HTML builder.
makeElement :: Monad m
            => Text       -- ^ Name.
            -> [Attributes]
            -> HtmlT m a  -- ^ Children HTML.
            -> HtmlT m a -- ^ A parent element.
{-# INLINE[1] makeElement #-}
makeElement name attr children = do
  write
    (s "<" <> Blaze.fromText name <> foldlMapWithKey buildAttr (attributeList attr) <> s ">")
  v <- children
  write (s "</" <> Blaze.fromText name <> s ">")
  pure v

-- | Make an HTML builder for elements which have no ending tag.
makeElementNoEnd :: Monad m
                 => Text       -- ^ Name.
                 -> [Attributes]
                 -> HtmlT m () -- ^ A parent element.
makeElementNoEnd name attr =
  write
    (s "<" <> Blaze.fromText name <> foldlMapWithKey buildAttr (attributeList attr) <> s ">")

-- | Build and encode an attribute.
buildAttr :: Text -> Builder -> Builder
buildAttr key val = s " " <> Blaze.fromText key <> s "=\"" <> val <> s "\""

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: (Text -> Builder -> Builder) -> Seq Attribute -> Builder
foldlMapWithKey f attributes =
  case nubOrdMaybe (map fst pairs) of
    Just keyList ->
      foldMap (\k -> fromMaybe mempty (fmap (f k) (M.lookup k values))) keyList
      where values = M.fromListWithKey combineAttributes pairs
    Nothing -> foldMap (\(Attribute k v) -> f k v) attributes
  where
    pairs = map (\(Attribute k v) -> (k,v)) (toList attributes)

-- | Special handling for class and style, which are common things
-- people want to combine.
combineAttributes :: Text -> Builder -> Builder -> Builder
combineAttributes "class" v2 v1 = v1 <> " " <> v2
combineAttributes "style" v2 v1 = v1 <> ";" <> v2
combineAttributes _       v2 v1 = v1 <> v2

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

-- | Write some HTML output.
write :: Monad m => Builder -> HtmlT m ()
write b = HtmlT (modify' (<> b))
{-# inline write #-}

attributeList :: [Attributes] -> Seq Attribute
attributeList = foldMap unAttributes

--------------------------------------------------------------------------------
-- Deprecated definitions

relaxHtmlT :: Monad m => HtmlT Identity a -> HtmlT m a
relaxHtmlT = undefined
{-# DEPRECATED relaxHtmlT "DO NOT USE. This was exported accidentally and throws an exception." #-}

commuteHtmlT :: (Monad m, Monad n)
             => HtmlT m a      -- ^ unpurely generated HTML
             -> m (HtmlT n a)  -- ^ Commuted monads. /Note:/ @n@ can be 'Identity'
commuteHtmlT h = do
  (builder, a) <- runHtmlT h
  return . HtmlT $ put builder >> return a
{-# DEPRECATED commuteHtmlT "This has incorrect behavior and will lose HTML output. See commuteHtmlT2." #-}
