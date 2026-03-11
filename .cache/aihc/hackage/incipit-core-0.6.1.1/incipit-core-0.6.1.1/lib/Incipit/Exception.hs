-- |Combinators that catch exceptions in 'IO' and embed them into 'Sem'.
module Incipit.Exception where

import qualified Control.Exception as Base
import Control.Exception (Exception, SomeException)
import Data.Bifunctor (first)
import Data.Either (Either)
import Data.Function ((.))
import Data.Functor (fmap, void)
import Data.Maybe (Maybe)
import Polysemy (Embed, Member, Sem, embed)
import System.IO (IO)
import System.IO.Error (IOError)

import Incipit.Either (rightToMaybe)
import Incipit.String.Conversion (show)
import Incipit.String.Reexport (Text)

-- |Run an 'IO' via 'Embed' and catch exceptions of type @e@, returning 'Either'.
-- Unlike all other combinators, this doesn't convert the exception to 'Text'.
tryIOE ::
  ∀ e r a .
  Exception e =>
  Member (Embed IO) r =>
  IO a ->
  Sem r (Either e a)
tryIOE =
  embed @IO . Base.try @e
{-# inline tryIOE #-}

-- |Run an 'IO' via 'Embed' and catch exceptions of type @e@, returning 'Either'.
tryIO ::
  ∀ e r a .
  Exception e =>
  Member (Embed IO) r =>
  IO a ->
  Sem r (Either Text a)
tryIO =
  embed @IO . fmap (first show) . Base.try @e
{-# inline tryIO #-}

-- |Run an 'IO' via 'Embed' and catch IOError, returning 'Either'.
tryIOError ::
  ∀ r a .
  Member (Embed IO) r =>
  IO a ->
  Sem r (Either Text a)
tryIOError =
  tryIO @IOError
{-# inline tryIOError #-}

-- |Run an 'IO' via 'Embed' and catch all exceptions, returning 'Either'.
tryAny ::
  ∀ r a .
  Member (Embed IO) r =>
  IO a ->
  Sem r (Either Text a)
tryAny =
  tryIO @SomeException
{-# inline tryAny #-}

-- |Run an 'IO' via 'Embed' and catch exceptions of type @e@, returning 'Maybe'.
tryIOMaybe ::
  ∀ e r a .
  Exception e =>
  Member (Embed IO) r =>
  IO a ->
  Sem r (Maybe a)
tryIOMaybe =
  fmap rightToMaybe . tryIO @e
{-# inline tryIOMaybe #-}

-- |Run an 'IO' via 'Embed' and catch 'IOError', returning 'Maybe'.
tryIOErrorMaybe ::
  ∀ r a .
  Member (Embed IO) r =>
  IO a ->
  Sem r (Maybe a)
tryIOErrorMaybe =
  tryIOMaybe @IOError
{-# inline tryIOErrorMaybe #-}

-- |Run an 'IO' via 'Embed' and catch all exceptions, returning 'Maybe'.
tryMaybe ::
  ∀ r a .
  Member (Embed IO) r =>
  IO a ->
  Sem r (Maybe a)
tryMaybe =
  tryIOMaybe @SomeException
{-# inline tryMaybe #-}

-- |Run an 'IO' via 'Embed' and catch and ignore exceptions of type @e@.
tryIO_ ::
  ∀ e r .
  Exception e =>
  Member (Embed IO) r =>
  IO () ->
  Sem r ()
tryIO_ =
  void . tryIO @e
{-# inline tryIO_ #-}

-- |Run an 'IO' via 'Embed' and catch and ignore 'IOError'.
tryIOError_ ::
  ∀ r .
  Member (Embed IO) r =>
  IO () ->
  Sem r ()
tryIOError_ =
  void . tryIO @IOError
{-# inline tryIOError_ #-}

-- |Run an 'IO' via 'Embed' and catch and ignore all exceptions.
tryAny_ ::
  ∀ r .
  Member (Embed IO) r =>
  IO () ->
  Sem r ()
tryAny_ =
  void . tryIO @SomeException
{-# inline tryAny_ #-}

-- |Run an 'IO' via 'Embed' and catch and ignore all exceptions.
ignoreException ::
  Member (Embed IO) r =>
  IO () ->
  Sem r ()
ignoreException =
  tryAny_
{-# inline ignoreException #-}
{-# deprecated ignoreException "renamed to 'tryAny_'" #-}
