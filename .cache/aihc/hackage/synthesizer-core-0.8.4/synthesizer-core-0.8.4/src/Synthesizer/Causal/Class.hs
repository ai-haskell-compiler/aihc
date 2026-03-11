{-# LANGUAGE TypeFamilies #-}
module Synthesizer.Causal.Class (
   module Synthesizer.Causal.Class,
   Util.chainControlled,
   Util.replicateControlled,
   ) where

import qualified Synthesizer.Causal.Utility as Util

import qualified Control.Category as Cat
import Control.Arrow (Arrow, arr, (<<<), (&&&), )


type family ProcessOf (signal :: * -> *) :: * -> * -> *

class (Arrow process, ProcessOf (SignalOf process) ~ process) => C process where
   type SignalOf process :: * -> *
   toSignal :: process () a -> SignalOf process a
   fromSignal :: SignalOf process b -> process a b


infixl 0 $<, $>, $*
-- infixr 0 $:*   -- can be used together with $

apply ::
   (C process) => process a b -> SignalOf process a -> SignalOf process b
apply proc sig =
   toSignal (proc <<< fromSignal sig)

applyFst, ($<) ::
   (C process) => process (a,b) c -> SignalOf process a -> process b c
applyFst proc sig =
   proc <<< feedFst sig

applySnd, ($>) ::
   (C process) => process (a,b) c -> SignalOf process b -> process a c
applySnd proc sig =
   proc <<< feedSnd sig

applyConst ::
   (C process) => process a b -> a -> SignalOf process b
applyConst proc a =
   toSignal (proc <<< arr (\() -> a))

applyConstFst ::
   (Arrow process) => process (a,b) c -> a -> process b c
applyConstFst proc a =
   proc <<< feedConstFst a

applyConstSnd ::
   (Arrow process) => process (a,b) c -> b -> process a c
applyConstSnd proc a =
   proc <<< feedConstSnd a


feedFst :: (C process) => SignalOf process a -> process b (a,b)
feedFst sig =
   fromSignal sig &&& Cat.id

feedSnd :: (C process) => SignalOf process a -> process b (b,a)
feedSnd sig =
   Cat.id &&& fromSignal sig

{-# INLINE feedConstFst #-}
feedConstFst :: (Arrow process) => a -> process b (a,b)
feedConstFst a = arr (\b -> (a,b))

{-# INLINE feedConstSnd #-}
feedConstSnd :: (Arrow process) => a -> process b (b,a)
feedConstSnd a = arr (\b -> (b,a))


($*) ::
   (C process) =>
   process a b -> SignalOf process a -> SignalOf process b
($*) = apply
($<) = applyFst
($>) = applySnd
