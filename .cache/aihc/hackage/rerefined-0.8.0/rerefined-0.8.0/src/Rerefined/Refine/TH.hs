module Rerefined.Refine.TH
  ( refineTH
  , refine1TH
  ) where

import Rerefined.Refine
import Rerefined.Predicate
import Language.Haskell.TH.Syntax qualified as TH
import Data.Text qualified as Text

-- | Refine @a@ with predicate @p@ at compile time via Template Haskell.
--
-- Use like @$$('refineTH' \@p a)@.
refineTH
    :: forall p a m
    .  (Refine p a, TH.Lift a, TH.Quote m, MonadFail m)
    => a
    -> TH.Code m (Refined p a)
refineTH = either refineTHFail TH.liftTyped . refine @p @a

-- | Refine @f a@ with functor predicate @p@ at compile time via Template
--   Haskell.
--
-- Use like @$$('refine1TH' \@p a)@.
refine1TH
    :: forall p f a m
    .  (Refine1 p f, TH.Lift (f a), TH.Quote m, MonadFail m)
    => f a
    -> TH.Code m (Refined1 p f a)
refine1TH = either refineTHFail TH.liftTyped . refine1 @p @f

-- | Template Haskell refinement failure helper.
refineTHFail
    :: forall a m. MonadFail m => RefineFailure -> TH.Code m a
refineTHFail = TH.liftCode . fail . Text.unpack . prettyRefineFailure
