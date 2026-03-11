module Synthesizer.Causal.Arrow where

import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Generic.Signal as SigG
import Control.Arrow (Arrow, )


class Arrow arrow => C arrow where
   apply ::
      (SigG.Transform sig a, SigG.Transform sig b) =>
      arrow a b -> sig a -> sig b

instance C Causal.T where
   apply = Causal.apply

instance C (->) where
   apply = SigG.map
