-- | Interaction between stuck equalities.
--
-- A wanted equality with a saturated type family application on one side
-- waits until the application reduces. Two such wanteds can still say
-- something together: when both mention the same application, their other
-- sides are equal. So @F t0 ~ Maybe t0@ and @F t0 ~ Maybe (Count Nat)@
-- equate @Maybe t0@ with @Maybe (Count Nat)@, which solves @t0@. The
-- application then reduces and both wanteds solve on their own.
--
-- Like injectivity improvement this produces equalities, never evidence:
-- each wanted is still solved afterwards, by reduction or by a given.
module Aihc.Tc.Solve.Interact
  ( improveSharedFamilyApplications,
  )
where

import Aihc.Tc.Monad (TcM)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, reduceTypeFamilies, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (void)
import Data.List (tails)

-- | Equate the other sides of wanted equalities that share a saturated
-- type family application.
--
-- The result says whether the interaction solved a meta variable, which
-- means the wanteds are worth another attempt.
improveSharedFamilyApplications :: [Pred] -> TcM Bool
improveSharedFamilyApplications wanteds = do
  before <- mapM zonkPred wanteds
  sides <- concat <$> mapM familySides wanteds
  sequence_
    [ improveOne leftOther rightOther
    | (application, leftOther) : rest <- tails sides,
      (otherApplication, rightOther) <- rest,
      sameType application otherApplication
    ]
  after <- mapM zonkPred wanteds
  pure (before /= after)
  where
    improveOne left right
      | sameType left right = pure ()
      | otherwise = void (unifyTypes left right)

-- | The saturated family applications that a wanted equates with another
-- type, each with that other type. A family application is often a field
-- of the type the wanted names, so the outer structure decomposes first.
familySides :: Pred -> TcM [(TcType, TcType)]
familySides predicate =
  case predicate of
    EqPred rawLeft rawRight -> do
      left <- normalize rawLeft
      right <- normalize rawRight
      sidesOf left right
    _ -> pure []
  where
    sidesOf left right = do
      leftIsFamily <- isTypeFamilyApplication left
      rightIsFamily <- isTypeFamilyApplication right
      let here = [(left, right) | leftIsFamily] <> [(right, left) | rightIsFamily]
      children <-
        if leftIsFamily || rightIsFamily
          then pure []
          else concat <$> decomposeNominalEquality left right
      deeper <- concat <$> mapM (uncurry sidesOf) children
      pure (here <> deeper)

normalize :: TcType -> TcM TcType
normalize ty = zonkType ty >>= reduceTypeFamilies >>= unsaturateFamilyApplication
