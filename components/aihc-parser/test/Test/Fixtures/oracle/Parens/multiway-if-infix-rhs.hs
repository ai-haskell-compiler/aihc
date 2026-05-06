{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
module M where

actorVulnerable condAnyHarmfulFoeAdj condCanMelee condManyThreatAdj condSupport1 condSolo condInMelee heavilyDistressed fleeingMakesSense =
  return $!
    fleeingMakesSense
      && if | condAnyHarmfulFoeAdj ->
              not condCanMelee || condManyThreatAdj && not condSupport1 && not condSolo
            | condInMelee -> False
            | heavilyDistressed -> True
            | otherwise -> False
      && condCanFlee
