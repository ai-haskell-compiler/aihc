-- |
--
module Distribution.SPDX.Extra (
  -- * Types
  -- | We don't export the constructors, as they change with Cabal version.
  License,
  LicenseExpression,
  SimpleLicenseExpression,
  LicenseId,
  LicenseExceptionId,

  -- * Logic
  satisfies,
  equivalent,
  ) where

import Control.Monad.SAT
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class (lift)
import Data.Map.Strict (Map)
import Control.Monad.Trans.State
import Data.Maybe (isNothing)

import Distribution.SPDX
       (License (..), LicenseExceptionId, LicenseExpression (..), LicenseId,
       SimpleLicenseExpression (..))

-- $setup
-- >>> import Distribution.Parsec (simpleParsec)
-- >>> let unsafeParseExpr e = maybe (error $ "invalid: " ++ e) (id :: License -> License) (simpleParsec e)

-- |
--
-- @⟦ satisfies a b ⟧ ≡ a ≥ b ≡ a ∧ b = b @
--
-- >>> unsafeParseExpr "GPL-3.0-only" `satisfies` unsafeParseExpr "ISC AND MIT"
-- False
--
-- >>> unsafeParseExpr "Zlib" `satisfies` unsafeParseExpr "ISC AND MIT AND Zlib"
-- True
--
-- >>> unsafeParseExpr "(MIT OR GPL-2.0-only)" `satisfies` unsafeParseExpr "(ISC AND MIT)"
-- True
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0-only)" `satisfies` unsafeParseExpr "(MIT AND GPL-2.0-only)"
-- True
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0-only)" `satisfies` unsafeParseExpr "(ISC AND GPL-2.0-only)"
-- False
--
satisfies :: License -- ^ package license
          -> License -- ^ license policy
          -> Bool
satisfies a b = isNothing $ runSATMaybe $ flip evalStateT Map.empty $ do
    a' <- exprToProp a >>= lift . addDefinition
    b' <- exprToProp b >>= lift . addDefinition
    lift $ addProp $ neg $ lit b' --> lit a'
    lift solve_

    -- exprToLSLic b `LS.preorder` exprToLSLic a

-- | Check wheather two 'LicenseExpression' are equivalent.
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0-only)" `equivalent` unsafeParseExpr "(GPL-2.0-only AND MIT)"
-- True
--
-- >>> unsafeParseExpr "MIT" `equivalent` unsafeParseExpr "MIT OR BSD-3-Clause"
-- False
--
equivalent :: License -> License -> Bool
equivalent a b = isNothing $ runSATMaybe $ flip evalStateT Map.empty $ do
    a' <- exprToProp a >>= lift . addDefinition
    b' <- exprToProp b >>= lift . addDefinition
    lift $ addProp $ neg $ lit a' <-> lit b'
    lift solve_

----------------------------------f---------------------------------------------
-- internal
-------------------------------------------------------------------------------

data Lic = Lic !SimpleLicenseExpression !(Maybe LicenseExceptionId)
  deriving (Eq, Ord)

exprToProp :: License -> StateT (Map Lic (Lit s)) (SAT s) (Prop s)
exprToProp NONE          = return false
exprToProp (License lic) = licToProp lic

licToProp :: LicenseExpression -> StateT (Map Lic (Lit s)) (SAT s) (Prop s)
licToProp (EAnd a b) = do
    a' <- licToProp a
    b' <- licToProp b
    return (a' /\ b')
licToProp (EOr a b) = do
    a' <- licToProp a
    b' <- licToProp b
    return (a' \/ b')
licToProp (ELicense lic exc) = do
    let k = Lic lic exc
    s <- get
    case Map.lookup k s of
        Just l -> return (lit l)
        Nothing -> do
            l <- lift newLit
            put (Map.insert k l s)
            return (lit l)
