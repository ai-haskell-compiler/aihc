-- |

module Language.REST where

import Language.REST.OCAlgebra (OCAlgebra)
import Language.REST.OCToAbstract
import Language.REST.WQOConstraints.ADT (ConstraintsADT, adtOC)
import Language.REST.RPO
import Language.REST.RuntimeTerm
import Language.REST.Op
import System.IO (Handle)


-- | 'adtRPO' Is an ordering constraint algebra derived from the recursive
-- path ordering; it is a useful general-purpose OCA.
adtRPO :: (Handle, Handle) -> OCAlgebra (ConstraintsADT Op) RuntimeTerm IO
adtRPO z3 = lift (adtOC z3) rpo
