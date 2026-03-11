module Main (main) where

import Generators
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

import Distribution.SPDX
import Distribution.SPDX.Extra

main :: IO ()
main = quickCheckWith args $
    forAll (scaleGen (`div` 3) exprGen) $ \e' -> case e' of
        NONE      -> True
        License e -> isOsiApprovedExpr e == isOsiApprovedExpr' e
  where
    args = stdArgs
        { replay = Just (mkQCGen 1337, 0)
        , maxSuccess = 500
        }

osiLicenseIds :: [LicenseId]
osiLicenseIds = filter licenseIsOsiApproved [minBound .. maxBound]

osiLicenseExpr :: LicenseExpression
osiLicenseExpr = foldr1 EAnd $ map (\l -> ELicense (ELicenseId l) Nothing) osiLicenseIds

isOsiApprovedExpr :: LicenseExpression -> Bool
isOsiApprovedExpr (ELicense _ (Just _)) = False
isOsiApprovedExpr (ELicense sl Nothing) = simple sl
isOsiApprovedExpr (EAnd e1 e2)          = isOsiApprovedExpr e1 && isOsiApprovedExpr e2
isOsiApprovedExpr (EOr e1 e2)           = isOsiApprovedExpr e1 || isOsiApprovedExpr e2

simple :: SimpleLicenseExpression -> Bool
simple (ELicenseId i)     = licenseIsOsiApproved i
simple (ELicenseIdPlus _) = False -- we don't know ranges
simple (ELicenseRef _)    = False

isOsiApprovedExpr' :: LicenseExpression -> Bool
isOsiApprovedExpr' e = License e `satisfies` License osiLicenseExpr
