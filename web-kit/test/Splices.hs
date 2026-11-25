{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data Shape = Circle | Square

-- A declaration splice gets the constructors of Shape with reify.
$( do
     info <- reify ''Shape
     let names = case info of
           TyConI (DataD _ _ _ _ constructors _) -> [nameBase name | NormalC name _ <- constructors]
           _ -> []
     pure [ValD (VarP (mkName "shapeNames")) (NormalB (ListE (map (LitE . StringL) names))) []]
 )

main :: IO ()
main = do
  print $(litE (integerL (6 * 7)))
  print shapeNames
