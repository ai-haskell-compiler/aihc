{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Name (..), NameFlavour (..), mkNameG_tc, mkNameG_v, pkgString)

-- A quote in a module that also has a do block in an operand of an infix
-- expression. Name resolution runs two times on such a module, and the
-- second time must not see the annotations of the first time.
triple :: Q Exp
triple = [|\x -> (x, 'c', "s")|]

withUnit :: (() -> IO a) -> IO a
withUnit action = action ()

-- The program builds the syntax tree of the quote when it runs.
showTriple :: IO ()
showTriple =
  withUnit $ \_ -> do
    tree <- runQ triple
    print tree

-- A quote in the body of a splice of a quote uses a variable that the
-- outer quote binds.
nested :: Q Exp
nested = [|\y -> $([|y|])|]

-- A declaration splice gives a signature and a function. The function has
-- a local declaration.
$( pure
     [ SigD (mkName "twice") (AppT (AppT ArrowT (ConT (mkName "Int"))) (ConT (mkName "Int"))),
       FunD
         (mkName "twice")
         [ Clause
             [VarP (mkName "x")]
             (NormalB (InfixE (Just (VarE (mkName "x"))) (VarE (mkName "+")) (Just (VarE (mkName "same")))))
             [ValD (VarP (mkName "same")) (NormalB (VarE (mkName "x"))) []]
         ]
     ]
 )

-- A declaration splice with name quotes. Each name keeps its package and
-- its module.
$( pure
     [ SigD (mkName "minus") (AppT (AppT ArrowT (ConT ''Int)) (ConT ''Int)),
       ValD (VarP (mkName "minus")) (NormalB (VarE 'negate)) []
     ]
 )

-- A declaration splice without the splice syntax, with no declarations.
pure []

-- A type splice gives the type of a signature. The module does not type
-- check when the splice does not run.
answer :: $(conT ''Int)
answer = 42

-- A type of this module, above a declaration splice.
data Shape = Circle Int | Square Int Int
  deriving (Show)

-- A declaration splice reads the facts of a type of its own module with
-- reify, and makes declarations that use the type and its constructors.
$( do
     TyConI (DataD _ _ _ _ constructors _) <- reify ''Shape
     let names = [nameBase name | NormalC name _ <- constructors]
     pure
       [ SigD (mkName "shapeNames") (AppT ListT (ConT ''String)),
         ValD (VarP (mkName "shapeNames")) (NormalB (ListE (map (LitE . StringL) names))) [],
         SigD (mkName "firstShape") (ConT ''Shape),
         ValD (VarP (mkName "firstShape")) (NormalB (AppE (ConE 'Circle) (LitE (IntegerL 3)))) []
       ]
 )

-- A type of this module, with an instance that a splice makes.
data Box a = Box a | Pair a a
  deriving (Eq, Show)

-- A declaration splice makes an instance of a class that the program does
-- not import, and a function of the module of the class. The splice names
-- them with their package and their module, as a derived instance of a
-- library does.
$( do
     let classes make occurrence =
           case 'print of
             Name _ (NameG _ package _) -> make (pkgString package) "Data.Functor.Classes" occurrence
             _ -> error "The name print has no package."
     f <- newName "f"
     x <- newName "x"
     y <- newName "y"
     a <- newName "a"
     b <- newName "b"
     let call function first second = AppE (AppE (VarE function) (VarE first)) (VarE second)
         box = AppT (ConT ''Box) (ConT ''Int)
     pure
       [ InstanceD
           Nothing
           []
           (AppT (ConT (classes mkNameG_tc "Eq1")) (ConT ''Box))
           [ FunD
               (classes mkNameG_v "liftEq")
               [ Clause [VarP f, ConP 'Box [] [VarP x], ConP 'Box [] [VarP y]] (NormalB (call f x y)) [],
                 Clause
                   [VarP f, ConP 'Pair [] [VarP x, VarP a], ConP 'Pair [] [VarP y, VarP b]]
                   (GuardedB [(NormalG (call f x y), call f a b), (NormalG (VarE 'otherwise), ConE 'False)])
                   [],
                 Clause [WildP, WildP, WildP] (NormalB (ConE 'False)) []
               ]
           ],
         SigD (mkName "sameBox") (AppT (AppT ArrowT box) (AppT (AppT ArrowT box) (ConT ''Bool))),
         ValD (VarP (mkName "sameBox")) (NormalB (VarE (classes mkNameG_v "eq1"))) []
       ]
 )

main :: IO ()
main = do
  -- Splices that build the syntax tree with functions.
  print ($(litE (integerL 42)) :: Int)
  print ($(appE (varE (mkName "negate")) (litE (integerL 7))) + 1 :: Int)
  putStrLn $(litE (stringL (concat (replicate 3 "ab"))))
  -- Splices that build the syntax tree with quotes.
  print ($([|length "abc" + 1|]) :: Int)
  -- The quote lifts the value of a variable from outside the quote.
  print ($(let n = 5 :: Int in [|n * 2|]) :: Int)
  print ($([|\x -> x + 1|]) (41 :: Int))
  -- The name in the quote keeps its meaning where a local name hides it.
  let length = const (0 :: Int) in print ($([|length "abcd"|]) :: Int)
  print $([|(if True then 'a' else 'b', [length "x", 2])|])
  print (twice 21)
  print (minus 5)
  -- The program makes the names of name quotes when it runs.
  putStrLn (nameBase 'negate ++ " " ++ nameBase ''Maybe)
  -- A splice makes a unique name, and a splice recovers from a failure.
  print ($(do x <- newName "x"; lamE [varP x] (varE x)) (7 :: Int))
  print ($(recover (litE (integerL 1)) (fail "no")) :: Int)
  -- A splice reads the facts of imported names with reify: a data type, a
  -- data constructor, and a type synonym. The fields of the constructors
  -- use the type variables of the data type.
  putStrLn
    $( do
         TyConI (DataD _ _ binders _ constructors _) <- reify ''Either
         DataConI _ _ parent <- reify 'Just
         TyConI (TySynD _ [] (AppT ListT (ConT element))) <- reify ''String
         let describe constructor =
               case constructor of
                 NormalC name fields -> nameBase name ++ "/" ++ show (length fields)
                 _ -> "?"
             variables = [name | KindedTV name _ _ <- binders]
             fieldVariables = [name | NormalC _ [(_, VarT name)] <- constructors]
         litE (stringL (unwords ([show (length binders)] ++ map describe constructors ++ [show (fieldVariables == variables), nameBase parent, nameBase element])))
     )
  -- The declarations of this module above it, and the results of the
  -- splice above, are in the declaration group before this splice.
  print shapeNames
  print firstShape
  print ($(do TyConI (DataD _ _ _ _ constructors _) <- reify ''Shape; litE (integerL (fromIntegral (length constructors)))) :: Int)
  print (sameBox (Pair 1 2) (Pair 1 2), sameBox (Box 1) (Pair 1 1), sameBox (Pair 1 2) (Pair 1 3))
  showTriple
  runQ nested >>= print
