{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Array.Byte (ByteArray)
import qualified Data.Kind as Kind
import qualified GHC.Exts as Exts
import Data.Proxy (Proxy (..))
import Helper (Describe (..), Size (..), deriveDescribe, deriveLabel, isMode, labelText, pathVariable, plusHelper, twiceHelper, typedOffset, typedSteps, warmType)
import Language.Haskell.TH
import Lower (Mode (..), lowerFile)
import Quasi (quasiLines)
import Language.Haskell.TH.Syntax (Lift (..), Name (..), NameFlavour (..), mkNameG_tc, mkNameG_v, pkgString)

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

-- | A case quote with variables that are not in scope. GHC accepts them,
-- and the syntax tree has unbound variables.
holeCase :: Q Exp
holeCase = [|case _ of { "a" -> missing; _ -> 'c' }|]

-- | A quote with a constructor that is not in scope. GHC accepts it, and
-- the syntax tree has an unbound variable.
holeConstructor :: Q Exp
holeConstructor = [|Missing|]

-- | A type operator of this module.
type a :+ b = Either a b

infixr 5 :+

-- | A type quote with a type operator. GHC gives an application of the
-- operator.
operatorQuote :: Q Type
operatorQuote = [t|Int :+ Bool :+ Char|]

-- | Whether the tree of 'operatorQuote' has the expected form.
operatorQuoteForm :: Type -> Bool
operatorQuoteForm ty =
  case ty of
    AppT (AppT (ConT plus) (ConT int)) (AppT (AppT (ConT plus') (ConT bool)) (ConT char)) ->
      plus == ''(:+) && plus' == ''(:+) && int == ''Int && bool == ''Bool && char == ''Char
    _ -> False

-- | A type quote with a forall, a context, and a type variable that the
-- forall binds. The names of the tree are the names of the program.
forallQuote :: Q Type
forallQuote = [t|forall a. Show a => Maybe a -> [a]|]

-- | A type quote with a kind signature. The program does not run it, but
-- the module must compile.
kindQuote :: Q Type
kindQuote = [t|Maybe (Int :: Kind.Type)|]

-- | A type quote with a data constructor with no tick. With DataKinds, the
-- constructor is promoted.
promotedQuote :: Q Type
promotedQuote = [t|Proxy True|]

-- | Whether the tree of 'promotedQuote' has the expected form.
promotedQuoteForm :: Type -> Bool
promotedQuoteForm ty =
  case ty of
    AppT (ConT proxy) (PromotedT true) -> proxy == ''Proxy && true == 'True
    _ -> False

-- | A type quote with type splices. The tree has the type of each splice.
spliceQuote :: Q Type
spliceQuote = [t|Either $(conT ''Int) $([t|Bool|])|]

-- | Whether the tree of 'spliceQuote' has the expected form.
spliceQuoteForm :: Type -> Bool
spliceQuoteForm ty =
  case ty of
    AppT (AppT (ConT either') (ConT int)) (ConT bool) -> either' == ''Either && int == ''Int && bool == ''Bool
    _ -> False

-- | A type splice gives a promoted list.
promotedList :: Proxy $(pure (AppT (AppT PromotedConsT (ConT ''Int)) PromotedNilT))
promotedList = Proxy

-- | A type splice gives a promoted constructor of a module that this module
-- does not import.
warmProxy :: $(warmType)
warmProxy = Proxy

-- | Whether the tree of 'forallQuote' has the expected form.
forallQuoteForm :: Type -> Bool
forallQuoteForm ty =
  case ty of
    ForallT [PlainTV binder SpecifiedSpec] [AppT (ConT showClass) (VarT inContext)] (AppT (AppT ArrowT (AppT (ConT maybeType) (VarT argument))) (AppT ListT (VarT element))) ->
      showClass == ''Show && maybeType == ''Maybe && all (== binder) [inContext, argument, element]
    _ -> False

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

-- | A value of this module that the module does not export.
secret :: Int
secret = 41

-- | A quote in a splice names the argument, which has the name of a
-- top-level value. The name keeps the meaning that it has at the splice.
localSecret :: Int -> Int
localSecret secret = $([|secret|])

-- | The same for an argument with the name of an imported function.
localId :: Int -> Int
localId id = $([|id|])

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

-- A function of a different module of the program derives instances for
-- a type of this module and for a type of a different module.
-- | A data type with a phantom parameter and a representational one.
data Tagged tag value = Tagged value

-- | A constructor with a strict field, a lazy field, and an unpacked field.
data Fields = Fields !(Maybe Int) Bool {-# UNPACK #-} !Point

data Point = Point !Int !Int

-- | An infix constructor with a fixity declaration.
data Joined = Int :+: Int

infixr 6 :+:

$(deriveDescribe ''Shape)

$(deriveDescribe ''Mode)

-- A splice makes an instance of a class of a module of the program that
-- this module does not import.
$(deriveLabel ''Shape)

$( do
     signature <- [t|Int -> (Int, String)|]
     pure
       [ SigD (mkName "pairWithText") signature,
         FunD (mkName "pairWithText") [Clause [VarP (mkName "n")] (NormalB (TupE [Just (VarE (mkName "n")), Just (AppE (VarE 'show) (VarE (mkName "n")))])) []]
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
  -- A local binder around the splice binds the name of the quote, as in
  -- GHC.
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
  runQ holeCase >>= print
  runQ holeConstructor >>= print
  -- A splice uses a function of a different module of the program.
  print $(plusHelper 1)
  putStrLn (describe (Circle 1))
  putStrLn (describe Fast)
  -- A name from reify and a name quote of the same type are equal, in a
  -- splice of this module and in a function of a different module.
  print $(do TyConI (DataD _ name _ _ _ _) <- reify ''Shape; if name == ''Shape then [|True|] else [|False|])
  print $(isMode ''Mode)
  -- A splice reads the roles of a type of this module and of an
  -- imported type.
  putStrLn $(do local <- reifyRoles ''Tagged; imported <- reifyRoles ''Maybe; litE (stringL (show (local, imported))))
  -- A splice reads the decided strictness of the fields of a constructor
  -- of this module and of an imported constructor.
  putStrLn $(do local <- reifyConStrictness 'Fields; imported <- reifyConStrictness 'Just; litE (stringL (show (local, imported))))
  -- A splice reads the fixity of a constructor of this module, of an
  -- imported operator, and of a name with no fixity declaration.
  putStrLn $(do local <- reifyFixity '(:+:); imported <- reifyFixity '(++); none <- reifyFixity 'id; litE (stringL (show (local, imported, none))))
  -- A splice reads the facts of the unit type, a tuple type, and the list
  -- type. The name quote of a tuple type is the name that the library gives.
  putStrLn $(do TyConI (DataD _ _ unit _ _ _) <- reify ''(); TyConI (DataD _ _ pair _ _ _) <- reify ''(,); TyConI (DataD _ _ _ _ list _) <- reify ''[]; litE (stringL (show (length unit, length pair, length list, ''(,) == tupleTypeName 2))))
  -- A type quote in a splice gives the type of a signature.
  print (pairWithText 4)
  runQ forallQuote >>= print . forallQuoteForm
  runQ promotedQuote >>= print . promotedQuoteForm
  runQ operatorQuote >>= print . operatorQuoteForm
  runQ spliceQuote >>= print . spliceQuoteForm
  -- An expression quote with a type signature. The signature has a type
  -- splice.
  print $([|minBound :: $(conT ''Int)|])
  print promotedList
  print warmProxy
  putStrLn (labelText (Square 1 2))
  -- A quote in a splice body names a value and a constructor of this
  -- module that the module does not export, and a constructor in a
  -- pattern.
  print ($([|secret + 1|]) :: Int)
  print $([|Circle 7|])
  print ($([|\(Circle radius) -> radius|]) (Circle 9))
  -- A variable that the splice body binds stays local in the quote: the
  -- quote lifts its value, and the top-level value with the same name is
  -- not used.
  print ($(let secret = 1 :: Int in [|secret|]) :: Int)
  -- A quote with a tuple pattern.
  print ($([|\(left, right) -> left + right|]) (2, 3 :: Int))
  print (localSecret 1)
  print (localId 2)
  -- A splice lifts a syntax tree of Template Haskell. The program gets
  -- the same tree.
  print $(lift (LamE [VarP (mkName "z")] (TupE [Just (AppE (VarE (mkName "f")) (LitE (StringL "s"))), Nothing])))
  -- The reify of a class gives the instances that the module can see.
  putStrLn
    $( do
         ClassI _ instances <- reify ''Lift
         let heads = [name | InstanceD _ _ (AppT _ (ConT name)) _ <- instances]
         litE (stringL (show (''Exp `elem` heads, ''Bool `elem` heads)))
     )
  -- A splice reads its source location. The start is the position of the
  -- dollar sign. The directory of the file depends on the build, so the
  -- program only shows the file name.
  putStrLn $(do loc <- location; litE (stringL (unwords [reverse (takeWhile (/= '/') (reverse (loc_filename loc))), loc_module loc, show (loc_start loc), show (loc_end loc)])))
  -- A quote of a different module names a function that its module does
  -- not export.
  print ($twiceHelper (21 :: Int))
  -- A quote with a record construction and a record update.
  let size = $([|(Size {width = 1, height = 2}) {height = 5}|])
  print (width size, height size)
  print ($(litE (stringL lowerFile)) == lowerFile)
  -- A splice gets the facts of a name from mkName in the scope of the
  -- splice: a type, a function, and a data constructor of the module.
  print $(do TyConI (DataD _ _ _ _ constructors _) <- reify (mkName "Shape"); VarI _ _ _ <- reify (mkName "answer"); DataConI _ _ parent <- reify (mkName "Circle"); tupE [litE (integerL (fromIntegral (length constructors))), if parent == ''Shape then [|True|] else [|False|]])
  -- A splice lifts a byte array, as the derived instance of a type with a
  -- byte array field does. The program gets the same bytes.
  print (Exts.toList ($(lift (Exts.fromList [1, 2, 255] :: ByteArray)) :: ByteArray))
  -- A splice lifts constructors of base types. The names of the
  -- constructors are the names in these libraries.
  print ($(lift (Nothing :: Maybe Int, Just 'j', Left 3 :: Either Int Bool)) :: (Maybe Int, Maybe Char, Either Int Bool))
  mapM_ putStrLn quasiLines
  -- Typed splices of typed quotes.
  print ($$(typedSteps 3) 5, $$(typedOffset 41))
  putStrLn $pathVariable
