{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- The -ddump-splices writes the sayableSubConstraints output to stdout during
-- compilation:
{-- OPTIONS_GHC -ddump-splices #-}

import           Data.Maybe ( catMaybes )
import           Data.Text ( Text )
import qualified Language.Haskell.TH as TH
import qualified Prettyprinter as PP
import           Text.Sayable

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Runners.AntXML

import Helpers


data Bar = Bar
data Baz = Baz Bar
data Foo = Foo { fld1 :: Bar
               , fld2 :: [Baz]
               , fld3 :: Maybe Bar
               }

data HiddenValue = HiddenValue Char
data Foo2 = FC1 Bar [Maybe Baz] | FC2 Bar Int HiddenValue

data Bar3 a = Bar3 (Maybe a)
data Baz3 a b = BazL a | BazR b

data Foo3 a = Foo3 { inputs :: Bar3 a, outputs :: Baz3 a String }


$(return [])


instance Sayable "loud" Bar where sayable _ = t'"BAR" &+ '!'
instance {-# OVERLAPPABLE #-} Sayable stag Bar where sayable _ = t'"bar" &+ '.'

-- This instance has no constraints, so the "loud" will not invoke the proper
-- Sayable for Bar.
instance Sayable stag Baz where sayable (Baz bar) = t'"baz" &- bar

-- This instance has the necessary constraints (via our TH helper) so the "loud"
-- Sayable for Bar will be properly invoked.
instance $(sayableSubConstraints $ ofType ''Foo >> tagVar "stag"
          ) => Sayable stag Foo where
  sayable foo = t'"Foo" &- fld1 foo &+ ',' &* fld2 foo &+ ',' &? fld3 foo &- 'E'

-- TH-supplied sub-element constraints for Foo2 with a filter.  Also verify that
-- only the last of the tag specification operations is used.
instance $(sayableSubConstraints $ do ofType ''Foo2
                                      -- tagNat 3
                                      tagSym "three"
                                      subElemFilter foo2Filter
                                      tagVar "t"
          ) => Sayable t Foo2 where
  sayable = \case
    FC1 x y -> t'"First Foo2 form with" &- x &- t'"and" &* catMaybes y
    FC2 x y _ -> t'"Second Foo2 form with" &- x &- y

-- TH-supplied sub-element constraints for Foo3 with parameterized sub-elements.
instance $(sayableSubConstraints $ do ofType ''Foo3
                                      tagVar "t"
                                      paramVar "a"
                                      paramTH (TH.ConT ''String)
          ) => Sayable t (Foo3 a) where
  sayable f3 = t'"Foo3" &- inputs f3 &- t'"-->" &- outputs f3

-- TH-supplied sub-element constraints with explicit override
instance $(sayableSubConstraints $ do ofType ''Bar3
                                      paramVar "a"
                                      tagSym "loud"
         ) => Sayable "loud" (Bar3 a) where
  sayable (Bar3 mba) = t'"BAR3" &? mba
instance {-# OVERLAPPABLE #-}
         $(sayableSubConstraints $ paramVar "a" >> ofType ''Bar3
          ) => Sayable saytag (Bar3 a) where
  sayable (Bar3 mba) = t'"bar3" &? mba

-- TH-supplied sub-element constraints with explicit override
instance $(sayableSubConstraints $ do paramVar "a"
                                      tagSym "loud"
                                      paramTH $ TH.ConT ''String
                                      ofType ''Baz3
         ) => Sayable "loud" (Baz3 a String) where
  sayable = \case
    BazL a -> t'"BAZL" &- a
    BazR b -> t'"BAZR" &- b
instance {-# OVERLAPPABLE #-}
         $(sayableSubConstraints $ do ofType ''Baz3
                                      paramVar "a"
                                      paramTH $ TH.ConT ''String
                                      paramNat 0  -- extra: ignored
                                      paramVar "a"  -- extra: ignored
                                      tagVar "lvl"
          ) => Sayable lvl (Baz3 a String) where
  sayable = \case
    BazL a -> t'"bazl" &- a
    BazR b -> t'"bazr" &- b


----------------------------------------------------------------------

main :: IO ()
main = tests >>= defaultMainWithIngredients (antXMLRunner : defaultIngredients)

tests :: IO TestTree
tests = testGroup "Sayable" <$> sequence
  [
    testSpec "Operators" $
    describe "operator results shown in haddocks" $ do
      it "renders &-" $
        (sez @"info" $ t'"hello" &- t'"world") `shouldBe` "hello world"
      it "renders &+" $
        (sez @"info" $ t'"hello" &+ t'"world") `shouldBe` "helloworld"
      it "renders &%" $
        (sez @"info" $ t'"hello" &% (t'"world", t'"!")) `shouldBe` "hello (world, !)"
      it "renders &!" $
        (sez @"info" $ PP.group &! t'"hi") `shouldBe` "hi"
      it "renders &*" $
        (sez @"info" $ t'"three:" &* [1, 2, 3::Int]) `shouldBe` "three: 1, 2, 3"
      it "renders &+*" $
        (sez @"info" $ t'"three:" &- '(' &+* [1, 2, 3::Int] &+ ')')
        `shouldBe` "three: (1, 2, 3)"
      it "renders &:*" $
        (sez @"info" $ t'"three:" &- t'".." &:* [1, 2, 3::Int])
        `shouldBe` "three: 1..2..3"
      it "renders &!*" $
        (sez @"info" $ t'"three:" &- PP.align . PP.vsep &!* [1, 2, 3::Int])
        `shouldBe` "three: 1, \n       2, \n       3"
      it "renders &!$*" $
        (sez @"info" $ t'"three:" &- PP.align &!$* [1, 2, 3::Int])
         `shouldBe` "three: 1, 2, 3"
      it "renders &!:*" $
        (sez @"info" $ t'"three:" &- (PP.align . PP.vsep &!:* (t'" or")) [1, 2, 3::Int])
        `shouldBe` "three: 1 or\n       2 or\n       3"
      it "renders &?" $
        (sez @"info" $ t'"It's" &? Just (t'"something") &- t'"or" &? (Nothing :: Maybe Text))
        `shouldBe` "It's something or"
      it "renders &!?" $
        (sez @"info" $ PP.group &!? Just (t'"hi")) `shouldBe` "hi"
      it "renders &<" $
        (sez @"info" $ t'"Hello" &< t'"world") `shouldBe` "Hello\nworld"
      it "renders &<*" $
        (sez @"info" $ t'"three:" &<* [1, 2, 3::Int]) `shouldBe` "three:\n1, 2, 3"
      it "renders &<? Just" $
        (sez @"info" $ t'"First" &<? Just (t'"something"))
        `shouldBe` "First\nsomething"
      it "renders &<? Nothing" $
        (sez @"info" $ t'"Then" &<? (Nothing :: Maybe Text)) `shouldBe` "Then"
      it "renders &+?" $
        (sez @"info" $ t'"It's" &+? (Nothing :: Maybe Text) &- t'"ok" &+? Just ("time" :: Text))
        `shouldBe` "It's oktime"


  , testSpec "sub-constraints" $ do
      -- As described in the haddocks for sayableSubConstraints
    describe "sub-constraints working" $ do
      let v = Foo Bar [Baz Bar, Baz Bar] $ Just Bar
      -- Foo does have sub-element constraints, so "loud" is different than
      -- "normal", unlike Baz which doesn't express sub-constraints and therefore
      -- its sub-elements don't properly get the propagated Sayable tag.
      it "normal rendering" $
        (sez @"normal" v) `shouldBe` "Foo bar., baz bar., baz bar., bar. E"
      it "loud rendering" $
        (sez @"loud" v) `shouldBe` "Foo BAR!, baz bar., baz bar., BAR! E"

    describe "sub-constraints constructor filtering" $ do
      let v = FC2 Bar 0 (HiddenValue 'h')
      it "normal rendering" $
        (sez @"normal" v) `shouldBe` "Second Foo2 form with bar. 0"
      it "loud rendering" $
        (sez @"loud" v) `shouldBe` "Second Foo2 form with BAR! 0"

    describe "sub-constraints with varBindings" $ do
      let v = Foo3 (Bar3 (Just (Bar3 (Just (9 :: Int))))) (BazL (Bar3 Nothing))
      it "normal rendering" $
        (sez @"normal" v) `shouldBe` "Foo3 bar3 bar3 9 --> bazl bar3"
      it "loud rendering" $
        (sez @"loud" v) `shouldBe` "Foo3 BAR3 BAR3 9 --> BAZL BAR3"


  ]
