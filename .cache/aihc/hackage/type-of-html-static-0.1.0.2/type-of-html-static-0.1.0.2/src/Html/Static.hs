{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Html.Static
  ( static
  , optimize
  ) where

import Language.Haskell.TH
import Data.Proxy
import Html

-- | Template haskell function to annote which parts of a html document are static.
--
-- >>> :t $(static (div_ "<>"))
-- Proxy "<div>&lt;&gt;</div>"
--
-- Note that you can annote any part of the document which is itself a
-- valid document and doesn't contain variables which are defined in
-- the same file.
--
-- >>> :t \x -> div_ ($(static (div_ "<>")) # x)
-- \x -> 'Div > (Proxy "<div>&lt;&gt;</div>" # x)
static :: Document a => a -> Q Exp
static x = pure (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (LitT (StrTyLit (renderString x)))))

-- | Try to optimize a quoted html document at compiletime.
--
-- >>> :t \x -> $(optimize [| div_ "a" # x |])
-- \x -> 'Div > Proxy "a" # x
--
-- This function recurses into an Q Exp and turns every literal
-- expression which is applied to an element or to an attribute into a
-- Proxy Symbol.  It stops recursing a branch if any other expression
-- than an element, attribute or (#) is found to avoid non
-- typechecking code, like an if statement where only one branch is
-- converted into a Proxy Symbol or an function, which wants a 'Div >
-- String as argument, receiving a Proxy Symbol.
--
-- If you don't mind changing type signatures, this function is safe
-- to apply to any document and generally increases performance
-- substantially.
--
-- Note that it's different to `static`: it doesn't evaluate any
-- function but it can be applied to documents with variables.  It is
-- less powerfull but more convenient because you can simply apply it
-- to your entire html document. `static` you'd have to apply only to
-- parts which can be calculated statically. Be aware, that it stops
-- as well on known variables, so you have to apply it to the
-- definition of your page and not to the variable to which your
-- definition is bound.
optimize :: Q Exp -> Q Exp
optimize = fmap f

  where
    f (AppE (VarE a) b) | nameModule a == Just "Html.Element" || nameModule a == Just "Html.Attribute" = AppE (VarE a) (f b)
    f (AppE (AppE (VarE a) b) c) | nameModule a == Just "Html.Element" || nameModule a == Just "Html.Type.Internal" && nameBase a == "#" = AppE (AppE (VarE a) (f b)) (f c)
    f (InfixE (Just a) (VarE b) (Just c)) | nameModule b == Just "Html.Type.Internal" && nameBase b == "#"
      = InfixE (Just (f a)) (VarE b) (Just (f c))
    f (LitE (CharL x)) = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (LitT (StrTyLit (renderString x))))
    f (LitE (StringL x)) = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (LitT (StrTyLit (renderString x))))
    f (LitE (IntegerL x)) = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (LitT (StrTyLit (renderString x))))
    f x = x
