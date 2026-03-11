{-# LANGUAGE CPP, TemplateHaskell #-}
module Web.Routes.TH
     ( derivePathInfo
     , derivePathInfo'
     , standard
     , mkRoute
     ) where

import Control.Applicative           ((<$>))
import Control.Monad                 (ap, replicateM)
import Data.Char                     (isUpper, toLower, toUpper)
import Data.List                     (intercalate, foldl')
import Data.List.Split               (split, dropInitBlank, keepDelimsL, whenElt)
import Data.Text                     (pack, unpack)
import Data.Typeable                 (typeOf)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax    (nameBase)
import Text.ParserCombinators.Parsec ((<|>),many1)
import Web.Routes.PathInfo

-- | use Template Haskell to create 'PathInfo' instances for a type.
--
-- > $(derivePathInfo ''SiteURL)
--
-- Uses the 'standard' formatter by default.
derivePathInfo :: Name
               -> Q [Dec]
derivePathInfo = derivePathInfo' standard

-- FIXME: handle when called with a type (not data, newtype)

-- | use Template Haskell to create 'PathInfo' instances for a type.
--
-- This variant allows the user to supply a function that transforms
-- the constructor name to a prettier rendering. It is important that
-- the transformation function generates a unique output for each
-- input. For example, simply converting the string to all lower case
-- is not acceptable, because then 'FooBar' and 'Foobar' would be
-- indistinguishable.
--
-- > $(derivePathInfo' standard ''SiteURL)
--
-- see also: 'standard'
derivePathInfo' :: (String -> String)
                -> Name
                -> Q [Dec]
derivePathInfo' formatter name
    = do c <- parseInfo name
         case c of
           Tagged cons cx keys ->
               do let context = pure $  [ AppT (ConT ''PathInfo) (VarT key) | key <- keys ] ++ cx
                  i <- instanceD context (mkType ''PathInfo [mkType name (map varT keys)])
                       [ toPathSegmentsFn cons
                       , fromPathSegmentsFn cons
                       ]
                  return [i]
    where
      toPathSegmentsFn :: [(Name, Int)] -> DecQ
      toPathSegmentsFn cons
          = do inp <- newName "inp"
               let body = caseE (varE inp) $
                            [ do args <- replicateM nArgs (newName "arg")
                                 let matchCon = conP conName (map varP args)
                                     conStr = formatter (nameBase conName)
                                 match matchCon (normalB (toURLWork conStr args)) []
                                  |  (conName, nArgs) <- cons ]
                   toURLWork :: String -> [Name] -> ExpQ
                   toURLWork conStr args
                       = foldr1 (\a b -> appE (appE [| (++) |] a) b) ([| [pack conStr] |] : [ [| toPathSegments $(varE arg) |] | arg <- args ])
               funD 'toPathSegments [clause [varP inp] (normalB body)  []]
      fromPathSegmentsFn :: [(Name,Int)] -> DecQ
      fromPathSegmentsFn cons
          = do let body = (foldl1 (\a b -> appE (appE [| (<|>) |] a) b)
                            [ parseCon conName nArgs
                            | (conName, nArgs) <- cons])
                   parseCon :: Name -> Int -> ExpQ
                   parseCon conName nArgs = foldl1 (\a b -> appE (appE [| ap |] a) b)
                                                   ([| segment (pack $(stringE (formatter $ nameBase conName))) >> return $(conE conName) |]
                                                   : (replicate nArgs [| fromPathSegments |]))
               funD 'fromPathSegments [clause [] (normalB body) []]

mkType :: Name -> [TypeQ] -> TypeQ
mkType con = foldl appT (conT con)

data Class = Tagged [(Name, Int)] Cxt [Name]

parseInfo :: Name -> Q Class
parseInfo name
    = do info <- reify name
         case info of
           TyConI (DataD cx _ keys _ cs _)    -> return $ Tagged (map conInfo cs) cx $ map conv keys
           TyConI (NewtypeD cx _ keys _ con _)-> return $ Tagged [conInfo con] cx $ map conv keys
           _ -> error ("Unexpected " <> show (typeOf info) <> ": " <> show info)
    where conInfo (NormalC n args) = (n, length args)
          conInfo (RecC n args) = (n, length args)
          conInfo (InfixC _ n _) = (n, 2)
          conInfo (ForallC _ _ con) = conInfo con
#if MIN_VERSION_template_haskell(2,17,0)
          conv (PlainTV nm _) = nm
          conv (KindedTV nm _ _) = nm
#else
          conv (PlainTV nm) = nm
          conv (KindedTV nm _) = nm
#endif

-- | the standard formatter
--
-- Converts @CamelCase@ to @camel-case@.
--
-- see also: 'derivePathInfo' and 'derivePathInfo''
standard :: String -> String
standard =
    intercalate "-" . map (map toLower) . split splitter
  where
    splitter = dropInitBlank . keepDelimsL . whenElt $ isUpper

mkRoute :: Name -> Q [Dec]
mkRoute url =
    do (Tagged cons _ _) <- parseInfo url
       fn <- funD (mkName "route") $
               map (\(con, numArgs) ->
                        do -- methods <- parseMethods con
                           -- runIO $ print methods
                           args <- replicateM numArgs (newName "arg")
                           clause [conP con $ map varP args] (normalB $ foldl' appE (varE (mkName (headLower (nameBase con)))) (map varE args)) []
                   ) cons
       return [fn]
    where
      headLower :: String -> String
      headLower (c:cs) = toLower c : cs

-- work in progress

parseMethods :: Name -> Q [Name]
parseMethods con =
    do info <- reify con
       case info of
         (DataConI _ ty _) ->
             do runIO $ print ty
                runIO $ print $ lastTerm ty
                return $ extractMethods (lastTerm ty)

extractMethods :: Type -> [Name]
extractMethods ty =
    case ty of
      (AppT (ConT con) (ConT method)) ->
          [method]
      (AppT (ConT con) methods) ->
          extractMethods' methods
        where
          extractMethods' :: Type -> [Name]
          extractMethods' t = map (\(ConT n) -> n) (leafs t)

-- | return the 'Type' after the right-most @->@. Or the original 'Type' if there are no @->@.
lastTerm :: Type -> Type
lastTerm t@(AppT l r)
    | hasArrowT l = lastTerm r
    | otherwise   = t
lastTerm t = t

-- | tests if a 'Type' contains an 'ArrowT' somewhere
hasArrowT :: Type -> Bool
hasArrowT ArrowT     = True
hasArrowT (AppT l r) = hasArrowT l || hasArrowT r
hasArrowT _          = False

leafs :: Type -> [Type]
leafs (AppT l@(AppT _ _) r) = leafs l ++ leafs r
leafs (AppT _ r) = leafs r
leafs t          = [t]
