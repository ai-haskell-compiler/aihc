{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.XML.Pickle.Basic where

import           Control.Applicative
import           Control.Monad (ap)
import qualified Control.Category as Cat
import           Control.Exception (Exception)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Data.XML.Types

data PU t a = PU
  { unpickleTree :: t -> UnpickleResult t a
  , pickleTree :: a -> t
  }

data UnpickleResult t a = UnpickleError UnpickleError
                        | NoResult Text -- ^ Not found, description of element
                        | Result a (Maybe t) -- ^ Result and remainder. The
                                             -- remainder is wrapped in @Maybe@
                                             -- to avoid a @Monoid@ constraint
                                             -- on @t@
                                             --
                                             --  /Invariant/: When @t@ is a
                                             -- @Monoid@, the empty remainder
                                             -- should always be @Nothing@
                                             -- instead of @Just mempty@
                          deriving (Functor, Show)


instance Applicative (UnpickleResult t) where
    pure = return
    (<*>) = ap

instance Monad (UnpickleResult t) where
    return x = Result x Nothing
    Result x r >>= f = case f x of
        Result y r' -> Result y (if isJust r then r else r')
        y -> y
    UnpickleError e >>= _ = UnpickleError e
    NoResult e >>= _ = NoResult e


data UnpickleError = ErrorMessage Text
            | TraceStep (Text, Text) UnpickleError
            | Variants [UnpickleError]
            deriving (Show, Typeable)

instance Exception UnpickleError

infixl 6 <++>
(<++>) :: (Text, Text) -> UnpickleError -> UnpickleError
(<++>) = TraceStep

mapUnpickleError :: (UnpickleError -> UnpickleError)
                 -> UnpickleResult t a
                 -> UnpickleResult t a
mapUnpickleError f (UnpickleError e) = UnpickleError $ f e
mapUnpickleError _ x = x

missing :: String -> UnpickleError
missing e = upe $ "Entity not found: " ++ e

missingE :: String -> UnpickleResult t a
missingE = UnpickleError . missing

upe :: String -> UnpickleError
upe e = ErrorMessage (Text.pack e)

showTr :: (Text, Text) -> String
showTr (name, "") = Text.unpack name
showTr (name, extra) = concat [Text.unpack name , " (", Text.unpack extra, ")"]

printUPE :: UnpickleError -> [String]
printUPE (ErrorMessage m) = [Text.unpack m]
printUPE (Variants vs) = concat
                       . zipWith (:) (map (\x -> show x ++ ")") [(1 :: Int)..])
                       . map (map ( "  " ++))
                       $ (printUPE <$> vs)
printUPE (TraceStep t es) =
    let (n, es') = collapsSteps t es
    in  ("-> " ++ showTr  t
         ++ if n > 0
            then (" [x" ++ show (n+1) ++"]" )
            else "")
        : printUPE es'
  where
    collapsSteps t (TraceStep t' ns) | t == t'
        = let (n, ns') = collapsSteps t ns
          in (n+1, ns')
    collapsSteps _ es = (0, es)

ppUnpickleError :: UnpickleError -> String
ppUnpickleError e = "Error while unpickling:\n"
                      ++ unlines (map ("  " ++) (printUPE e))

leftoverE :: String -> UnpickleResult t a
leftoverE l = UnpickleError . upe $ "Leftover Entities" ++ if null l then "" else
                                                             ": " ++ l
child :: Show a => PU a b -> a -> UnpickleResult t b
child xp v = case unpickleTree xp v of
    UnpickleError e -> UnpickleError e
    NoResult e -> missingE $ Text.unpack e
    Result _ (Just es) -> leftoverE $ show es
    Result r Nothing -> Result r Nothing

child' :: PU t a -> t -> UnpickleResult t1 a
child' xp v = case unpickleTree xp v of
    UnpickleError e -> UnpickleError e
    NoResult e -> missingE $ Text.unpack e
    Result _ (Just _es) -> leftoverE ""
    Result r Nothing -> Result r Nothing

leftover :: Maybe t -> UnpickleResult t ()
leftover = Result ()

remList :: [t] -> Maybe [t]
remList [] = Nothing
remList xs = Just xs

mapError :: (UnpickleError -> UnpickleError) -> PU t a -> PU t a
mapError f xp = PU { unpickleTree = mapUnpickleError f . unpickleTree xp
                   , pickleTree = pickleTree xp
                   }

infixl 6 <++.>
(<++.>) :: (Text, Text) -> UnpickleResult t a -> UnpickleResult t a
(<++.>) s = mapUnpickleError (s <++>)

infixr 0 <?>
-- | Override the last backtrace level in the error report.
(<?>) :: (Text, Text) -> PU t a -> PU t a
(<?>) tr = mapError (swapStack tr)
  where
    swapStack ns (TraceStep _s e) = TraceStep ns e
    swapStack _ns e = error $ "Can't replace non-trace step: " ++ show e

(<??>) :: Text -> PU t a -> PU t a
(<??>) tr = mapError (swapStack tr)
  where
    swapStack ns (TraceStep (_,s) e) = TraceStep (ns,s) e
    swapStack _ns e = error $ "Can't replace non-trace step: " ++ show e



infixr 1 <?+>
-- | Add a back trace level to the error report.
(<?+>) :: (Text, Text) -> PU t a -> PU t a
(<?+>) tr = mapError (tr <++>)

data UnresolvedEntityException = UnresolvedEntityException
                                   deriving (Typeable, Show)
instance Exception UnresolvedEntityException

ppName :: Name -> String
ppName (Name local ns pre) = let
  ns' = case ns of
    Nothing -> []
    Just ns'' -> ["{", Text.unpack ns'',"}"]
  pre' = case  pre of
    Nothing -> []
    Just pre'' -> [Text.unpack pre'',":"]
  in  concat . concat $ [["\""],ns', pre', [Text.unpack local], ["\""]]

instance Cat.Category PU where
    id = PU (\t -> Result t Nothing) id
    g . f = PU { pickleTree = pickleTree f . pickleTree g
               , unpickleTree = \val -> case unpickleTree f val of
                   UnpickleError e -> UnpickleError e
                   NoResult e -> NoResult e
                   Result resf re -> case unpickleTree g resf of
                       UnpickleError e -> UnpickleError e
                       NoResult e -> NoResult e
                       Result _ (Just _) -> leftoverE ""
                       Result resg Nothing -> Result resg re
               }
