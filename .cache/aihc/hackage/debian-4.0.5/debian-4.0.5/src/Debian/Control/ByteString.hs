{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Control.ByteString
    ( Control'(..)
    , Paragraph'(..)
    , Field'(..)
    , Control
    , Paragraph
    , Field
    , ControlFunctions(..)
    -- * Helper Functions
    , mergeControls
    , fieldValue
    , removeField
    , prependFields
    , appendFields
    , renameField
    , modifyField
    , raiseFields
    ) where

-- Standard GHC modules

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..))
#endif
import Control.Applicative (Alternative(..))
import qualified Control.Exception as E
import "mtl" Control.Monad.State

import Data.Char(toLower, isSpace)
import Data.List
import Control.Monad (MonadPlus(..), ap, liftM, unless)

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

-- Third Party Modules

import qualified Data.ByteString.Char8 as C

import Debian.Control.Common hiding (protectFieldText')

-- Local Modules

-- import ByteStreamParser

-- * Types
{-
newtype Control = Control [Paragraph]
newtype Paragraph = Paragraph [Field]
newtype Field = Field (C.ByteString, C.ByteString)
-}

type Control = Control' C.ByteString
type Paragraph = Paragraph' C.ByteString
type Field = Field'  C.ByteString
-- * Control Parser

type ControlParser a = Parser C.ByteString a

pKey :: ControlParser C.ByteString
pKey = notEmpty $ pTakeWhile (\c -> (c /= ':') && (c /= '\n'))

pValue :: ControlParser C.ByteString
pValue = Parser $ \bs ->
    let newlines = C.elemIndices '\n' bs
        rest = dropWhile continuedAfter newlines ++ [C.length bs]
        continuedAfter i = bs `safeIndex` (i+1) `elem` map Just " \t#"
        (text, bs') = C.splitAt (head rest) bs
    in Ok (text, bs')

pField :: ControlParser Field
pField =
    do k <- pKey
       _ <- pChar ':'
       v <- pValue
--       pChar '\n'
       (pChar '\n' >> return ()) <|> pEOF
       return (Field (k,v))

pComment :: ControlParser Field
pComment = Parser $ \bs ->
    let newlines = C.elemIndices '\n' bs
        linestarts = 0 : map (+1) newlines
        rest = dropWhile commentAt linestarts ++ [C.length bs]
        commentAt i = bs `safeIndex` i == Just '#'
        (text, bs') = C.splitAt (head rest) bs
    in if C.null text
       then Empty
       else Ok (Comment text, bs')

pParagraph :: ControlParser Paragraph
pParagraph =
    do f <- pMany1 (pComment <|> pField)
       pSkipMany (pChar '\n')
       return (Paragraph f)

pControl :: ControlParser Control
pControl =
    do pSkipMany (pChar '\n')
       c <- pMany pParagraph
       return (Control c)


-- parseControlFromFile :: FilePath -> IO (Either String Control)

instance ControlFunctions C.ByteString where
    parseControlFromFile fp =
        do c <- C.readFile fp
           case parse pControl c of
             Nothing -> return (Left (newErrorMessage (Message ("Failed to parse " ++ fp)) (newPos fp 0 0)))
             (Just (cntl,_)) -> return (Right cntl)
    parseControlFromHandle sourceName handle =
        E.try (C.hGetContents handle) >>=
        either (\ (e :: E.SomeException) -> error ("parseControlFromHandle ByteString: Failure parsing " ++ sourceName ++ ": " ++ show e)) (return . parseControl sourceName)
    parseControl sourceName c =
        do case parse pControl c of
             Nothing -> Left (newErrorMessage (Message ("Failed to parse " ++ sourceName)) (newPos sourceName 0 0))
             Just (cntl,_) -> Right cntl
    lookupP fieldName (Paragraph fields) =
        let pFieldName = C.pack (map toLower fieldName) in
        find (\ (Field (fieldName',_)) -> C.map toLower fieldName' == pFieldName) fields
    -- NOTE: probably inefficient
    stripWS = C.reverse . strip . C.reverse . strip
        where strip = C.dropWhile (flip elem [' ', '\t'])
    protectFieldText = protectFieldText'
    asString = C.unpack

protectFieldText' :: C.ByteString -> C.ByteString
protectFieldText' s =
    case C.lines s of
      [] -> mempty
      (l : ls) -> dropWhileEnd isSpace $ C.unlines $ l : map protect ls
    where
      dropWhileEnd :: (Char -> Bool) -> C.ByteString -> C.ByteString
      dropWhileEnd func = C.reverse . C.dropWhile func . C.reverse
      protect :: C.ByteString -> C.ByteString
      protect l = maybe mempty (\ c -> if isHorizSpace c then l else C.cons ' ' l) (C.find (const True :: Char -> Bool) l)
      isHorizSpace c = elem c " \t"

{-
main =
    do [fp] <- getArgs
       C.readFile fp >>= \c -> maybe (putStrLn "failed.") (print . length . fst) (parse pControl c)
-}
-- * Helper Functions

safeIndex :: C.ByteString -> Int -> Maybe Char
bs `safeIndex` i = if i < C.length bs then Just (bs `C.index` i) else Nothing

-- * Parser

data Result a
    = Ok a
    | Fail
    | Empty
      deriving Show

-- m2r :: Maybe a -> Result a
-- m2r (Just a) = Ok a
-- m2r Nothing = Empty

r2m :: Result a -> Maybe a
r2m (Ok a) = Just a
r2m _ = Nothing

newtype Parser state a = Parser { unParser :: (state -> Result (a, state)) }

instance Functor (Parser state) where
    fmap f m =
        Parser $ \ state ->
            let r = (unParser m) state in
            case r of
              Ok (a,state') -> Ok (f a,state')
              Empty -> Empty
              Fail -> Fail

instance Applicative (Parser state) where
    pure = return
    (<*>) = ap

instance Alternative (Parser state) where
    empty =
        Parser $ \state ->
            (unParser mzero) state
    (<|>) = mplus

instance Monad (Parser state) where
    return a = Parser (\s -> Ok (a,s))
    m >>= f =
        Parser $ \state ->
            let r = (unParser m) state in
            case r of
              Ok (a,state') ->
                  case unParser (f a) $ state' of
                    Empty -> Fail
                    o -> o
              Empty -> Empty
              Fail -> Fail

instance MonadPlus (Parser state) where
    mzero = Parser (const Empty)
    mplus (Parser p1) (Parser p2) =
        Parser (\s -> case p1 s of
                        Empty -> p2 s
                        o -> o
               )

--       Parser (\s -> maybe (p2 s) (Just) (p1 s))


_pSucceed :: a -> Parser state a
_pSucceed = return

_pFail :: Parser state a
_pFail = Parser (const Empty)


satisfy :: (Char -> Bool) -> Parser C.ByteString Char
satisfy f =
    Parser $ \bs ->
        if C.null bs
        then Empty
        else let (s,ss) = (C.head bs, C.tail bs) in
             if (f s)
                then Ok (s,ss)
                else Empty

pChar :: Char -> Parser C.ByteString Char
pChar c = satisfy ((==) c)


_try :: Parser state a -> Parser state a
_try (Parser p) =
    Parser $ \bs -> case (p bs) of
                      Fail -> Empty
                      o -> o

pEOF :: Parser C.ByteString ()
pEOF =
    Parser $ \bs -> if C.null bs then Ok ((),bs) else Empty

pTakeWhile :: (Char -> Bool) -> Parser C.ByteString C.ByteString
pTakeWhile f =
    Parser $ \bs -> Ok (C.span f bs)

_pSkipWhile :: (Char -> Bool) -> Parser C.ByteString ()
_pSkipWhile p =
    Parser $ \bs -> Ok ((), C.dropWhile p bs)

pMany ::  Parser st a -> Parser st [a]
pMany p
    = scan id
    where
      scan f = do x <- p
                  scan (\tail -> f (x:tail))
               <|> return (f [])

notEmpty :: Parser st C.ByteString -> Parser st C.ByteString
notEmpty (Parser p) =
    Parser $ \s -> case p s of
                     o@(Ok (a, _s)) ->
                         if C.null a
                         then Empty
                         else o
                     x -> x

pMany1 :: Parser st a -> Parser st [a]
pMany1 p =
    do x <- p
       xs <- pMany p
       return (x:xs)

pSkipMany :: Parser st a -> Parser st ()
pSkipMany p = scan
    where
      scan = (p >> scan) <|> return ()

_pSkipMany1 :: Parser st a -> Parser st ()
_pSkipMany1 p = p >> pSkipMany p

parse :: Parser state a -> state -> Maybe (a, state)
parse p s = r2m ((unParser p) s)
