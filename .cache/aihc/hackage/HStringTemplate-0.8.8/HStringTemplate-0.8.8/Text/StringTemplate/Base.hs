{-# LANGUAGE RelaxedPolyRec, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Base
    (StringTemplate(..), StringTemplateShows(..), ToSElem(..), STGroup,
     Stringable(..), stShowsToSE, inSGen,
     toString, toPPDoc, render, newSTMP, newAngleSTMP,
     getStringTemplate, getStringTemplate',
     setAttribute, setManyAttrib,
     setNativeAttribute, setManyNativeAttrib,
     withContext, optInsertTmpl, setEncoder,
     paddedTrans, SEnv(..), parseSTMP, dumpAttribs,
     checkTemplate, checkTemplateDeep,
     parseSTMPNames
    ) where
import Control.Arrow
import Control.Applicative hiding ((<|>),many,optional)
import Control.Monad
import Control.DeepSeq
import qualified Control.Exception as C
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable
import System.IO.Unsafe

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Text.PrettyPrint.HughesPJ as PP

import Text.StringTemplate.Classes
import Text.StringTemplate.Instances()

{--------------------------------------------------------------------
  Generic Utilities
--------------------------------------------------------------------}

type TmplParser = GenParser Char ((Char, Char),[String],[String],[String])

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = (<$>) . (<$>)
infixr 8 <$$>

(|.) :: (t1 -> t2) -> (t -> t1) -> t -> t2
(|.) f g = f . g
infixr 3 |.

(.>>) :: (Monad m) => m a -> m b -> m b
(.>>) f g = f >> g
infixr 5 .>>

fromMany :: b -> ([a] -> b) -> [a] -> b
fromMany e _ [] = e
fromMany _ f xs  = f xs

swing :: (((a -> c1) -> c1) -> b -> c) -> b -> a -> c
swing = flip . (. flip id)

paddedTrans :: a -> [[a]] -> [[a]]
paddedTrans _ [] = []
paddedTrans n as = take (maximum . map length $ as) . trans $ as
    where trans ([] : xss)  = (n : map h xss) :  trans ([n] : map t xss)
          trans ((x : xs) : xss) = (x : map h xss) : trans (m xs : map t xss)
          trans _ = [];
          h (x:_) = x; h _ = n; t (_:y:xs) = y:xs; t _ = [n];
          m (x:xs) = x:xs; m _ = [n];

{--------------------------------------------------------------------
  StringTemplate and the API
--------------------------------------------------------------------}

-- | A function that generates StringTemplates.
-- This is conceptually a query function into a \"group\" of StringTemplates.
type STGroup a = String -> (StFirst (StringTemplate a))

-- | A String with \"holes\" in it. StringTemplates may be composed of any
-- 'Stringable' type, which at the moment includes 'String's, 'ByteString's,
-- PrettyPrinter 'Doc's, and 'Endo' 'String's, which are actually of type
-- 'ShowS'. When a StringTemplate is composed of a type, its internals are
-- as well, so it is, so to speak \"turtles all the way down.\"
data StringTemplate a = STMP {senv :: SEnv a,  runSTMP :: Either String (SEnv a -> a), chkSTMP :: SEnv a -> (Maybe String, Maybe [String], Maybe [String])}

-- | Renders a StringTemplate to a String.
toString :: StringTemplate String -> String
toString = render

-- | Renders a StringTemplate to a 'Text.PrettyPrint.HughesPJ.Doc'.
toPPDoc :: StringTemplate PP.Doc -> PP.Doc
toPPDoc = render

-- | Generic render function for a StringTemplate of any type.
render :: Stringable a => StringTemplate a -> a
render = either (showStr) id . runSTMP <*> senv

nullEnv :: SEnv a
nullEnv = SEnv M.empty [] mempty id

-- | Returns a tuple of three Maybes. The first is set if there is a parse error in the template.
-- The next is set to a list of attributes that have not been set, or Nothing if all attributes are set.
-- The last is set to a list of invoked templates that cannot be looked up, or Nothing if all invoked templates can be found.
-- Note that this check is shallow -- i.e. missing attributes and templates are only caught in the top level template, not any invoked subtemplate.
checkTemplate :: Stringable a => StringTemplate a -> (Maybe String, Maybe [String], Maybe [String])
checkTemplate t = chkSTMP t (senv t)

-- | Parses a String to produce a StringTemplate, with \'$\'s as delimiters.
-- It is constructed with a stub group that cannot look up other templates.
newSTMP :: Stringable a => String -> StringTemplate a
newSTMP s = STMP nullEnv (parseSTMP ('$','$') s) (chkStmp ('$','$') s)

-- | Parses a String to produce a StringTemplate, delimited by angle brackets.
-- It is constructed with a stub group that cannot look up other templates.
newAngleSTMP :: Stringable a => String -> StringTemplate a
newAngleSTMP s = STMP nullEnv (parseSTMP ('<','>') s) (chkStmp ('<','>') s)

-- | Yields a StringTemplate with the appropriate attribute set.
-- If the attribute already exists, it is appended to a list.
setAttribute :: (ToSElem a, Stringable b) => String -> a -> StringTemplate b -> StringTemplate b
setAttribute s x st = st {senv = envInsApp s (toSElem x) (senv st)}

-- | Yields a StringTemplate with the appropriate attributes set.
-- If any attribute already exists, it is appended to a list.
setManyAttrib :: (ToSElem a, Stringable b) => [(String, a)] -> StringTemplate b -> StringTemplate b
setManyAttrib = flip . foldl' . flip $ uncurry setAttribute

-- | Yields a StringTemplate with the appropriate attribute set.
-- If the attribute already exists, it is appended to a list.
-- This will not translate the attribute through any intermediate
-- representation, so is more efficient when, e.g. setting
-- attributes that are large bytestrings in a bytestring template.
setNativeAttribute :: Stringable b => String -> b -> StringTemplate b -> StringTemplate b
setNativeAttribute s x st = st {senv = envInsApp s (SNAT x) (senv st)}

-- | Yields a StringTemplate with the appropriate attributes set.
-- If any attribute already exists, it is appended to a list.
-- Attributes are added natively, which may provide
-- efficiency gains.
setManyNativeAttrib :: (Stringable b) => [(String, b)] -> StringTemplate b -> StringTemplate b
setManyNativeAttrib = flip . foldl' . flip $ uncurry setNativeAttribute

-- | Replaces the attributes of a StringTemplate with those
-- described in the second argument. If the argument does not yield
-- a set of named attributes but only a single one, that attribute
-- is named, as a default, \"it\".
withContext :: (ToSElem a, Stringable b) => StringTemplate b -> a -> StringTemplate b
withContext st x = case toSElem x of
                     SM a -> st {senv = (senv st) {smp = a}}
                     b -> st {senv = (senv st) {smp = M.singleton "it" b}}

-- | Queries an String Template Group and returns Just the appropriate
-- StringTemplate if it exists, otherwise, Nothing.
getStringTemplate :: (Stringable a) => String -> STGroup a -> Maybe (StringTemplate a)
getStringTemplate s sg = stGetFirst (sg s)

-- | As with 'getStringTemplate' but never inlined, so appropriate for use
-- with volatile template groups.
{-# NOINLINE getStringTemplate' #-}
getStringTemplate' :: (Stringable a) => String -> STGroup a -> Maybe (StringTemplate a)
getStringTemplate' s sg = stGetFirst (sg s)

-- | Adds a set of global options to a single template
optInsertTmpl :: [(String, String)] -> StringTemplate a -> StringTemplate a
optInsertTmpl x st = st {senv = optInsert (map (second justSTR) x) (senv st)}

-- | Sets an encoding function of a template that all values are
-- rendered with. For example one useful encoder would be 'Text.Html.stringToHtmlString'. All attributes will be encoded once and only once.
setEncoder :: (Stringable a) => (a -> a) -> StringTemplate a -> StringTemplate a
setEncoder x st = st {senv = (senv st) {senc = x} }

-- | A special template that simply dumps the values of all the attributes set in it.
-- This may be made available to any template as a function by adding it to its group.
-- I.e. @ myNewGroup = addSuperGroup myGroup $ groupStringTemplates [("dumpAttribs", dumpAttribs)] @
dumpAttribs :: Stringable a => StringTemplate a
dumpAttribs = STMP nullEnv (Right $ \env -> showVal env (SM $ smp env)) (const (Nothing, Nothing, Nothing))

{--------------------------------------------------------------------
  Internal API
--------------------------------------------------------------------}
--IMPLEMENT groups having stLookup return a Maybe for regions

data SEnv a = SEnv {smp :: SMap a, sopts :: [(String, (SEnv a -> SElem a))], sgen :: STGroup a, senc :: a -> a}

inSGen :: (STGroup a -> STGroup a) -> StringTemplate a -> StringTemplate a
inSGen f st@STMP{senv = env} = st {senv = env {sgen = f (sgen env)} }

{-
envLookup :: String -> SEnv a -> Maybe (SElem a)
envLookup x = M.lookup x . smp
-}

envLookupEx :: String -> SEnv a -> SElem a
envLookupEx x snv = case M.lookup x (smp snv) of
                      Just a -> a
                      Nothing -> case optLookup "throwException" snv of
                                   Just _ -> C.throw $ NoAttrib x
                                   Nothing -> SNull

envInsert :: (String, SElem a) -> SEnv a -> SEnv a
envInsert (s, x) y = y {smp = M.insert s x (smp y)}
envInsApp :: Stringable a => String -> SElem a -> SEnv a -> SEnv a
envInsApp  s  x  y = y {smp = M.insertWith go s x (smp y)}
    where go a (LI bs) = LI (a:bs)
          go a b = LI [a,b]

optLookup :: String -> SEnv a -> Maybe (SEnv a -> SElem a)
optLookup x = lookup x . sopts
optInsert :: [(String, SEnv a -> SElem a)] -> SEnv a -> SEnv a
optInsert x env = env {sopts = x ++ sopts env}
nullOpt :: SEnv a -> SElem a
nullOpt = fromMaybe (justSTR "") =<< optLookup "null"

stLookup :: (Stringable a) => String -> SEnv a -> StringTemplate a
stLookup x env = maybe (newSTMP ("No Template Found for: " ++ x))
                 (\st-> st {senv = mergeSEnvs env (senv st)}) $ stGetFirst (sgen env x)

--merges values of former into latter, preserving encoder
--of latter, as well as non-overriden options. group of latter is overridden.
mergeSEnvs :: SEnv a -> SEnv a -> SEnv a
mergeSEnvs x y = SEnv {smp = M.union (smp x) (smp y), sopts = (sopts y ++ sopts x), sgen = sgen x, senc = senc y}

parseSTMP :: (Stringable a) => (Char, Char) -> String -> Either String (SEnv a -> a)
parseSTMP x = either (Left . show) Right . runParser (stmpl False) (x,[],[],[]) "" . dropTrailingBr

dropTrailingBr :: String -> String
dropTrailingBr ('\r':'\n':[]) = []
dropTrailingBr ('\n':[]) = []
dropTrailingBr [] = []
dropTrailingBr (x:xs) = x : dropTrailingBr xs

getSeps :: TmplParser (Char, Char)
getSeps = (\(x,_,_,_) -> x) <$> getState

tellName :: String -> TmplParser ()
tellName x = getState >>= \(s,q,n,t) -> setState (s,q,x:n,t)

tellQQ :: String -> TmplParser ()
tellQQ x = getState >>= \(s,q,n,t) -> setState (s,x:q,n,t)

tellTmpl :: String -> TmplParser ()
tellTmpl x = getState >>= \(s,q,n,t) -> setState (s,q,n,x:t)

-- | Gets all quasiquoted names, normal names & templates used in a given template.
-- Must be passed a pair of chars denoting the delimeters to be used.
parseSTMPNames :: (Char, Char) -> String -> Either ParseError ([String],[String],[String])
parseSTMPNames cs s = runParser getRefs (cs,[],[],[]) "" s
    where getRefs = do
            _ <- stmpl False :: TmplParser (SEnv String -> String)
            (_,qqnames,regnames,tmpls) <- getState
            return (qqnames, regnames, tmpls)

chkStmp :: Stringable a => (Char, Char) -> String -> SEnv a -> (Maybe String, Maybe [String], Maybe [String])
chkStmp cs s snv = case parseSTMPNames cs s of
                     Left err -> (Just $ "Parse error: " ++ show err, Nothing, Nothing)
                     Right (_, regnames, tmpls) ->
                         let nonms   = filter (\x -> not $ elem x (M.keys $ smp snv)) regnames
                             notmpls = filter (\x -> isNothing $ stGetFirst (sgen snv x)) tmpls
                         in (Nothing, if null nonms then Nothing else Just nonms,
                                      if null notmpls then Nothing else Just notmpls)

data TmplException = NoAttrib String | NoTmpl String | ParseError String String deriving (Show, Typeable)
instance C.Exception TmplException

-- | Generic render function for a StringTemplate of any type.
renderErr :: Stringable a => String -> StringTemplate a -> a
renderErr n t = case runSTMP t of
                Right rt -> rt (senv t)
                Left err -> case optLookup "throwException" (senv t) of
                              Just _ -> C.throw $ ParseError n err
                              Nothing -> showStr err (senv t)

-- | Returns a tuple of three lists. The first is of templates with parse errors, and their errors. The next is of missing attributes, and the last is of missing templates. If there are no errors, then all lists will be empty. This check is performed recursively.
checkTemplateDeep :: (Stringable a, NFData a) => StringTemplate a -> ([(String,String)], [String], [String])
checkTemplateDeep t = case runSTMP t of
                        Left err -> ([("Top Level Template", err)], [],[])
                        Right _ -> unsafePerformIO $ go ([],[],[]) $ inSGen (`mappend` nullGroup) $ optInsertTmpl [("throwException","true")] t
    where go (e1,e2,e3) tmpl = (C.evaluate (rnf $ render tmpl) >> return (e1,e2,e3)) `C.catch`
                                  \e -> case e of NoTmpl x -> go (e1,e2,x:e3) $ addSub x tmpl
                                                  NoAttrib x -> go (e1,x:e2, e3) $ setAttribute x "" tmpl
                                                  ParseError n x -> go ((n,x):e1,e2,e3) $ addSub n tmpl
          addSub x tmpl = inSGen (mappend $ blankGroup x) tmpl
          blankGroup x s = StFirst $ if x == s then Just (newSTMP "") else Nothing
          nullGroup x = StFirst $ Just (C.throw $ NoTmpl x)

{--------------------------------------------------------------------
  Internal API for polymorphic display of elements
--------------------------------------------------------------------}

mconcatMap' :: Stringable a => SEnv a -> [b] -> (b -> a) -> a
mconcatMap' snv xs f = mintercalate sep . map f $ xs
    where sep = showVal snv $ fromMaybe (justSTR "") =<< optLookup "separator" $ snv

showVal :: Stringable a => SEnv a -> SElem a -> a
showVal snv se = case se of
                   STR x  -> stEncode x
                   BS  x  -> stEncodeBS x
                   TXT x  -> stEncodeText x
                   LI xs  -> joinUpWith showVal xs
                   SM sm  -> joinUpWith showAssoc $ M.assocs sm
                   STSH x -> stEncode (format x)
                   SNAT x -> senc snv x
                   SBLE x -> x
                   SNull  -> showVal <*> nullOpt $ snv
    where format = maybe stshow . stfshow <*> optLookup "format" $ snv
          joinUpWith f xs = mconcatMap' snv xs (f snv)
          showAssoc e (k,v) = stEncode (k ++ ": ") `mlabel` showVal e v
          stEncode     = senc snv . stFromString
          stEncodeBS   = senc snv . stFromByteString
          stEncodeText = senc snv . stFromText

showStr :: Stringable a => String -> SEnv a -> a
showStr = const . stFromString

{--------------------------------------------------------------------
  Utility Combinators
--------------------------------------------------------------------}

justSTR :: String -> b -> SElem a
justSTR = const . STR
stshow :: STShow -> String
stshow (STShow a) = stringTemplateShow a
stfshow :: Stringable a => SEnv a -> (SEnv a -> SElem a) -> STShow -> String
stfshow snv fs (STShow a) = stringTemplateFormattedShow
                            (stToString <$$> showVal <*> fs $ snv) a

around :: Char -> GenParser Char st t -> Char -> GenParser Char st t
around x p y = do {_ <- char x; v<-p; _ <- char y; return v}
spaced :: GenParser Char st t -> GenParser Char st t
spaced p = do {spaces; v<-p; spaces; return v}

identifierChar :: GenParser Char st Char
identifierChar = alphaNum <|> char '_'

word :: GenParser Char st String
word = many1 identifierChar

comlist :: GenParser Char st a -> GenParser Char st [a]
comlist p = spaced (p `sepBy1` spaced (char ','))

props :: Stringable a => TmplParser [SEnv a -> SElem a]
props = many $ char '.' >> (around '(' subexprn ')' <|> justSTR <$> word)

escapedChar, escapedStr :: String -> GenParser Char st String
escapedChar chs =
    noneOf chs >>= \x -> if x == '\\' then anyChar >>= \y -> return [y] else return [x]
escapedStr chs = concat <$> many1 (escapedChar chs)

{-
escapedStr' chs = dropTrailingBr <$> escapedStr chs
-}

{--------------------------------------------------------------------
  The Grammar
--------------------------------------------------------------------}
myConcat :: Stringable a => [SEnv a -> a] -> (SEnv a -> a)
myConcat xs a = mconcatMap xs ($ a)


-- | if p is true, stmpl can fail gracefully, false it dies hard.
-- Set to false at the top level, and true within if expressions.
stmpl :: Stringable a => Bool -> TmplParser (SEnv a -> a)
stmpl p = do
  (ca, cb) <- getSeps
  myConcat <$> many (showStr <$> escapedStr [ca] <|> try (around ca optExpr cb)
                    <|> try comment <|> bl <?> "template")
      where bl | p = try blank | otherwise = blank

subStmp :: Stringable a => TmplParser (([SElem a], [SElem a]) -> SEnv a -> a)
subStmp = do
  (ca, cb) <- getSeps
  udEnv <- option (transform ["it"]) (transform <$> try attribNames)
  st <- myConcat <$> many (showStr <$> escapedStr (ca:"}|")
                         <|> try (around ca optExpr cb)
                         <|> try comment <|> blank  <?> "subtemplate")
  return (st <$$> udEnv)
      where transform an (att,is) =
                flip (foldr envInsert) $ zip ("i":"i0":an) (is++att)
            attribNames = (char '|' >>) . return =<< comlist (spaced word)

comment :: Stringable a => TmplParser (SEnv a -> a)
comment = do
  (ca, cb) <- getSeps
  _ <- string [ca,'!'] >> manyTill anyChar (try . string $ ['!',cb])
  return (showStr "")

blank :: Stringable a => TmplParser (SEnv a -> a)
blank = do
  (ca, cb) <- getSeps
  _ <- char ca
  spaces
  _ <- char cb
  return (showStr "")

optExpr :: Stringable a => TmplParser (SEnv a -> a)
optExpr = do
  (_, cb) <- getSeps
  (try (string ("else"++[cb])) <|> try (string "elseif(") <|>
    try (string "endif")) .>> fail "Malformed If Statement." <|> return ()
  expr <- try ifstat <|> spaced exprn
  opts <- (char ';' >> optList) <|> return []
  return $ expr . optInsert opts
      where -- opt = around ';' (spaced word) '=' >>= (<$> spaced subexprn) . (,)
            optList = sepBy oneOpt (char ',' <|> char ';')
            oneOpt = do
              o <- spaced word
              _ <- char '='
              v <- spaced subexprn
              return (o,v)

{--------------------------------------------------------------------
  Statements
--------------------------------------------------------------------}

optLine :: TmplParser ()
optLine = optional (char '\r') >> optional (char '\n')

--if env then do stuff
getProp :: Stringable a => [SEnv a -> SElem a] -> SElem a -> SEnv a -> SElem a
getProp (p:ps) (SM mp) env =
  case M.lookup (stToString . showVal env $ p env) mp of
    Just prop -> getProp ps prop env
    Nothing -> case optLookup "throwException" env of
                 Just _ -> C.throw . NoAttrib $ "yeek" --intercalate "." . map showIt $ (p:ps)
                 Nothing -> SNull
  --where showIt x = stToString . showVal env $ x env
getProp (_:_) _ _ = SNull
getProp _ se _ = se

ifIsSet :: t -> t -> Bool -> SElem a -> t
ifIsSet t e n SNull = if n then e else t
ifIsSet t e n _ = if n then t else e

ifstat ::Stringable a => TmplParser (SEnv a -> a)
ifstat = do
  (_, cb) <- getSeps
  _ <- string "if("
  n <- option True (char '!' >> return False)
  e <- subexprn
  p <- props
  char ')' >> char cb >> optLine
  act <- stmpl True
  cont <- (try elseifstat <|> try elsestat <|> endifstat)
  return (ifIsSet act cont n =<< getProp p =<< e)

elseifstat ::Stringable a => TmplParser (SEnv a -> a)
elseifstat = getSeps >>= char . fst >> string "else" >> ifstat

elsestat ::Stringable a => TmplParser (SEnv a -> a)
elsestat = do
  (ca, cb) <- getSeps
  _ <- around ca (string "else") cb
  optLine
  act <- stmpl True
  _ <- char ca >> string "endif"
  return act

endifstat ::Stringable a => TmplParser (SEnv a -> a)
endifstat = getSeps >>= char . fst >> string "endif" >> return (showStr "")

{--------------------------------------------------------------------
  Expressions
--------------------------------------------------------------------}

exprn :: Stringable a => TmplParser (SEnv a -> a)
exprn = do
  exprs <- comlist ( (SBLE <$$> around '(' exprn ')')
                     <|> subexprn)
             <?> "expression"
  templ <- tmplChain
  return $ fromMany (showVal <*> head exprs)
             ((sequence exprs >>=) . seqTmpls') templ
      where tmplChain = many (char ':' >> iterApp <$> comlist (anonTmpl <|> regTemplate)) <?> "template call"

seqTmpls' :: Stringable a => [[SElem a] -> SEnv a -> [a]] -> [SElem a] -> SEnv a -> a
seqTmpls' tmpls elems snv = mintercalate sep $ seqTmpls tmpls elems snv
    where sep = showVal snv $ fromMaybe (justSTR "") =<< optLookup "separator" $ snv

seqTmpls :: Stringable a => [[SElem a] -> SEnv a -> [a]] -> [SElem a] -> SEnv a -> [a]
seqTmpls [f]    y snv = f y snv
seqTmpls (f:fs) y snv = concatMap (\x -> seqTmpls fs x snv) (map ((:[]) . SBLE) $ f y snv)
seqTmpls  _ _ _   = [stFromString ""]

subexprn :: Stringable a => TmplParser (SEnv a -> SElem a)
subexprn = cct <$> spaced
            (braceConcat
             <|> SBLE <$$> ($ ([SNull],ix0)) <$> try regTemplate
             <|> attrib
             <|> SBLE <$$> ($ ([SNull],ix0)) <$> anonTmpl
             <?> "expression")
           `sepBy1` spaced (char '+')
    where cct xs@(_:_:_) = SBLE |.
                           flip mconcatMap <$> showVal <*> sequence xs
          cct [x] = x
          cct  _  = const SNull

braceConcat :: Stringable a => TmplParser (SEnv a -> SElem a)
braceConcat = LI . foldr go [] <$$> sequence <$> around '['(comlist subexprn)']'
    where go (LI x) lst = x++lst; go x lst = x:lst

literal :: GenParser Char st (b -> SElem a)
literal = justSTR <$> (around '"' (concat <$> many (escapedChar "\"")) '"'
                   <|> around '\'' (concat <$> many (escapedChar "'")) '\'')

attrib :: Stringable a => TmplParser (SEnv a -> SElem a)
attrib = do
  a <-     literal
       <|> try functn
       <|> envLookupEx <$> regWord
       <|> envLookupEx <$> qqWord
       <|> around '(' subexprn ')'
          <?> "attribute"
  proprs <- props
  return $ fromMany a ((a >>=) . getProp) proprs
      where qqWord = do
              w <- around '`' word '`'
              tellQQ w
              return $ '`' : w ++ "`"
            regWord = do
              w <- word
              tellName w
              return w

--add null func
functn :: Stringable a => TmplParser (SEnv a -> SElem a)
functn = do
  f <- string "first" <|> try (string "rest") <|> string "reverse"
       <|> string "strip"
       <|> try (string "length") <|> string "last" <?> "function"
  (fApply f .) <$> around '(' subexprn ')'
      where fApply str (LI xs)
                | str == "first"  = if null xs then SNull else head xs
                | str == "last"   = if null xs then SNull else last xs
                | str == "rest"   = if null xs then SNull else (LI . tail) xs
                | str == "reverse" = LI . reverse $ xs
                | str == "strip"  = LI . filter (not . liNil) $ xs
                | str == "length" = STR . show . length $ xs
            fApply str x
                | str == "rest"   = LI []
                | str == "length" = STR "1"
                | otherwise       = x
            liNil (LI x) = null x
            liNil _      = False

{--------------------------------------------------------------------
  Templates
--------------------------------------------------------------------}
--change makeTmpl to do notation for clarity?



mkIndex :: (Num b, Show b) => [b] -> [[SElem a]]
mkIndex = map ((:) . STR . show . (1+) <*> (:[]) . STR . show)
ix0 :: [SElem a]
ix0 = [STR "1",STR "0"]

cycleApp :: (Stringable a) => [([SElem a], [SElem a]) -> SEnv a -> a] -> [([SElem a], [SElem a])]  -> SEnv a -> [a]
cycleApp x y snv = map ($ snv) (zipWith ($) (cycle x) y)

pluslen :: [a] -> [([a], [SElem b])]
pluslen xs = zip (map (:[]) xs) $ mkIndex [0..(length xs)]

liTrans :: [SElem a] -> [([SElem a], [SElem a])]
liTrans = pluslen' . paddedTrans SNull . map u
    where u (LI x) = x; u x = [x]
          pluslen' xs = zip xs $ mkIndex [0..(length xs)]

--map repeatedly, then finally concat
iterApp :: Stringable a => [([SElem a], [SElem a]) -> SEnv a -> a] -> [SElem a] -> SEnv a -> [a]
iterApp [f] (LI xs:[])    snv = map (flip f snv) (pluslen xs)
iterApp [f] vars@(LI _:_) snv = map (flip f snv) (liTrans vars)
iterApp [f] v             snv = [f (v,ix0) snv]
iterApp fs (LI xs:[])     snv = cycleApp fs (pluslen xs) snv
iterApp fs vars@(LI _:_)  snv = cycleApp fs (liTrans vars) snv
iterApp fs xs             snv = cycleApp fs (pluslen xs) snv

anonTmpl :: Stringable a => TmplParser (([SElem a], [SElem a]) -> SEnv a -> a)
anonTmpl = around '{' subStmp '}'

regTemplate :: Stringable a => TmplParser (([SElem a], [SElem a]) -> SEnv a -> a)
regTemplate = do
  try (functn::TmplParser (SEnv String -> SElem String)) .>> fail "" <|> return ()
  name <- justSTR <$> many1 (identifierChar <|> char '/')
          <|> around '(' subexprn ')'
  tryTellTmpl (name nullEnv)
  vals <- around '(' (spaced $ try assgn <|> anonassgn <|> return []) ')'
  return $ join . (. name) . makeTmpl vals
      where makeTmpl v ((se:_),is) (STR x)  =
                renderErr x |. stBind . (zip ["it","i","i0"] (se:is) ++)
                             . swing (map . second) v <*> stLookup x
            makeTmpl _ _ _ = showStr "Invalid Template Specified"
            stBind v st = st {senv = foldr envInsert (senv st) v}
            anonassgn = (:[]) . (,) "it" <$> subexprn
            assgn = (spaced word >>= (<$> char '=' .>> spaced subexprn) . (,))
                    `sepEndBy1` char ';'
            tryTellTmpl (STR x) = tellTmpl x
            tryTellTmpl _ = return ()

--DEBUG

{-pTrace s = pt <|> return ()
    where pt = try $
               do
                 x <- try $ many1 anyChar
                 trace (s++": " ++x) $ try $ char 'z'
                 fail x
-}
