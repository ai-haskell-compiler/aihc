{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
{- |
Module      :  Script
Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2014, 2018, 2020, 2024 Douglas Burke
License     :  GPL V2

Maintainer  :  Douglas Burke
Stability   :  experimental
Portability :  CPP, OverloadedStrings

This module implements the Swish script processor:  it parses a script
from a supplied string, and returns a list of Swish state transformer
functions whose effect, when applied to a state value, is to implement
the supplied script.

-}

module Swish.Script
    ( 
      -- * Syntax
      -- $syntax
      
      -- ** Defining a prefix
      -- $prefixLine
      
      -- ** Naming a graph
      -- $nameItem
      
      -- ** Reading and writing graphs
      
      -- $readGraph
      
      -- $writeGraph
      
      -- ** Merging graphs
      -- $mergeGraphs
      
      -- ** Comparing graphs
      
      -- $compareGraphs
      
      -- $assertEquiv
      
      -- $assertMember
      
      -- ** Defining rules
      
      -- $defineRule
      
      -- $defineRuleset
      
      -- $defineConstraints
      
      -- ** Apply a rule
      -- $fwdChain
      
      -- $bwdChain
      
      -- ** Define a proof
      -- $proof
      
      -- * An example script
      -- $exampleScript
      
      -- * Parsing
      
      parseScriptFromText 
    )
where

import Swish.Datatype (typeMkRules)
import Swish.Monad ( SwishStateIO, SwishStatus(..))
import Swish.Monad (modGraphs, findGraph, findFormula
                   , modRules, findRule
                   , modRulesets, findRuleset
                   , findOpenVarModify, findDatatype
                   , setInfo, setError, setStatus)
import Swish.Proof (explainProof, showsProof)
import Swish.Rule (Formula(..), Rule(..)) 
import Swish.Ruleset (makeRuleset, getRulesetRule, getRulesetNamespace, getMaybeContextRule)
import Swish.VarBinding (composeSequence)

import Swish.RDF.Datatype (RDFDatatype)

import Swish.RDF.Ruleset (RDFFormula, RDFRule, RDFRuleset)
import Swish.RDF.Ruleset (makeRDFClosureRule)
import Swish.RDF.Proof (RDFProofStep)
import Swish.RDF.Proof (makeRDFProof, makeRDFProofStep)
import Swish.RDF.VarBinding (RDFVarBindingModify)

import Swish.RDF.GraphShowLines ()

import Swish.RDF.Graph
    ( RDFGraph, RDFLabel(..)
    , NamespaceMap
    , setNamespaces
    , merge, addGraphs
    )

import Swish.RDF.Parser.Utils (whiteSpace, lexeme, symbol, eoln, manyTill)

import Swish.RDF.Parser.N3
    ( parseAnyfromText
    , parseN3      
    , N3Parser, N3State(..)
    , getPrefix
    , subgraph
    , n3symbol -- was uriRef2,
    , quickVariable -- was varid
    , lexUriRef
    , newBlankNode
    )

import Swish.Namespace (ScopedName, getScopeNamespace)
import Swish.QName (QName, qnameFromURI)

import Swish.RDF.Formatter.N3 (formatGraphAsBuilder)

import Swish.Utils.ListHelpers (flist)

import Text.ParserCombinators.Poly.StateText

import Control.Monad (unless, when, void)
import Control.Monad.State (modify, gets, lift)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

#if MIN_VERSION_base(4, 7, 0)
import Data.Functor (($>))
#endif

import Network.URI (URI(..))

import qualified Control.Exception as CE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO as LIO
import qualified System.IO.Error as IO

#if !MIN_VERSION_base(4, 7, 0)
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

------------------------------------------------------------
--
--  The parser used to be based on the Notation3 parser, and used many
--  of the same syntax productions, but the top-level productions used
--  are quite different. With the parser re-write it's less clear
--  what is going on.
--
-- NOTE: during the parser re-write we strip out some of this functionality
-- 

-- | Parser for Swish script processor
parseScriptFromText :: 
  Maybe QName -- ^ Default base for the script
  -> L.Text   -- ^ Swish script
  -> Either String [SwishStateIO ()]
parseScriptFromText = parseAnyfromText script 

----------------------------------------------------------------------
--  Syntax productions
----------------------------------------------------------------------

between :: Parser s lbr -> Parser s rbr -> Parser s a -> Parser s a
between = bracket

n3SymLex :: N3Parser ScopedName
n3SymLex = lexeme n3symbol

setTo :: N3Parser ()
setTo = isymbol ":-"

semicolon :: N3Parser ()
semicolon = isymbol ";"

comma :: N3Parser ()
comma = isymbol ","

commentText :: N3Parser String
commentText = semicolon *> restOfLine

script :: N3Parser [SwishStateIO ()]
script = do
  whiteSpace
  scs <- many command
  eof
  return scs

isymbol :: String -> N3Parser ()
isymbol = void . symbol

command :: N3Parser (SwishStateIO ())
command =
  prefixLine
  <|> nameItem
  <|> readGraph
  <|> writeGraph
  <|> mergeGraphs
  <|> compareGraphs
  <|> assertEquiv
  <|> assertMember
  <|> defineRule
  <|> defineRuleset
  <|> defineConstraints
  <|> checkProofCmd
  <|> fwdChain
  <|> bwdChain

prefixLine :: N3Parser (SwishStateIO ())
prefixLine = do
  -- try $ isymbol "@prefix"
  isymbol "@prefix"
  getPrefix
  whiteSpace
  isymbol "."
  return $ return ()

--  name :- graph
--  name :- ( graph* )
nameItem :: N3Parser (SwishStateIO ())
nameItem = 
  ssAddGraph <$> n3SymLex <*> (symbol ":-" *> graphOrList)
  
maybeURI :: N3Parser (Maybe URI)
maybeURI = (Just <$> lexUriRef) <|> return Nothing

--  @read name  [ <uri> ]
readGraph :: N3Parser (SwishStateIO ())
readGraph = commandName "@read" *> (ssRead <$> n3SymLex <*> maybeURI)

--  @write name [ <uri> ] ; Comment
writeGraph :: N3Parser (SwishStateIO ())
writeGraph =
        do  { commandName "@write"
            ; n <- n3SymLex
            ; let gs = ssGetList n :: SwishStateIO (Either String [RDFGraph])
            ; muri <- maybeURI
            ; ssWriteList muri gs <$> commentText
            }

--  @merge ( name* ) => name
mergeGraphs :: N3Parser (SwishStateIO ())
mergeGraphs = do
  commandName "@merge"
  gs <- graphList
  isymbol "=>"
  n <- n3SymLex
  return $ ssMerge n gs

-- @compare  name name
compareGraphs :: N3Parser (SwishStateIO ())
compareGraphs =
  commandName "@compare" *> (ssCompare <$> n3SymLex <*> n3SymLex)
  
-- @<command> name name ; Comment
assertArgs :: (ScopedName -> ScopedName -> String -> SwishStateIO ())
              -> String -> N3Parser (SwishStateIO ())
assertArgs assertFunc cName = do
  commandName $ '@':cName
  assertFunc <$> n3SymLex <*> n3SymLex <*> commentText
      
--  @asserteq name name ; Comment
assertEquiv :: N3Parser (SwishStateIO ())
assertEquiv = assertArgs ssAssertEq "asserteq" 
        
--  @assertin name name ; Comment              
assertMember :: N3Parser (SwishStateIO ())
assertMember = assertArgs ssAssertIn "assertin"
  
--  @rule name :- ( name* ) => name [ | ( (name var*)* ) ]               
defineRule :: N3Parser (SwishStateIO ())
defineRule =
        do  { commandName "@rule"
            ; rn <- n3SymLex
            ; setTo
            ; ags <- graphOrList
            ; isymbol "=>"
            ; cg  <- graphExpr
            ; vms <- varModifiers <|> pure []
            ; return $ ssDefineRule rn ags cg vms
            }

--  @ruleset name :- ( name* ) ; ( name* )
defineRuleset :: N3Parser (SwishStateIO ())
defineRuleset =
  commandName "@ruleset" *>      
  (ssDefineRuleset <$> n3SymLex <*> (setTo *> nameList) <*> (semicolon *> nameList))
  
--  @constraints pref :- ( name* ) | ( name* )
defineConstraints :: N3Parser (SwishStateIO ())
defineConstraints =
  commandName "@constraints" *>      
  (ssDefineConstraints <$> n3SymLex <*> (setTo *> graphOrList) <*> (symbol "|" *> nameOrList))
  
--  @proof name ( name* )
--    @input name
--    @step name ( name* ) => name  # rule-name, antecedents, consequent
--    @result name
checkProofCmd :: N3Parser (SwishStateIO ())
checkProofCmd =
        do  { commandName "@proof"
            ; pn  <- n3SymLex
            ; sns <- nameList
            ; commandName "@input"
            ; igf <- formulaExpr
            ; sts <- many checkStep
            ; commandName "@result"
            ; ssCheckProof pn sns igf sts <$> formulaExpr
            }

checkStep ::
    N3Parser (Either String [RDFRuleset]
                -> SwishStateIO (Either String RDFProofStep))
checkStep =
  commandName "@step" *>      
  (ssCheckStep <$> n3SymLex <*> formulaList <*> (symbol "=>" *> formulaExpr))

--  #   ruleset rule (antecedents) => result
--  @fwdchain pref name ( name* ) => name
fwdChain :: N3Parser (SwishStateIO ())
fwdChain =
        do  { commandName "@fwdchain"
            ; sn  <- n3SymLex
            ; rn  <- n3SymLex
            ; ags <- graphOrList
            ; isymbol "=>"
            ; cn  <- n3SymLex
            ; s <- stGet
            ; let prefs = prefixUris s
            ; return $ ssFwdChain sn rn ags cn prefs
            }

--  #   ruleset rule consequent <= (antecedent-alts)
--  @bwdchain pref name graph <= name
bwdChain :: N3Parser (SwishStateIO ())
bwdChain =
        do  { commandName "@bwdchain"
            ; sn  <- n3SymLex
            ; rn  <- n3SymLex
            ; cg  <- graphExpr
            ; isymbol "<="
            ; an  <- n3SymLex
            ; s <- stGet
            ; let prefs = prefixUris s
            ; return $ ssBwdChain sn rn cg an prefs
            }

----------------------------------------------------------------------
--  Syntax clause helpers
----------------------------------------------------------------------

-- TODO: is the loss of identLetter a problem?
commandName :: String -> N3Parser ()
-- commandName cmd = try (string cmd *> notFollowedBy identLetter *> whiteSpace)
commandName cmd = symbol cmd $> ()

restOfLine :: N3Parser String
restOfLine = manyTill (satisfy (const True)) eoln <* whiteSpace
  
br :: N3Parser a -> N3Parser a
br = between (symbol "(") (symbol ")")

nameList :: N3Parser [ScopedName]
nameList = br $ many n3SymLex
  
toList :: a -> [a]
toList = (:[])
           
nameOrList :: N3Parser [ScopedName]
nameOrList =
  (toList <$> n3SymLex)      
  <|> nameList
  
graphExpr :: N3Parser (SwishStateIO (Either String RDFGraph))
graphExpr =
        graphOnly
    <|>
        fmap (fmap formExpr) <$> formulaExpr

graphOnly :: N3Parser (SwishStateIO (Either String RDFGraph))
graphOnly =
        do  { isymbol "{"
            ; b <- newBlankNode
            ; g <- subgraph b
            ; isymbol "}"
            ; s <- stGet
            ; let gp = setNamespaces (prefixUris s) g
            ; return $ return (Right gp)
            }

graphList :: N3Parser [SwishStateIO (Either String RDFGraph)]
graphList = br (many graphExpr)

graphOrList :: N3Parser [SwishStateIO (Either String RDFGraph)]
graphOrList =
  (toList <$> graphExpr)
  <|> graphList

formulaExpr :: N3Parser (SwishStateIO (Either String RDFFormula))
formulaExpr = n3SymLex >>= namedGraph

namedGraph :: ScopedName -> N3Parser (SwishStateIO (Either String RDFFormula))
namedGraph n =
  (ssAddReturnFormula n <$> (setTo *> graphOnly))
  <|> return (ssGetFormula n)

formulaList :: N3Parser [SwishStateIO (Either String RDFFormula)]
formulaList = between (symbol "(") (symbol ")") (many formulaExpr)

varModifiers :: N3Parser [(ScopedName,[RDFLabel])]
varModifiers = symbol "|" *> varModList

varModList :: N3Parser [(ScopedName,[RDFLabel])]
varModList = 
  br (sepBy varMod comma)
  <|> toList <$> lexeme varMod

varMod :: N3Parser (ScopedName,[RDFLabel])
varMod = (,) <$> n3SymLex <*> many (lexeme quickVariable)

----------------------------------------------------------------------
--  SwishState helper functions
----------------------------------------------------------------------
--
--  The functions below operate in the SwishStateIO monad, and are used
--  to assemble an executable version of the parsed script.

-- | Return a message to the user. At present the message begins with '# '
-- but this may be removed.
--
ssReport :: 
  String  -- ^ message contents
  -> SwishStateIO ()
-- ssReport msg = lift $ putStrLn $ "# " ++ msg
ssReport msg = modify $ setInfo $ "# " ++ msg

ssReportLabel :: 
  String     -- ^ label for the message
  -> String  -- ^ message contents
  -> SwishStateIO ()
ssReportLabel lbl msg = ssReport $ lbl ++ ": " ++ msg

ssAddReturnFormula ::
    ScopedName -> SwishStateIO (Either String RDFGraph)
    -> SwishStateIO (Either String RDFFormula)
ssAddReturnFormula nam gf =
        do  { egr <- gf
            ; ssAddGraph nam [return egr]
            ; return $ fmap (Formula nam) egr
            }

ssAddGraph ::
    ScopedName -> [SwishStateIO (Either String RDFGraph)]
    -> SwishStateIO ()
ssAddGraph nam gf =
    let errmsg = "Graph/list not added: " ++ show nam ++ "; "
    in
        do  { esg <- sequence gf        -- [Either String RDFGraph]
            ; let egs = sequence esg    -- Either String [RDFGraph]
            ; let fgs = case egs of
                    Left  er -> setError  (errmsg ++ er)
                    Right gs -> modGraphs (M.insert nam gs)
            ; modify fgs
            }

ssGetGraph :: ScopedName -> SwishStateIO (Either String RDFGraph)
ssGetGraph nam = do
  grs <- ssGetList nam
  pure $ case grs of
           Left emsg -> Left emsg
           -- Does it make sense to allow the empty graph?
           Right [] -> Left ("Graph or list is empty: " ++ show nam)
           Right (gr:_) -> Right gr
  
ssGetFormula :: ScopedName -> SwishStateIO (Either String RDFFormula)
ssGetFormula nam = gets find
    where
        find st = case findFormula nam st of
            Nothing -> Left ("Formula not present: " ++ show nam)
            Just gr -> Right gr

ssGetList :: ScopedName -> SwishStateIO (Either String [RDFGraph])
ssGetList nam = gets find
    where
        find st = case findGraph nam st of
            Nothing  -> Left ("Graph or list not present: " ++ show nam)
            Just grs -> Right grs

ssRead :: ScopedName -> Maybe URI -> SwishStateIO ()
ssRead nam muri = ssAddGraph nam [ssReadGraph muri]

ssReadGraph :: Maybe URI -> SwishStateIO (Either String RDFGraph)
ssReadGraph muri = 
  let gf inp = case inp of
        Left  es -> Left es
        Right is -> parseN3 is (muri >>= qnameFromURI)
        
  in gf `fmap` getResourceData muri

ssWriteList ::
    Maybe URI -> SwishStateIO (Either String [RDFGraph]) -> String
    -> SwishStateIO ()
ssWriteList muri gf comment = do
  esgs <- gf
  case esgs of
    Left  er   -> modify $ setError ("Cannot write list: " ++ er)
    Right []   -> putResourceData Nothing (B.fromLazyText (L.concat ["# ", L.pack comment, "\n+ Swish: Writing empty list"]))
    Right [gr] -> ssWriteGraph muri gr comment
    Right grs  -> mapM_ writegr (zip [(0::Int)..] grs)
      where
        writegr (n,gr) = ssWriteGraph (murin muri n) gr
                         ("[" ++ show n ++ "] " ++ comment)
        murin Nothing    _ = Nothing
        murin (Just uri) n = 
          let rp = reverse $ uriPath uri
              (rLastSet, rRest) = break (== '/') rp
              (before, after) = break (== '.') $ reverse rLastSet
              newPath = reverse rRest ++ "/" ++ before ++ show n ++ after
          in case rLastSet of
            "" -> error $ "Invalid URI (path ends in /): " ++ show uri
            _ -> Just $ uri { uriPath = newPath }
         
  

{-
ssWrite ::
    Maybe String -> SwishStateIO (Either String RDFGraph) -> String
    -> SwishStateIO ()
ssWrite muri gf comment =
        do  { esg <- gf
            ; case esg of
                Left  er -> modify $ setError ("Cannot write graph: "++er)
                Right gr -> ssWriteGraph muri gr comment
            }
-}

ssWriteGraph :: Maybe URI -> RDFGraph -> String -> SwishStateIO ()
ssWriteGraph muri gr comment =
    putResourceData muri (c `mappend` formatGraphAsBuilder gr)
    where
        c = B.fromLazyText $ L.concat ["# ", L.pack comment, "\n"]

ssMerge ::
    ScopedName -> [SwishStateIO (Either String RDFGraph)]
    -> SwishStateIO ()
ssMerge nam gfs =
    let errmsg = "Graph merge not defined: " ++ show nam ++ "; "
    in
        do  { ssReportLabel "Merge" (show nam)
            ; esg <- sequence gfs       -- [Either String RDFGraph]
            ; let egs = sequence esg    -- Either String [RDFGraph]
            ; let fgs = case egs of
                    Left  er -> setError  (errmsg ++ er)
                    Right [] -> setError  (errmsg ++ "No graphs to merge")
                    Right gs -> modGraphs (M.insert nam [g])
                            where g = foldl1 merge gs
            ; modify fgs
            }

ssCompare :: ScopedName -> ScopedName -> SwishStateIO ()
ssCompare n1 n2 =
        do  { ssReportLabel "Compare" (show n1 ++ " " ++ show n2)
            ; g1 <- ssGetGraph n1
            ; g2 <- ssGetGraph n2
            ; when (g1 /= g2) (modify $ setStatus SwishGraphCompareError)
            }

ssAssertEq :: ScopedName -> ScopedName -> String -> SwishStateIO ()
ssAssertEq n1 n2 comment =
    let er1 = ":\n  Graph or list compare not performed:  invalid graph/list."
    in
        do  { ssReportLabel "AssertEq" comment
            ; g1 <- ssGetList n1
            ; g2 <- ssGetList n2
            ; case (g1,g2) of
                (Left er,_) -> modify $ setError (comment ++ er1 ++ "\n  " ++ er)
                (_,Left er) -> modify $ setError (comment ++ er1 ++ "\n  " ++ er)
                (Right gr1,Right gr2) -> 
                    when (S.fromList gr1 /= S.fromList gr2) $ modify $
                      setError (comment ++ ":\n  Graph " ++ show n1
                                ++ " differs from " ++ show n2 ++ ".")
            }

ssAssertIn :: ScopedName -> ScopedName -> String -> SwishStateIO ()
ssAssertIn n1 n2 comment =
    let er1 = ":\n  Membership test not performed:  invalid graph."
        er2 = ":\n  Membership test not performed:  invalid list."
    in
        do  { ssReportLabel "AssertIn" comment
            ; g1 <- ssGetGraph n1
            ; g2 <- ssGetList  n2
            ; case (g1,g2) of
                (Left er,_) -> modify $ setError (comment ++ er1 ++ "\n  " ++ er)
                (_,Left er) -> modify $ setError (comment ++ er2 ++ "\n  " ++ er)
                (Right gr,Right gs) ->
                    unless (gr `elem` gs) $ modify $
                    setError (comment ++ ":\n  Graph " ++ show n1
                              ++ " not a member of " ++ show n2)
            }

--  Note:  this is probably incomplete, though it should work in simple cases.
--  A complete solution would have the binding modifiers subject to
--  re-arrangement to suit the actual bound variables encountered.
--  See VarBinding.findCompositions and VarBinding.findComposition
--
--  This code should be adequate if variable bindings are always used
--  in combinations consisting of a single modifier followed by any number
--  of filters.
--
ssDefineRule ::
    ScopedName
    -> [SwishStateIO (Either String RDFGraph)]
    -> SwishStateIO (Either String RDFGraph)
    -> [(ScopedName,[RDFLabel])]
    -> SwishStateIO ()
ssDefineRule rn agfs cgf vmds =
    let errmsg1 = "Rule definition error in antecedent graph(s): "
        errmsg2 = "Rule definition error in consequent graph: "
        errmsg3 = "Rule definition error in variable modifier(s): "
        errmsg4 = "Incompatible variable binding modifier sequence"
    in
        do  { aesg <- sequence agfs     -- [Either String RDFGraph]
            ; let ags = sequence aesg   :: Either String [RDFGraph]
            ; cg <- cgf                 -- Either String RDFGraph
            ; let vmfs = map ssFindVarModify vmds
            ; evms <- sequence vmfs     -- [Either String RDFVarBindingModify]
            ; let vms = sequence evms   :: Either String [RDFVarBindingModify]
            ; let frl = case (ags,cg,vms) of
                    (Left er,_,_) -> setError (errmsg1 ++ er)
                    (_,Left er,_) -> setError (errmsg2 ++ er)
                    (_,_,Left er) -> setError (errmsg3 ++ er)
                    (Right agrs,Right cgr,Right vbms) ->
                        let
                            newRule = makeRDFClosureRule rn agrs cgr
                        in
                        case composeSequence vbms of
                            Just vm -> let nr = newRule vm in modRules (M.insert (ruleName nr) nr)
                            Nothing -> setError errmsg4
            ; modify frl
            }

ssFindVarModify ::
    (ScopedName,[RDFLabel]) -> SwishStateIO (Either String RDFVarBindingModify)
ssFindVarModify (nam,lbs) = gets $ \st ->
  case findOpenVarModify nam st of
    Just ovbm -> Right (ovbm lbs)
    Nothing   -> Left  ("Undefined modifier: " ++ show nam)

ssDefineRuleset ::
    ScopedName
    -> [ScopedName]
    -> [ScopedName]
    -> SwishStateIO ()
ssDefineRuleset sn ans rns =
    let errmsg1 = "Error in ruleset axiom(s): "
        errmsg2 = "Error in ruleset rule(s): "
    in
        do  { let agfs = mapM ssGetFormula ans
                                        :: SwishStateIO [Either String RDFFormula]
            ; aesg <- agfs              -- [Either String RDFFormula]
            ; let eags = sequence aesg  :: Either String [RDFFormula]
            ; let erlf = mapM ssFindRule rns
                                        :: SwishStateIO [Either String RDFRule]
            ; rles <- erlf              -- [Either String RDFRule]
            ; let erls = sequence rles  :: Either String [RDFRule]
            ; let frs = case (eags,erls) of
                    (Left er,_) -> setError (errmsg1 ++ er)
                    (_,Left er) -> setError (errmsg2 ++ er)
                    (Right ags,Right rls) ->
                        modRulesets (M.insert (getRulesetNamespace rs) rs)
                        where
                            rs = makeRuleset (getScopeNamespace sn) ags rls
            ; modify frs
            }

ssFindRule :: ScopedName -> SwishStateIO (Either String RDFRule)
ssFindRule nam = gets find
    where
        find st = case findRule nam st of
            Nothing -> Left ("Rule not found: " ++ show nam)
            Just rl -> Right rl

ssDefineConstraints  ::
    ScopedName
    -> [SwishStateIO (Either String RDFGraph)]
    -> [ScopedName]
    -> SwishStateIO ()
ssDefineConstraints  sn cgfs dtns =
    let errmsg1 = "Error in constraint graph(s): "
        errmsg2 = "Error in datatype(s): "
    in
        do  { cges <- sequence cgfs     -- [Either String RDFGraph]
            ; let ecgs = sequence cges  :: Either String [RDFGraph]
            ; let ecgr = case ecgs of
                    Left er   -> Left er
                    Right []  -> Right mempty
                    Right grs -> Right $ foldl1 merge grs
            ; edtf <- mapM ssFindDatatype dtns
                                        -- [Either String RDFDatatype]
            ; let edts = sequence edtf   :: Either String [RDFDatatype]
            ; let frs = case (ecgr,edts) of
                    (Left er,_) -> setError (errmsg1 ++ er)
                    (_,Left er) -> setError (errmsg2 ++ er)
                    (Right cgr,Right dts) ->
                        modRulesets (M.insert (getRulesetNamespace rs) rs)
                        where
                            rs  = makeRuleset (getScopeNamespace sn) [] rls
                            rls = concatMap (`typeMkRules` cgr) dts
            ; modify frs
            }

ssFindDatatype :: ScopedName -> SwishStateIO (Either String RDFDatatype)
ssFindDatatype nam = gets find
    where
        find st = case findDatatype nam st of
            Nothing -> Left ("Datatype not found: " ++ show nam)
            Just dt -> Right dt


ssCheckProof ::
    ScopedName                                      -- proof name
    -> [ScopedName]                                 -- ruleset names
    -> SwishStateIO (Either String RDFFormula)      -- input formula
    -> [Either String [RDFRuleset]                  -- proof step from rulesets
        -> SwishStateIO (Either String RDFProofStep)]
    -> SwishStateIO (Either String RDFFormula)      -- result formula
    -> SwishStateIO ()
ssCheckProof pn sns igf stfs rgf =
    let
        infmsg1 = "Proof satisfied: "
        errmsg1 = "Error in proof ruleset(s): "
        errmsg2 = "Error in proof input: "
        errmsg3 = "Error in proof step(s): "
        errmsg4 = "Error in proof goal: "
        errmsg5 = "Proof not satisfied: "
        proofname = " (Proof " ++ show pn ++ ")"
    in
        do  { let rs1 = map ssFindRuleset sns       :: [SwishStateIO (Either String RDFRuleset)]
            ; rs2 <- sequence rs1                   -- [Either String RDFRuleset]
            ; let erss = sequence rs2               :: Either String [RDFRuleset]
            ; eig <- igf                            -- Either String RDFFormula
            ; let st1  = sequence $ flist stfs erss :: SwishStateIO [Either String RDFProofStep]
            ; st2 <- st1                            -- [Either String RDFProofStep]
            ; let ests = sequence st2               :: Either String [RDFProofStep]
            ; erg  <- rgf                           -- Either String RDFFormula
            ; let proof = case (erss,eig,ests,erg) of
                    (Left er,_,_,_) -> Left (errmsg1 ++ er ++ proofname)
                    (_,Left er,_,_) -> Left (errmsg2 ++ er ++ proofname)
                    (_,_,Left er,_) -> Left (errmsg3 ++ er ++ proofname)
                    (_,_,_,Left er) -> Left (errmsg4 ++ er ++ proofname)
                    (Right rss, Right ig, Right sts, Right rg) ->
                        Right (makeRDFProof rss ig rg sts)
            ; when False $ case proof of
                    (Left  _)  -> return ()
                    (Right pr) -> putResourceData Nothing $
                                    B.fromLazyText (L.concat ["Proof ", L.pack (show pn), "\n"])
                                    `mappend`
                                    B.fromString (showsProof "\n" pr "\n")
                                    -- TODO: clean up
            ; let checkproof = case proof of
                    (Left  er) -> setError er
                    (Right pr) ->
                        case explainProof pr of
                            Nothing -> setInfo (infmsg1 ++ show pn)
                            Just ex -> setError (errmsg5 ++ show pn ++ ", " ++ ex)
                        {-
                        if not $ checkProof pr then
                            setError (errmsg5++show pn)
                        else
                            setInfo (infmsg1++show pn)
                        -}
            ; modify checkproof
            }

ssCheckStep ::
    ScopedName                                      -- rule name
    -> [SwishStateIO (Either String RDFFormula)]    -- antecedent graph formulae
    -> SwishStateIO (Either String RDFFormula)      -- consequent graph formula
    -> Either String [RDFRuleset]                   -- rulesets
    -> SwishStateIO (Either String RDFProofStep)    -- resulting proof step
ssCheckStep _  _    _    (Left  er)  = return $ Left er
ssCheckStep rn eagf ecgf (Right rss) =
    let
        errmsg1 = "Rule not in proof step ruleset(s): "
        errmsg2 = "Error in proof step antecedent graph(s): "
        errmsg3 = "Error in proof step consequent graph: "
    in
        do  { let mrul = getMaybeContextRule rn rss :: Maybe RDFRule
            ; esag <- sequence eagf                 -- [Either String RDFFormula]]
            ; let eags = sequence esag              :: Either String [RDFFormula]
            ; ecg  <- ecgf                          -- Either String RDFFormula
            ; let est = case (mrul,eags,ecg) of
                    (Nothing,_,_) -> Left (errmsg1 ++ show rn)
                    (_,Left er,_) -> Left (errmsg2 ++ er)
                    (_,_,Left er) -> Left (errmsg3 ++ er)
                    (Just rul,Right ags,Right cg) ->
                        Right $ makeRDFProofStep rul ags cg
            ; return est
            }

ssFwdChain ::
    ScopedName                                      -- ruleset name
    -> ScopedName                                   -- rule name
    -> [SwishStateIO (Either String RDFGraph)]      -- antecedent graphs
    -> ScopedName                                   -- consequent graph name
    -> NamespaceMap                                 -- prefixes for new graph
    -> SwishStateIO ()
ssFwdChain sn rn agfs cn prefs =
    let
        errmsg1 = "FwdChain rule error: "
        errmsg2 = "FwdChain antecedent error: "
    in
        do  { erl  <- ssFindRulesetRule sn rn
            ; aesg <- sequence agfs     -- [Either String RDFGraph]
            ; let eags = sequence aesg   :: Either String [RDFGraph]
            ; let fcr = case (erl,eags) of
                    (Left er,_) -> setError (errmsg1 ++ er)
                    (_,Left er) -> setError (errmsg2 ++ er)
                    (Right rl,Right ags) ->
                        modGraphs (M.insert cn [cg])
                        where
                            cg = case fwdApply rl ags of
                                []  -> mempty
                                grs -> setNamespaces prefs $ foldl1 addGraphs grs
            ; modify fcr
            }

ssFindRulesetRule ::
    ScopedName -> ScopedName -> SwishStateIO (Either String RDFRule)
ssFindRulesetRule sn rn = gets find
    where
        find st = case findRuleset sn st of
            Nothing -> Left ("Ruleset not found: " ++ show sn)
            Just rs -> find1 rs
        find1 rs = case getRulesetRule rn rs of
            Nothing -> Left ("Rule not in ruleset: " ++ show sn ++ ": " ++ show rn)
            Just rl -> Right rl

ssFindRuleset ::
    ScopedName -> SwishStateIO (Either String RDFRuleset)
ssFindRuleset sn = gets find
    where
        find st = case findRuleset sn st of
            Nothing -> Left ("Ruleset not found: " ++ show sn)
            Just rs -> Right rs

ssBwdChain ::
    ScopedName                                      -- ruleset name
    -> ScopedName                                   -- rule name
    -> SwishStateIO (Either String RDFGraph)        -- consequent graphs
    -> ScopedName                                   -- antecedent alts name
    -> NamespaceMap                                 -- prefixes for new graphs
    -> SwishStateIO ()
ssBwdChain sn rn cgf an prefs =
    let
        errmsg1 = "BwdChain rule error: "
        errmsg2 = "BwdChain goal error: "
    in
        do  { erl <- ssFindRulesetRule sn rn
            ; ecg <- cgf                -- Either String RDFGraph
            ; let fcr = case (erl,ecg) of
                    (Left er,_) -> setError (errmsg1 ++ er)
                    (_,Left er) -> setError (errmsg2 ++ er)
                    (Right rl,Right cg) ->
                        modGraphs (M.insert an ags)
                        where
                            ags  = map mergegr (bwdApply rl cg)
                            mergegr grs = case grs of
                                [] -> mempty
                                _  -> setNamespaces prefs $ foldl1 addGraphs grs
            ; modify fcr
            }

--  Temporary implementation:  just read local file WNH     
--  (Add logic to separate filenames from URIs, and
--  attempt HTTP GET, or similar.)
getResourceData :: Maybe URI -> SwishStateIO (Either String L.Text)
getResourceData = maybe fromStdin fromUri 
  where
    fromStdin = Right <$> lift LIO.getContents
    fromUri = fromFile
    fromFile uri | uriScheme uri == "file:" = Right `fmap` lift (LIO.readFile $ uriPath uri)
                 | otherwise = error $ "Unsupported file name for read: " ++ show uri
                               
--  Temporary implementation:  just write local file
--  (Need to add logic to separate filenames from URIs, and
--  attempt HTTP PUT, or similar.)
putResourceData :: Maybe URI -> B.Builder -> SwishStateIO ()
putResourceData muri gsh = do
    ios <- lift . CE.try $ maybe toStdout toUri muri
    case ios of
      Left ioe -> modify $ setError
                    ("Error writing graph: " ++
                    IO.ioeGetErrorString ioe)
      Right _   -> return ()

    where
        toStdout  = LIO.putStrLn gstr
        toUri uri | uriScheme uri == "file:" = LIO.writeFile (uriPath uri) gstr
                  | otherwise                = error $ "Unsupported scheme for write: " ++ show uri
        gstr = B.toLazyText gsh

{- $syntax

The script syntax is based loosely on Notation3, and the script parser is an
extension of the Notation3 parser in the module "Swish.RDF.Parser.N3".
The comment character is @#@ and white space is ignored.

> script            := command *
> command           := prefixLine        |
>                      nameItem          |
>                      readGraph         |
>                      writeGraph        |
>                      mergeGraphs       |
>                      compareGraphs     |
>                      assertEquiv       |
>                      assertMember      |
>                      defineRule        |
>                      defineRuleset     |
>                      defineConstraints |
>                      checkProofCmd     |
>                      fwdChain          |
>                      bwdChain 

-}

{- $prefixLine

> prefixLine        := @prefix [<prefix>]: <uri> .

Define a namespace prefix and URI. 

The prefix thus defined is available for use in any subsequent script
command, and also in any graphs contained within the script file. (So,
prefix declarations do not need to be repeated for each graph
contained within the script.)

Graphs read from external files must contain their own prefix
declarations.

Example:

  > @prefix gex: <http://example1.com/graphs/>.
  > @prefix :    <http://example2.com/id/>.

-}

{- $nameItem

> nameItem          := name :- graph     |
>                      name :- ( graph* )

Graphs or lists of graphs can be given a name for use in other
statements.  A name is a qname (prefix:local) or a URI enclosed in
angle

Example:

> @prefix ex1: <http://example.com/graphs/> .
> @prefix ex2: <http://example.com/statements/> .
>
> ex1:gr1 :- { 
>     ex2:foo a ex2:Foo .
>     ex2:bar a ex2:Bar .
>     ex2:Bar rdfs:subClassOf ex2:Foo .
> }

-}

{- $readGraph

> readGraph         := @read name [<uri>]

The @\@read@ command reads in the contents of the given URI
- which at present only supports reading local files, so
no HTTP access - and stores it under the given name.

If no URI is given then the file is read from standard input.

Example:

  > @prefix ex: <http://example.com/> .
  > @read ex:foo <foo.n3>

-}

{- $writeGraph

> writeGraph        := @write name [<uri>] ; comment

The @\@write@ command writes out the contents of the given graph
- which at present only supports writing local files, so
no HTTP access. The comment text is written as a comment line
preceeding the graph contents.

If no URI is given then the file is written to the standard output.

Example:

  > @prefix ex: <http://example.com/> .
  > @read ex:gr1 <graph1.n3>
  > @read ex:gr2 <graph2.n3>
  > @merge (ex:gr1 ex:gr2) => ex:gr3
  > @write ex:gr3 ; the merged data
  > @write ex:gr3 <merged.n3> ; merge of graph1.n3 and graph2.n3

-}

{- $mergeGraphs

> mergeGraphs       := @merge ( name* ) => name

Create a new named graph that is the merge two or more graphs,
renaming bnodes as required to avoid node-merging.

When the merge command is run, the message

  > # Merge: <output graph name>

will be created on the standard output channel.

Example:

  > @prefix gex: <http://example.com/graph/>.
  > @prefix ex: <http://example.com/statements/>.
  > gex:gr1 :- { ex:foo ex:bar _:b1 . }
  > gex:gr2 :- { _:b1 ex:foobar 23. }
  > @merge (gex:gr1 gex:gr2) => gex:gr3
  > @write gex:gr3 ; merged graphs

When run in Swish, this creates the following output (along with
several other namespace declarations):

 > # merged graphs
 > @prefix ex: <http://example.com/statements/> .
 > ex:foo ex:bar [] .
 > [
 >  ex:foobar "23"^^xsd:integer
 > ] .

-}

{- $compareGraphs

> compareGraphs     := @compare name name

Compare two graphs for isomorphism, setting the Swish exit status to
reflect the result.

When the compare command is run, the message

  > # Compare: <graph1> <graph2>

will be created on the standard output channel.

Example:

  > @prefix gex: <http://example.com/graphs/>.
  > @read gex:gr1 <graph1.n3>
  > @read gex:gr2 <graph2.n3>
  > @compare gex:gr1 gex:gr2

-}

{- $assertEquiv

> assertEquiv       := @asserteq name name ; comment

Test two graphs or lists of graphs for isomorphism, reporting if they
differ. The comment text is included with any report generated.

When the command is run, the message

  > # AssertEq: <comment>

will be created on the standard output channel.

Example:

  > @prefix ex:  <http://id.ninebynine.org/wip/2003/swishtest/> .
  >
  > # Set up the graphs for the rules
  > ex:Rule01Ant :- { ?p ex:son ?o . }
  > ex:Rule01Con :- { ?o a ex:Male ; ex:parent ?p . }
  >
  > # Create a rule and a ruleset
  > @rule ex:Rule01 :- ( ex:Rule01Ant ) => ex:Rule01Con
  > @ruleset ex:rules :- (ex:TomSonDick ex:TomSonHarry) ; (ex:Rule01)
  >
  > # Apply the rule
  > @fwdchain ex:rules ex:Rule01 { :Tom ex:son :Charles . } => ex:Rule01fwd
  >
  > # Compare the results to the expected value
  > ex:ExpectedRule01fwd :- { :Charles a ex:Male ; ex:parent :Tom . }  
  > @asserteq ex:Rule01fwd ex:ExpectedRule01fwd
  >    ; Infer that Charles is male and has parent Tom

-}

{- $assertMember

> assertMember      := @assertin name name ; comment

Test if a graph is isomorphic to a member of a list of graphs,
reporting if no match is found. The comment text is included with any
report generated.

Example:

> @bwdchain pv:rules :PassengerVehicle ex:Test01Inp <= :t1b
> 
> @assertin ex:Test01Bwd0 :t1b ; Backward chain component test (0)
> @assertin ex:Test01Bwd1 :t1b ; Backward chain component test (1)

-}

{- $defineRule

> defineRule        := @rule name :- ( name* ) => name
> defineRule        := @rule name :- ( name* ) => name
>                       | ( (name var*)* )

Define a named Horn-style rule. 

The list of names preceding and following @=>@ are the antecedent and consequent
graphs, respectivelu. Both sets may contain variable nodes of the form 
@?var@.

The optional part, after the @|@ separator, is a list of variable
binding modifiers, each of which consists of a name and a list of
variables (@?var@) to which the modifier is applied. Variable binding
modifiers are built in to Swish, and are used to incorporate datatype
value inferences into a rule.  

-}

{- $defineRuleset

> defineRuleset     := @ruleset name :- ( name* ) ; ( name* ) 

Define a named ruleset (a collection of axioms and rules). The first
list of names are the axioms that are part of the ruleset, and the
second list are the rules.

-}

{- $defineConstraints

> defineConstraints := @constraints pref :- ( name* ) | [ name | ( name* ) ]

Define a named ruleset containing class-restriction rules based on a
datatype value constraint. The first list of
names is a list of graphs that together comprise the class-restriction
definitions (rule names are the names of the corresponding restriction
classes). The second list of names is a list of datatypes whose
datatype relations are referenced by the class restriction
definitions.

-}

{- $fwdChain

> fwdChain          := @fwdchain pref name ( name* ) => name

Define a new graph obtained by forward-chaining a rule. The first name
is the ruleset to be used. The second name is the rule name. The list
of names are the antecedent graphs to which the rule is applied. The
name following the @=>@ names a new graph that is the result of formward
chaining from the given antecedents using the indicated rule.

-}

{- $bwdChain

> bwdChain          := @bwdchain pref name graph <= name

Define a new list of alternative graphs obtained by backward-chaining
a rule. The first name is the ruleset to be used. The second name is
the rule name. The third name (before the @<=@) is the name of a goal graph
from which to backward chain. The final name (after the @<=@) names a new
list of graphs, each of which is an alternative antecedent from which
the given goal can be deduced using the indicated rule.


-}

{- $proof

> checkProofCmd     := proofLine nl
>                      inputLine nl
>                      (stepLine nl)*
>                      resultLine
> proofLine         := @proof name ( name* )

Check a proof, reporting the step that fails, if any.

The @\@proof@ line names the proof and specifies a list rulesets
(proof context) used.  The remaining lines specify the input
expression (@\@input@), proof steps (@\@step@) and the final result
(@\@result@) that is demonstrated by the proof.

> inputLine         := @input name

In a proof, indicates an input expression upon which the proof is
based. Exactly one of these immediately follows the @\@proof@ command.

> stepLine          := @step name ( name* ) => name

This defines a step of the proof; any number of these immediately
follow the @\@input@ command.

It indicates the name of the rule applied for this step, a list of
antecedent graphs, and a named graph that is deduced by this step.
For convenience, the deduced graph may introduce a new named graph
using an expression of the form:

  > name :- { statements }

> resultLine        := @result name

This defines the goal of the proof, and completes a proof
definition. Exactly one of these immediately follows the @\@step@
commands.  For convenience, the result statement may introduce a new
named graph using an expression of the form:

  > name :- { statements }

-}

{- $exampleScript

This is the example script taken from
<http://www.ninebynine.org/Software/swish-0.2.1.html#sec-script-example>
with the proof step adjusted so that it passes.

> # -- Example Swish script --
> #
> # Comment lines start with a '#'
> #
> # The script syntax is loosely based on Notation3, but it is a quite 
> # different language, except that embedded graphs (enclosed in {...})
> # are encoded using Notation3 syntax.
> #
> # -- Prefix declarations --
> #
> # As well as being used for all labels defined and used by the script
> # itself, these are applied to all graph expressions within the script 
> # file, and to graphs created by scripted inferences, 
> # but are not applied to any graphs read in from an external source.
> 
> @prefix ex:  <http://id.ninebynine.org/wip/2003/swishtest/> .
> @prefix pv:  <http://id.ninebynine.org/wip/2003/swishtest/pv/> .
> @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
> @prefix xsd_integer: <http://id.ninebynine.org/2003/XMLSchema/integer#> .
> @prefix rs_rdf:  <http://id.ninebynine.org/2003/Ruleset/rdf#> .
> @prefix rs_rdfs: <http://id.ninebynine.org/2003/Ruleset/rdfs#> .
> @prefix :   <http://id.ninebynine.org/default/> .
> 
> # Additionally, prefix declarations are provided automatically for:
> #    @prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
> #    @prefix rdfs:  <file://www.w3.org/2000/01/rdf-schema#> .
> #    @prefix rdfd:  <http://id.ninebynine.org/2003/rdfext/rdfd#> .
> #    @prefix rdfo:  <http://id.ninebynine.org/2003/rdfext/rdfo#> .
> #    @prefix owl:   <http://www.w3.org/2002/07/owl#> .
> 
> # -- Simple named graph declarations --
> 
> ex:Rule01Ant :- { ?p ex:son ?o . }
> 
> ex:Rule01Con :- { ?o a ex:Male ; ex:parent ?p . }
> 
> ex:TomSonDick :- { :Tom ex:son :Dick . }
> ex:TomSonHarry :- { :Tom ex:son :Harry . }
> 
> # -- Named rule definition --
> 
> @rule ex:Rule01 :- ( ex:Rule01Ant ) => ex:Rule01Con
> 
> # -- Named ruleset definition --
> #
> # A 'ruleset' is a collection of axioms and rules.
> #
> # Currently, the ruleset is identified using the namespace alone;
> # i.e. the 'rules' in 'ex:rules' below is not used.  
> # This is under review.
> 
> @ruleset ex:rules :- (ex:TomSonDick ex:TomSonHarry) ; (ex:Rule01)
> 
> # -- Forward application of rule --
> #
> # The rule is identified here by ruleset and a name within the ruleset.
> 
> @fwdchain ex:rules ex:Rule01 { :Tom ex:son :Charles . } => ex:Rule01fwd
> 
> # -- Compare graphs --
> #
> # Compare result of inference with expected result.
> # This is a graph isomorphism test rather than strict equality, 
> # to allow for bnode renaming.
> # If the graphs are not equal, a message is generated, which
> # includes the comment (';' to end of line)
> 
> ex:ExpectedRule01fwd :- { :Charles a ex:Male ; ex:parent :Tom . }  
> 
> @asserteq ex:Rule01fwd ex:ExpectedRule01fwd
>    ; Infer that Charles is male and has parent Tom
> 
> # -- Display graph (to screen and a file) --
> #
> # The comment is included in the output.
> 
> @write ex:Rule01fwd ; Charles is male and has parent Tom
> @write ex:Rule01fwd <Example1.n3> ; Charles is male and has parent Tom
> 
> # -- Read graph from file --
> #
> # Creates a new named graph in the Swish environment.
> 
> @read ex:Rule01inp <Example1.n3>
> 
> # -- Proof check --
> #
> # This proof uses the built-in RDF and RDFS rulesets, 
> # which are the RDF- and RDFS- entailment rules described in the RDF
> # formal semantics document.
> #
> # To prove:
> #     ex:foo ex:prop "a" .
> # RDFS-entails
> #     ex:foo ex:prop _:x .
> #     _:x rdf:type rdfs:Resource .
> #
> # If the proof is not valid according to the axioms and rules of the 
> # ruleset(s) used and antecedents given, then an error is reported 
> # indicating the failed proof step.
> 
> ex:Input  :- { ex:foo ex:prop "a" . }
> ex:Result :- { ex:foo ex:prop _:a . _:a rdf:type rdfs:Resource . }
> 
> @proof ex:Proof ( rs_rdf:rules rs_rdfs:rules )
>   @input  ex:Input
>   @step   rs_rdfs:r3 ( rs_rdfs:a10 rs_rdfs:a39 )
>           => ex:Stepa :- { rdfs:Literal rdf:type rdfs:Class . }
>   @step   rs_rdfs:r8 ( ex:Stepa )
>           => ex:Stepb :- { rdfs:Literal rdfs:subClassOf rdfs:Resource . }
>   @step   rs_rdf:lg ( ex:Input )
>           => ex:Stepc :- { ex:foo ex:prop _:a . _:a rdf:_allocatedTo "a" . }
>   @step   rs_rdfs:r1 ( ex:Stepc )
>           => ex:Stepd :- { _:a rdf:type rdfs:Literal . }
>   @step   rs_rdfs:r9 ( ex:Stepb ex:Stepd )
>           => ex:Stepe :- { _:a rdf:type rdfs:Resource . }
>   @step   rs_rdf:se  ( ex:Stepc ex:Stepd ex:Stepe )
>           => ex:Result
>   @result ex:Result
> 
> # -- Restriction based datatype inferencing --
> #
> # Datatype inferencing based on a general class restriction and
> # a predefined relation (per idea noted by Pan and Horrocks).
> 
> ex:VehicleRule :-
>   { :PassengerVehicle a rdfd:GeneralRestriction ;
>       rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ;
>       rdfd:constraint xsd_integer:sum ;
>       rdfd:maxCardinality "1"^^xsd:nonNegativeInteger . }
> 
> # Define a new ruleset based on a declaration of a constraint class
> # and reference to built-in datatype.
> # The datatype constraint xsd_integer:sum is part of the definition 
> # of datatype xsd:integer that is cited in the constraint ruleset
> # declaration.  It relates named properties of a class instance.
> 
> @constraints pv:rules :- ( ex:VehicleRule ) | xsd:integer
> 
> # Input data for test cases:
> 
> ex:Test01Inp :-
>   { _:a1 a :PassengerVehicle ;
>       :seatedCapacity "30"^^xsd:integer ;
>       :standingCapacity "20"^^xsd:integer . }
> 
> # Forward chaining test case:
> 
> ex:Test01Fwd :- { _:a1 :totalCapacity "50"^^xsd:integer . }
> 
> @fwdchain pv:rules :PassengerVehicle ex:Test01Inp => :t1f
> @asserteq :t1f ex:Test01Fwd  ; Forward chain test
> 
> # Backward chaining test case:
> #
> # Note that the result of backward chaining is a list of alternatives,
> # any one of which is sufficient to derive the given conclusion.
> 
> ex:Test01Bwd0 :-
>   { _:a1 a :PassengerVehicle .
>     _:a1 :totalCapacity "50"^^xsd:integer .
>     _:a1 :seatedCapacity "30"^^xsd:integer . }
> 
> ex:Test01Bwd1 :-
>   { _:a1 a :PassengerVehicle .
>     _:a1 :totalCapacity "50"^^xsd:integer .
>     _:a1 :standingCapacity "20"^^xsd:integer . }
> 
> # Declare list of graphs:
> 
> ex:Test01Bwd :- ( ex:Test01Bwd0 ex:Test01Bwd1 )
> 
> @bwdchain pv:rules :PassengerVehicle ex:Test01Inp <= :t1b
> @asserteq :t1b ex:Test01Bwd  ; Backward chain test
> 
> # Can test for graph membership in a list
> 
> @assertin ex:Test01Bwd0 :t1b ; Backward chain component test (0)
> @assertin ex:Test01Bwd1 :t1b ; Backward chain component test (1)
> 
> # -- Merge graphs --
> #
> # Merging renames bnodes to avoid collisions.
> 
> @merge ( ex:Test01Bwd0 ex:Test01Bwd1 ) => ex:Merged
> 
> # This form of comparison sets the Swish exit status based on the result.
> 
> ex:ExpectedMerged :-
>   { _:a1 a :PassengerVehicle .
>     _:a1 :totalCapacity "50"^^xsd:integer .
>     _:a1 :seatedCapacity "30"^^xsd:integer .
>     _:a2 a :PassengerVehicle .
>     _:a2 :totalCapacity "50"^^xsd:integer .
>     _:a2 :standingCapacity "20"^^xsd:integer . }
> 
> @compare ex:Merged ex:ExpectedMerged
> 
> # End of example script

If saved in the file example.ss, then it can be evaluated by saying
either of:

> % Swish -s=example.ss

or, from @ghci@:

> Prelude> :set prompt "Swish> "
> Swish> :m + Swish
> Swish> runSwish "-s=example.ss"

and the output is

> # AssertEq: Infer that Charles is male and has parent Tom
> # Charles is male and has parent Tom
> @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
> @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
> @prefix rdfd: <http://id.ninebynine.org/2003/rdfext/rdfd#> .
> @prefix owl: <http://www.w3.org/2002/07/owl#> .
> @prefix log: <http://www.w3.org/2000/10/swap/log#> .
> @prefix : <http://id.ninebynine.org/default/> .
> @prefix ex: <http://id.ninebynine.org/wip/2003/swishtest/> .
> @prefix pv: <http://id.ninebynine.org/wip/2003/swishtest/pv/> .
> @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
> @prefix xsd_integer: <http://id.ninebynine.org/2003/XMLSchema/integer#> .
> @prefix rs_rdf: <http://id.ninebynine.org/2003/Ruleset/rdf#> .
> @prefix rs_rdfs: <http://id.ninebynine.org/2003/Ruleset/rdfs#> .
> :Charles ex:parent :Tom ;
>          a ex:Male .
> 
> Proof satisfied: ex:Proof
> # AssertEq: Forward chain test
> # AssertEq: Backward chain test
> # AssertIn: Backward chain component test (0)
> # AssertIn: Backward chain component test (1)
> # Merge: ex:Merged
> # Compare: ex:Merged ex:ExpectedMerged

-}

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2018, 2020, 2024 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
