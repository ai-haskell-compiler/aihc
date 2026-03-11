{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | A minimalistic Mustache-like syntax, truly logic-less,
-- pure 'T.Text' template library
--
--    * Use only the simplest Mustache tag {{name}} called a variable.
--    * HTML agnostic
--

module Text.Glabrous
  (

  -- * 'Template'
    Template (..)

  -- ** Get a 'Template'
  , fromText
  , readTemplateFile

  -- ** 'Template' operations
  , addTag
  , tagsOf
  , tagsRename
  , isFinal
  , toText
  , toFinalText
  , compress
  , writeTemplateFile
  , insertTemplate
  , insertManyTemplates

  -- * 'Context'
  , Context (..)

  -- ** Get a 'Context'
  , initContext
  , fromTagsList
  , fromList
  , fromTemplate

  -- ** 'Context' operations
  , setVariables
  , deleteVariables
  , variablesOf
  , isSet
  , unsetContext
  , join

  -- ** JSON 'Context' file
  , readContextFile
  , writeContextFile
  , initContextFile

  -- * Processing
  , process
  , processWithDefault
  , partialProcess
  , Result (..)
  , partialProcess'

  ) where

import           Control.Monad            (guard)
import           Data.Aeson               hiding (Result)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as L
import qualified Data.HashMap.Strict      as H
import           Data.List                (intersect, intersperse, uncons)
import qualified Data.Text                as T
import qualified Data.Text.IO             as I

import           Text.Glabrous.Internal
import           Text.Glabrous.Types


-- | Get 'Just' a new 'Template' with new tag(s) inside, or 'Nothing'.
addTag :: Template       -- ^ The template to work on
       -> T.Text         -- ^ Text to be replaced by the new tag
       -> T.Text         -- ^ New tag's name
       -> Maybe Template -- ^ Just a new template or nothing
addTag Template{..} r n = do
  let nc = concatMap (insertTag r n) content
  guard (length nc > length content)
  return Template { content = nc }
  where
    insertTag t t' (Literal l) =
      filter
        (/= Literal T.empty)
        (intersperse (Tag t') $ Literal <$> T.splitOn t l)
    insertTag _ _ t@(Tag _) = [t]

-- | Optimize a 'Template' content after (many) 'partialProcess'(') rewriting(s).
compress :: Template -> Template
compress Template{..} =
  Template { content = go content [] }
  where
    go ts !ac = do
      let (a,b) = span isLiteral ts
          u = uncons b
      if not (null a)
        then case u of
          Just (c,d) -> go d (ac ++ [concatLiterals a] ++ [c])
          Nothing    -> ac ++ [concatLiterals a]
        else case u of
          Just (e,f) -> go f (ac ++ [e])
          Nothing    -> ac
      where
        concatLiterals =
          foldr trans (Literal "")
          where
            trans (Literal a) (Literal b) = Literal (a `T.append` b)
            trans _           _           = undefined

-- | Build an empty 'Context'.
initContext :: Context
initContext = Context { variables = H.empty }

-- | Populate with variables and/or update variables in the given 'Context'.
--
-- >λ>setVariables [("tag","replacement"), ("theme","Haskell")] context
-- >Context {variables = fromList [("etc.","..."),("theme","Haskell"),("tag","replacement"),("name","")]}
setVariables :: [(T.Text,T.Text)] -> Context -> Context
setVariables ts Context{..} =
  go ts variables
  where
    go _ts vs =
      case uncons _ts of
        Just ((k,v),ts') -> go ts' (H.insert k v vs)
        Nothing          -> Context { variables = vs }

-- | Delete variables from a 'Context' by these names.
--
-- >λ>deleteVariables ["tag"] context
-- >Context {variables = fromList [("etc.","..."),("theme","Haskell"),("name","")]}
deleteVariables :: [T.Text] -> Context -> Context
deleteVariables ts Context{..} =
  go ts variables
  where
    go _ts vs =
      case uncons _ts of
        Just (k,ts') -> go ts' (H.delete k vs)
        Nothing      -> Context { variables = vs }

-- | Build a 'Context' from a list of 'Tag's and replacement 'T.Text's.
--
-- >λ>fromList [("tag","replacement"), ("etc.","...")]
-- >Context {variables = fromList [("etc.","..."),("tag","replacement")]}
--
fromList :: [(T.Text, T.Text)] -> Context
fromList ts = Context { variables = H.fromList ts }

-- | Build an unset 'Context' from a list of 'Tag's.
--
-- >λ>fromTagsList ["tag","etc."]
-- >Context {variables = fromList [("etc.",""),("tag","")]}
fromTagsList :: [T.Text] -> Context
fromTagsList ts = fromList $ (,T.empty) <$> ts

-- | Build an unset ad hoc 'Context' from the given 'Template'.
fromTemplate :: Template -> Context
fromTemplate t =
  setVariables (toPair <$> tagsOf t) initContext
  where
    toPair (Tag e) = (e,T.empty)
    toPair _       = undefined

-- | Get a 'Context' from a JSON file.
readContextFile :: FilePath -> IO (Maybe Context)
readContextFile f = decode <$> L.readFile f

-- | Join two 'Context's if they don't share variables
-- name, or get the intersection 'Context' out of them.
join :: Context
     -> Context
     -> Either Context Context
join c c' = do
  let i = H.intersection (variables c) (variables c')
  if i == H.empty
    then Right Context { variables = H.union (variables c) (variables c') }
    else Left Context { variables = i }

-- | Write a 'Context' to a file.
--
-- @
-- {
--     "tag": "replacement",
--     "etc.": "..."
-- }
-- @
--
writeContextFile :: FilePath -> Context -> IO ()
writeContextFile f c = L.writeFile f (encodePretty c)

-- | Based on the given 'Context', write a JSON
-- 'Context' file with all its variables empty.
--
-- @
-- {
--     "tag": "",
--     "etc.": ""
-- }
-- @
--
initContextFile :: FilePath -> Context -> IO ()
initContextFile f Context{..} = L.writeFile f $
  encodePretty Context { variables = H.map (const T.empty) variables }

-- | Build 'Just' a (sub)'Context' made of unset variables
-- of the given context, or 'Nothing'.
--
-- >λ>unsetContext context
-- >Just (Context {variables = fromList [("name","")]})
--
unsetContext :: Context -> Maybe Context
unsetContext Context {..} = do
  let vs = H.filter (== T.empty) variables
  guard (vs /= H.empty)
  return Context { variables = vs }

-- | 'True' if the all variables of
-- the given 'Context' are not empty.
isSet :: Context -> Bool
isSet Context{..} =
  H.foldr (\v b -> b && v /= T.empty) True variables

-- | Get the list of the given 'Context' variables.
variablesOf :: Context -> [T.Text]
variablesOf Context{..} = H.keys variables

-- | Get a 'Template' from a file.
readTemplateFile :: FilePath -> IO (Either String Template)
readTemplateFile f = fromText <$> I.readFile f

-- | Write a 'Template' to a file.
writeTemplateFile :: FilePath -> Template -> IO ()
writeTemplateFile f t = I.writeFile f (toText t)

-- | get 'Just' a new 'Template' by inserting a 'Template'
-- into another one by replacing the 'Tag', or 'Nothing'.
--
-- >λ>insertTemplate t0 (Tag "template1") t1
insertTemplate :: Template       -- ^ The Template to insert in
               -> Token          -- ^ The Tag to be replaced
               -> Template       -- ^ The Template to be inserted
               -> Maybe Template -- ^ Just the new Template, or Nothing
insertTemplate _ (Literal _) _ = Nothing
insertTemplate te t te' = do
  guard (t `elem` content te)
  return Template { content = foldl trans [] (content te) }
  where
    trans o t'@(Tag _) =
      if t' == t
        then o ++ content te'
        else o ++ [t']
    trans o l = o ++ [l]

-- | get 'Just' a new 'Template' by inserting many 'Template's,
-- if there is at least one tag correspondence, or 'Nothing'.
--
-- >λ>insertManyTemplates t0 [(Tag "template1",t1),(Tag "template2",t2)]
insertManyTemplates :: Template -> [(Token,Template)] -> Maybe Template
insertManyTemplates te ttps = do
  guard (tagsOf te `intersect` (fst <$> ttps) /= mempty)
  return Template { content = foldl trans [] (content te) }
  where
    trans o li@(Literal _) = o ++ [li]
    trans o ta@(Tag _) =
      case lookupTemplate ta ttps of
        Nothing  -> o ++ [ta]
        Just te' -> o ++ content te'
    lookupTemplate (Literal _) _ = Nothing
    lookupTemplate _ []          = Nothing
    lookupTemplate t (p:ps) =
      if fst p == t
        then Just (snd p)
        else lookupTemplate t ps

-- | Output the content of the given 'Template'
-- as it is, with its 'Tag's, if they exist.
toText :: Template -> T.Text
toText Template{..} =
  T.concat (trans <$> content)
  where
    trans (Literal c) = c
    trans (Tag k)     = T.concat ["{{",k,"}}"]

-- | Output the content of the given 'Template'
-- with all its 'Tag's removed.
toFinalText :: Template -> T.Text
toFinalText Template{..} =
  foldl trans T.empty content
  where
    trans o (Literal l) = o `T.append` l
    trans o (Tag _)     = o

-- | Get the list of 'Tag's in the given 'Template'.
tagsOf :: Template -> [Token]
tagsOf Template{..} = filter isTag content

tagsRename :: [(T.Text,T.Text)] -> Template -> Template
tagsRename ts Template{..} =
  Template { content = rename <$> content }
  where
    rename t@(Tag n)     = maybe t Tag (lookup n ts)
    rename l@(Literal _) = l

-- | 'True' if a 'Template' has no more 'Tag'
-- inside and can be used as a final 'T.Text'.
isFinal :: Template -> Bool
isFinal Template{..} = all isLiteral content

-- | Process, discard 'Tag's which are not in the 'Context'
-- and replace them with nothing in the final 'T.Text'.
process :: Template -> Context -> T.Text
process = processWithDefault T.empty

-- | Process and replace missing variables in 'Context'
-- with the given default replacement 'T.Text'.
processWithDefault
  :: T.Text    -- ^ Default replacement text
  -> Template
  -> Context
  -> T.Text
processWithDefault d Template{..} c = toTextWithContext (const d) c content

-- | Process a (sub)'Context' present in the given template, leaving
-- untouched, if they exist, other 'Tag's, to obtain a new template.
partialProcess :: Template -> Context -> Template
partialProcess Template{..} c =
  Template { content = transTags content c }
  where
    transTags ts Context{..} =
      trans <$> ts
      where
        trans i@(Tag k) = maybe i Literal (H.lookup k variables)
        trans t         = t

-- | Process a (sub)'Context' present in the given template, and
-- get either a 'Final' 'T.Text' or a new 'Template' with its unset
-- ad hoc 'Context'.
--
-- >λ>partialProcess' template context
-- >Partial {template = Template {content = [Literal "Some ",Tag "tags",Literal " are unused in this ",Tag "text",Literal "."]}, context = Context {variables = fromList [("text",""),("tags","")]}}
partialProcess' :: Template -> Context -> Result
partialProcess' t c@Context{..} =
  case foldl trans (mempty,mempty) (content t) of
    (f,[]) -> Final (toTextWithContext (const T.empty) c f)
    (p,p') -> Partial Template { content = p } (fromTagsList p')
  where
    trans (!c',!ts) t' =
      case t' of
        Tag k ->
          case H.lookup k variables of
            Just v  -> (c' ++ [Literal v],ts)
            Nothing -> (c' ++ [t'],ts ++ [k])
        Literal _ -> (c' ++ [t'],ts)

