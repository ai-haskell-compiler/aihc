{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides XML picklers that plug into the XML tree of the
-- /xml-types/ package. This module was \"inspired\" by /hexpat-pickle/.
--
-- The API differences between /hexpat-pickle/ and this module include:
--
--  * When unpickling, picklers will /consume/ matching elmements so that they
--    will be ignored by sucessive picklers. To circumvent this behaviour, use
--    @'xpPeek'@
--
--  * Wrappers like 'xpWrap' are uncurried
--
--  * There are no lazy unpicklers
--
--  * Most unpicklers will produce an error when their child unpicklers fail to
--    consume all elements. Use 'xpClean' to discard those elements
--
-- The data type @'PU' t a@ represents both a pickler (converting Haskell data
-- to XML) and an unpickler (XML to Haskell data), so your code only needs to
-- be written once for both serialization and deserialization.  The 'PU'
-- primitives, such as 'xpElem' for XML elements, may be composed into complex
-- arrangements using 'xpPair' and other combinators.
--
-- Most picklers will try to find the /first match/ rather than failing when
-- the first element doesn't match. This is why the target type often is a
-- list. To prevent this behaviour and commit the pickler to the first element
-- available, use 'xpIsolate'.
--
-- The top level of the document does not follow this rule, because it is a
-- single node type. 'xpRoot' is needed to adapt this to type ['Node'] for your
-- pickler to use.  You would typically define a pickler for a whole document
-- with 'xpElem', then pickle it to a single 'Node' with @'pickleTree' (xpRoot
-- myDocPickler) value@.
--
-- /NB/: Unresolved entities are considered an error and will trigger an
-- exception
--
-- When unpickling, the following invariant regarding the list of remaining
-- elements should be observed:
--
-- * The returned list should be a subset of or the initial list itself, that
--   is, no elements should be added or changed
--
-- * The relative order of elements should be preserved
--
-- * Elements may, however, be removed from anywhere in the list
--
-- Here is a simple example to get you started:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Text
-- > import Data.XML.Types
-- > import Data.XML.Pickle
-- >
-- > -- Person name, age and description
-- > data Person = Person Text Int Text
-- >
-- > xpPerson :: PU [Node] Person
-- > xpPerson =
-- >     -- How to wrap and unwrap a Person
-- >     xpWrap (\((name, age), descr) -> Person name age descr)
-- >            (\(Person name age descr) -> ((name, age), descr)) $
-- >     xpElem "person"
-- >         (xpPair
-- >             (xpAttr "name" xpId)
-- >             (xpAttr "age" xpPrim))
-- >         (xpContent xpId)
-- >
-- > people = [
-- >     Person "Dave" 27 "A fat thin man with long short hair",
-- >     Person "Jane" 21 "Lives in a white house with green windows"]
-- >
-- > main = do
-- >     print $ pickle (xpRoot $ xpElemNodes "people" $ xpAll xpPerson) people
--
-- Program output would be an xml-value equivalent to:
--
-- > <people>
-- >   <person name="Dave" age="27">
-- >     A fat thin man with long short hair
-- >   </person>
-- >   <person name="Jane" age="21">
-- >     Lives in a white house with green windows
-- >  </person>
-- > </people>
--
-- Functions marked with /compat/ are included for compatibility with
-- /hexpat-pickle/.

module Data.XML.Pickle (
  -- * Types
  PU(..)
  , Attribute
  , UnpickleResult(..)
  -- * Pickler invocation
  , pickle
  , unpickle
  -- * Primitive picklers
  , xpUnit
  , xpZero
  , xpThrow
  , xpIso
  , xpPartial
  -- * Value-preserving picklers
  , xpId
  , xpFst
  , xpSnd
  , xpTrees
  , xpHead
  , xpTree
  , xpText0
  , xpText
  , xpString
  , xpRoot
  , xpPrim
  -- * XML specific picklers
  -- ** Attributes
  , xpAttribute
  , xpAttribute'
  , xpAttribute_
  , xpAttr
  , xpAttrImplied
  , xpAttrFixed
  , xpAddFixedAttr
  -- ** Elements
  , xpElem
  , xpElemWithName
  , xpElemByNamespace
  , xpElemVerbatim
  , xpElemAttrs
  , xpElemNodes
  , xpElemText
  , xpElemBlank
  , xpElemExists
  , xpElems
  -- ** Character Content
  , xpContent
  , xpBool
  -- * Pickler combinators
  -- ** Choice
  , xpOption
  , xpDefault
  , xpWithDefault
  , xpMap
  , xpAlt
  , xpChoice
  , xpEither
  , xpTryCatch
  -- ** Sequencing
  -- | /NB/: The sequencing operations /do not/ enforce any order on the
  -- matched elements unless stated otherwise, but you can commit individial
  -- picklers to the next available element with 'xpIsolate'. Applying
  -- @xpIsolate@ on all nested Picklers will in effect enforce order.
  --
  -- Howver, once a pickler consumes an element it will not be available to
  -- following picklers. You can circumvent this behaviour with 'xpPeek'.
  --
  -- If you want ensure that all elements are consumed after the last pickler
  -- is run you may want to use 'xpClean'.
  --
  -- *** Lists
  -- | The List pickler combinators will pickle lists in the given order
  -- without any special treatment and unpickle as stated.
  , xpFindMatches
  , xpFindFirst
  , xpAll
  , xpSubsetAll
  , xpAllByNamespace
  , xpList0
  , xpSeqWhile
  , xpList
  , xpListMinLen
  -- *** Tuples
  -- | Tuple combinators apply their picklers from left to right. They will
  -- succeed when all their constituents produce a value.
  , xp2Tuple
  , xpPair
  , (<#>)
  , xp3Tuple
  , xpTriple
  , xp4Tuple
  , xp5Tuple
  , xp6Tuple
  -- ** Wrappers
  -- *** Value wrappers
  , xpWrap
  , xpConst
  , xpWrapEither
  , xpWrapMaybe
  , xpWrapMaybe_
  , xpAssert
  , xpMayFail
  , xpUnliftElems
  -- *** Bookkeeping
  -- | Change the semantics of picklers
  , xpIsolate
  , xpPeek
  -- *** Cleannes
  -- | Picklers keep track of elements left over after unpickling, so the may
  -- be [@clean@] an unpickling is considered @clean@ when it doesn't leave any
  -- remainng elements.
  , xpClean
  -- * Error handling
  , UnpickleError(..)
  , ppUnpickleError
  , (<++>)
  , (<?+>)
  , (<?>)
  , (<??>)
  , UnresolvedEntityException(..)
  -- * Helper functions
  , flattenContent
  , tErr
  , getRest
)   where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char (isSpace)
import Data.Either
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types

import Data.XML.Pickle.Tuples
import Data.XML.Pickle.Basic

-- | Pickle a tree.
pickle :: PU t a -> a -> t
pickle = pickleTree

-- | Unpickle a tree.
unpickle :: PU t a -> t -> Either UnpickleError a
unpickle xp x = case unpickleTree xp x of
    UnpickleError e -> Left e
    NoResult e -> Left . ErrorMessage $ "Entity not found " `Text.append` e
    Result r _ -> Right r

for :: [a] -> (a -> b) -> [b]
for = flip map

type Attribute = (Name,[Content])

-- | Isomorphic pickler.
xpIso :: (a -> b) -> (b -> a) -> PU a b
xpIso f g = PU (\t -> Result (f t) Nothing) g

xpPartial :: (a -> Either Text b)
          -> (b -> a)
          -> PU a b
xpPartial f g = ("xpEither", "") <?+>
               PU { pickleTree = g
                  , unpickleTree = \v -> case f v of
                      Left e -> UnpickleError $ ErrorMessage e
                      Right r -> Result r Nothing
                  }

-- | Doesn't create or consume anything, always succeeds.
xpUnit :: PU [a] ()
xpUnit = PU (Result () . remList) (const [])


-- | Returns everything (remaining), untouched.
xpId :: PU a a
xpId = xpIso id id

-- | 'xpId'. (/compat/)
xpTrees :: PU a a
xpTrees = xpId

-- | Converts Booleans to XML boolean values
--
-- * \"true\" and \"1\" are read as @True@
--
-- * \"false\" and \"0\" are read as @False@
--
-- * All other values generate an unpickle error
--
-- Will always generate \"true\" or \"false\" (not \"0\" or \"1\") when
-- pickling.
xpBool :: PU Text Bool
xpBool = ("xpBool" ,"") <?+> PU
          { unpickleTree =
                 \v -> case () of ()
                                      | v `elem` ["true",  "1"] ->
                                          Result True Nothing
                                      | v `elem` ["false", "0"] ->
                                          Result False Nothing
                                      | otherwise -> UnpickleError
                                                     (ErrorMessage $
                                                      "Not a boolean value: "
                                                      `Text.append` v)
          , pickleTree = \v -> case v of
                     True -> "true"
                     False -> "false"
          }

-- | Apply a bijection before pickling / after unpickling.
xpWrap :: (a -> b) -> (b -> a) -> PU t a -> PU t b
xpWrap to from xp = ("xpWrap","") <?+>
                    PU { unpickleTree = \x -> to <$> unpickleTree xp x
                       , pickleTree = pickleTree xp . from
                       }

-- | Like xpWrap, but strips @Just@ (and treats @Nothing@ as a failure) during
-- unpickling.
xpWrapMaybe :: (a -> Maybe b) -> (b -> a) -> PU t a -> PU t b
xpWrapMaybe a2b b2a pua = ("xpWrapMaybe","") <?>
                 xpWrapMaybe_ "xpWrapMaybe can't encode Nothing" a2b b2a pua

-- | Like xpWrap, but strips @Just@ (and treats @Nothing@ as a failure) during
-- unpickling, with specified error message for @Nothing@ value.
xpWrapMaybe_ :: String -> (a -> Maybe b) -> ( b -> a) -> PU t a -> PU t b
xpWrapMaybe_ errorMsg a2b b2a pua = ("xpWrapMaybe_","") <?+> PU {
        unpickleTree = \t -> case unpickleTree pua t of
            Result val rest ->
                case a2b val of
                    Just val' -> Result val' rest
                    Nothing   -> UnpickleError $ upe errorMsg
            NoResult e -> NoResult e
            UnpickleError e  -> UnpickleError e
        , pickleTree = pickleTree pua  . b2a
    }

-- | Lift a pickler. Nothing is returned when the given pickler doesn't return
-- a value (e.g. the element isn't found). Does not affect unpickling errors.
-- @Nothing@ is pickled to @mempty@.
--
-- A typical example is:
--
-- > xpElemAttributes "score" $ xpOption $ xpAttribute "value" xpPrim
--
-- in which @Just 5@ is encoded as @\<score value=\"5\"\/\>@ and @Nothing@
-- as @\<score\/\>@.
xpOption :: PU [t] a -> PU [t] (Maybe a)
xpOption pu = PU { unpickleTree = doUnpickle
                 , pickleTree = \mValue ->
                      case mValue of
                          Just value -> pickleTree pu value
                          Nothing    -> mempty
                 }
  where
    doUnpickle t =
        case unpickleTree pu t of
            Result r t' -> Result (Just r) t'
            NoResult _e -> Result Nothing (remList t)
            UnpickleError e -> UnpickleError e

-- | Return one element, untouched.
xpHead :: PU [a] a
xpHead = PU {unpickleTree = \t' -> case t' of
                [] -> UnpickleError $ ("xpHead","")
                      <++> upe "No element remaining"
                t:ts -> Result t (if null ts then Nothing else Just ts)
            , pickleTree = return
            }

-- | 'xpHead'. (/compat/)
xpTree :: PU [a] a
xpTree = xpHead

-- | Specialised version of 'xpId'. (/compat/)
xpText0 :: PU Text Text
xpText0 = xpId

-- | Convert @Text@ to/from @String@.
xpString :: PU Text String
xpString = ("xpString", "") <?> xpIso Text.unpack Text.pack

-- | Test predicate when unpickling. Fails with given error message when the
-- predicate return false.
--
-- /NB/: The predicate will only be tested while /unpickling/. When pickling,
-- this is a noop.
xpAssert :: Text -> (a -> Bool) -> PU t a -> PU t a
xpAssert err p xp = ("xpAssert",err) <?+>
                    PU { unpickleTree = \t -> do
                              r <- unpickleTree xp t
                              unless (p r) $ UnpickleError assertErr
                              return r
                       , pickleTree = pickleTree xp
                       }
  where
    assertErr = upe ("Assertion failed; " ++ Text.unpack err)

-- | Like 'xpText0', but fails on non-empty input.
xpText :: PU Text Text
xpText = ("xpText","") <?> xpAssert "Input is empty" (not . Text.null) xpText0

-- | Transforms a pickler on lists to a pickler on single elements.
--
-- /NB/: Will error when the given pickler doesn't produce exactly one
-- element.
xpRoot ::PU [a] b -> PU a b
xpRoot pa = ("xpRoot","") <?+> PU
       { unpickleTree = \t -> case unpickleTree pa [t] of
              Result x Nothing -> Result x Nothing
              Result _x (Just _) -> UnpickleError $ upe "Leftover entities"
              UnpickleError e -> UnpickleError e
              NoResult e -> NoResult e
       , pickleTree = \t -> case pickleTree pa t of
           [t1] -> t1
           _    -> error "pickler called by xpRoot must output exactly one element"
       }

getFirst :: (t -> Bool) -> [t] -> Maybe (t, [t])
getFirst _ [] = Nothing
getFirst p (x:xs) = case p x of
      True  -> Just (x,xs)
      False -> second (x:) <$> getFirst p xs

-- | Pickle to/from attribute.
xpAttribute :: Name -> PU Text a -> PU [Attribute] a
xpAttribute name pu = ("xpAttr" , Text.pack $ ppName name) <?+> PU
        { unpickleTree = doUnpickle
        , pickleTree = \value -> [(name, [ContentText $ pickleTree pu value])]
        }
  where
    doUnpickle attrs = case getFirst ((== name) . fst) attrs of
      Nothing -> NoResult $ Text.pack $ ppName name
      Just ((_, c), rem')
        | all isContentText c -> case unpickleTree pu (contentToText c) of
          NoResult e -> missingE $ Text.unpack e
          UnpickleError e -> UnpickleError e
          Result _ (Just e) -> leftoverE $ show e
          Result r Nothing  -> Result r (remList rem')
        -- Content entities are not supported.
        | otherwise -> UnpickleError $
                       upe ("Unresolved entities in " ++ ppName name ++ ".")
    contentToText = Text.concat . map contentToText_
    contentToText_ (ContentText t) = t
    contentToText_ (ContentEntity t) = t
    isContentText (ContentText _) = True
    isContentText (ContentEntity _) = False

-- | 'xpAttribute' (/compat/)
xpAttr :: Name -> PU Text a -> PU [Attribute] a
xpAttr = xpAttribute

-- | Pickle attribute if @Just@ is given, on unpickling return @Just <val>@
-- when the attribute is found, @Nothing@ otherwise.
xpAttribute' :: Name -> PU Text a -> PU [Attribute] (Maybe a)
xpAttribute' name pu = xpOption $ xpAttr name pu

xpAttrImplied :: Name -> PU Text a -> PU [Attribute] (Maybe a)
xpAttrImplied = xpAttribute'

-- | Pickle an attribute with the specified name and value, fail if the same
-- attribute is not present on unpickle.
xpAttribute_ :: Name -> Text -> PU [Attribute] ()
xpAttribute_ name val =
    xpWrapMaybe_ ("expected fixed attribute "++ ppName name++"="++show val)
                (\v -> if v == val then Just () else Nothing) (const val) $
    xpAttr name xpId

-- | 'xpAttribute_' (/compat/)
xpAttrFixed :: Name -> Text -> PU [Attribute] ()
xpAttrFixed = xpAttribute_

-- | Merge successive @NodeContent@ values.
flattenContent :: [Node] -> [Node]
flattenContent xs = case foldr (\x (buf, res) -> case x of
                         NodeContent (ContentText txt)
                           -> (txt : buf, res)
                         NodeComment _ -> (buf, res)
                         e@(NodeElement _)
                           -> ([] , e : addConcatText buf res)
                         _ -> throw UnresolvedEntityException
                    ) ([], []) xs
                   of
                   (buf, res) -> addConcatText buf res
  where
    nc = NodeContent . ContentText
    addConcatText [] = id
    addConcatText xs' = let txt = Text.concat xs' in
        if Text.all isSpace txt then id else  (nc txt :)

-- | When unpickling, tries to find the first element with the supplied name.
-- Once such an element is found, it will commit to it and /fail/ if any of the
-- picklers don't match.
xpElem :: Name -- ^ Name of the 'Element'
          -> PU [Attribute] a -- ^ Pickler for attributes
          -> PU [Node] n  -- ^ Pickler for child nodes
          -> PU [Node] (a,n)
xpElem name attrP nodeP = tr <?+> PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \(a,n) -> [NodeElement $ Element name
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst (nodeElementNameHelper name) nodes of
      Just (NodeElement (Element _ attrs children), rem') -> do
          as <- ("attrs","") <++.> child attrP attrs
          cs <- ("children","") <++.> child nodeP (flattenContent children)
          leftover $ remList rem'
          return (as, cs)
      _ -> NoResult $ Text.pack $ ppName name

    tr = ("xpElem", Text.pack $ ppName name)

    nodeElementNameHelper name' (NodeElement (Element n _ _)) = n == name'
    nodeElementNameHelper _ _ = False

-- | Handle all elements with a given name. The unpickler will fail when any of
-- the elements fails to unpickle.
xpElems :: Name -- ^ Name of the 'Element' values
        -> PU [Attribute] a -- ^ Pickler for attributes
        -> PU [Node] n -- ^ Pickler for child nodes
        -> PU [Node] [(a, n)]
xpElems name attrs children = tr <?> xpSubsetAll isThisElem
                                       (xpElem name attrs children)
  where
    isThisElem (NodeElement (Element name' _ _)) = name' == name
    isThisElem _ = False

    tr = ("xpElems", Text.pack $ ppName name)

-- | Tries to apply the pickler to all the remaining elements; fails if any of
-- them don't match.
xpAll :: PU [a] b -> PU [a] [b]
xpAll xp = ("xpAll", "") <?+> PU { unpickleTree = doUnpickleTree
                                 , pickleTree = concatMap (pickleTree xp)
                                 } where
  doUnpickleTree = mapM (child' xp . return)

-- | For unpickling, apply the given pickler to a subset of the elements
-- determined by a given predicate.
--
-- Pickles like 'xpAll'.
xpSubsetAll :: (a -> Bool) -- ^ Predicate to select the subset
            -> PU [a] b    -- ^ Pickler to apply on the subset
            -> PU [a] [b]
xpSubsetAll p xp = ("xpSubsetAll","") <?+> PU { unpickleTree = \t ->
                     let (targets, rest) = partition p t in
                     do
                         leftover $ remList rest
                         child' (xpAll xp) targets
                     , pickleTree = pickleTree $ xpAll xp
             }


-- | Apply unpickler to all elements with the given namespace.
--
-- Pickles like 'xpAll'.
xpAllByNamespace :: Text -> PU [Node] b -> PU [Node] [b]
xpAllByNamespace namespace xp = ("xpAllByNamespace",namespace)
                                  <?> xpSubsetAll helper xp

  where
    helper (NodeElement (Element (Name _local (Just ns) _pre) _ _ ))
            = ns == namespace
    helper _ = False


-- | Pickle 'Element' without a restriction on the name. The name as taken /
-- returned as the first element of the triple.
xpElemWithName :: PU [Attribute] a  -- ^ Pickler for attributes
                  -> PU [Node] n    -- ^ Pickler for child nodes
                  -> PU [Node] (Name,a,n)
xpElemWithName attrP nodeP = ("xpElemWithName", "") <?+> PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \(name, a,n) -> [NodeElement $ Element name
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst nodeElementHelper nodes of
      Just (NodeElement (Element name attrs children), rem') -> do
          x <- child attrP attrs
          y <- child nodeP $ flattenContent children
          leftover $ remList rem'
          return (name, x, y)
      _ -> NoResult "element"
    nodeElementHelper (NodeElement Element{}) = True
    nodeElementHelper _ = False

-- | Find element by name space, prefixes are ignored.
xpElemByNamespace :: Text -- ^ Namespace
                  -> PU Text name -- ^ Pickler for the local name
                  -> PU [Attribute] a  -- ^ Pickler for attributes
                  -> PU [Node] n    -- ^ Pickler for child nodes
                  -> PU [Node] (name,a,n)
xpElemByNamespace ns nameP attrP nodeP = PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \(name, a,n) -> [NodeElement $ Element
                                     (Name (pickleTree nameP name) (Just ns) Nothing)
                                     (pickleTree attrP a)
                                     (pickleTree nodeP n)
                                    ]
         } where
    doUnpickleTree nodes = case getFirst (nodeElementNSHelper ns) nodes of
      Just (NodeElement (Element name attrs children), rem') -> tr name $
          do
              name'  <- child nameP (nameLocalName name)
              attrs' <- child attrP attrs
              nodes' <- child nodeP children
              leftover $ remList rem'
              return (name', attrs', nodes')

      _ -> NoResult $ "Element with namepspace " `Text.append` ns
    tr a res = case res of
        UnpickleError e -> UnpickleError (TraceStep
                                            ( "xpElemByNamespace"
                                            , Text.concat [ ns
                                                          , " ; "
                                                          , nameLocalName a])
                                          e)
        x -> x

    nodeElementNSHelper ns' (NodeElement (Element n _ _)) = nameNamespace n == Just ns'
    nodeElementNSHelper _ns _ = False

-- | Pickler returns the first found 'Element' untouched.
--
-- Unpickler wraps element in 'NodeElement'.
xpElemVerbatim ::  PU [Node] Element
xpElemVerbatim = PU
         { unpickleTree = doUnpickleTree
         , pickleTree   = \e -> [NodeElement e]
         } where
    doUnpickleTree nodes = case getFirst nodeElementHelper nodes of
      Just (NodeElement e@Element{}, re) -> Result e (remList re)
      _ -> NoResult "element"

    nodeElementHelper (NodeElement Element{}) = True
    nodeElementHelper _ = False

-- | A helper variant of 'xpElem' for elements that contain attributes but no
-- child tags.
xpElemAttrs :: Name -> PU [Attribute] b -> PU [Node] b
xpElemAttrs name puAttrs = xpWrap fst (\a -> (a,())) $
                             xpElem name puAttrs xpUnit

-- | A helper variant of 'xpElem' for elements that contain child nodes but no
-- attributes.
xpElemNodes :: Name -> PU [Node] b -> PU [Node] b
xpElemNodes name puChildren = xpWrap snd (\a -> ((),a)) $
                                xpElem name xpUnit puChildren

-- | A helper variant of 'xpElem' for elements that contain only character
-- data.
xpElemText :: Name -> PU [Node] Text
xpElemText name = xpElemNodes name $ xpContent xpId

-- | Helper for 'Element' values that don't contain anything.
xpElemBlank :: Name -> PU [Node] ()
xpElemBlank name = ("xpElemBlank", "") <?> xpWrap (const () ) (const ((),())) $
                                xpElem name xpUnit xpUnit

-- | When pickling, creates an empty element iff parameter is @True@.
--
-- When unpickling, checks whether element exists. Generates an error when the
-- element is not empty.
xpElemExists :: Name -> PU [Node] Bool
xpElemExists name = ("xpElemBlank", "") <?>
                    xpWrap (\x -> case x of Nothing -> False; Just _ -> True)
                           (\x -> if x then Just () else Nothing) $
                           xpOption (xpElemBlank name)


-- | Get the first non-element 'NodeContent' from a node.
xpContent :: PU Text a -> PU [Node] a
xpContent xp = ("xpContent","") <?+> PU
       { unpickleTree = doUnpickle
       , pickleTree = return . NodeContent . ContentText . pickleTree xp
       } where
     doUnpickle nodes = case getFirst nodeContentHelper
                               (filter (\node -> case node of
                                           NodeComment _ -> False
                                           _ -> True) nodes) of -- flatten
       Just (NodeContent (ContentText t), _re) -> child xp t
       Just (NodeContent (ContentEntity t), _) ->
           UnpickleError . upe $ "Unresolved entity" ++ show t ++ "."
       _ -> NoResult "node content"

     nodeContentHelper (NodeContent _) = True
     nodeContentHelper _ = False


-- | Unlift a pickler on 'Node' values to a pickler on 'Element' values. Nodes
-- generated during pickling that are not 'Element' values will be silently
-- discarded.
xpUnliftElems :: PU [Node] a -> PU [Element] a
xpUnliftElems xp = ("xpUnliftElems","") <?+> PU
             { unpickleTree = doUnpickle
             , pickleTree = nodesToElems . pickleTree xp
             }
  where
    doUnpickle nodes = case unpickleTree xp (map NodeElement nodes) of
        UnpickleError e -> UnpickleError e
        NoResult e -> NoResult e
        Result a r -> let r' = case r of
                              Nothing -> Nothing
                              Just rs' -> case nodesToElems rs' of
                                  [] -> Nothing
                                  rs -> Just rs
                      in Result a r'
    nodesToElems = foldr (\n es -> case n of
                                      NodeElement e -> e : es
                                      _ -> es) []

-- | Optional conversion with default value.
--
-- Unlike 'xpWithDefault' the default value is not encoded in the XML document,
-- during unpickling the default value is inserted if the pickler doesn't
-- return a value.
xpDefault :: (Eq a) => a -> PU [t] a -> PU [t] a
xpDefault df
    = xpWrap (fromMaybe df)
             (\ x -> if x == df then Nothing else Just x)
             .
      xpOption

-- | Attempt to use a pickler. Return a default value when the pickler doesn't
-- return anything (doesn't touch on 'UnpickleError').
--
-- Unlike 'xpDefault', the default value /is/ encoded in the XML document.
xpWithDefault :: a -> PU t a -> PU t a
xpWithDefault a pa = PU { pickleTree = pickleTree pa
                        , unpickleTree = \v -> case unpickleTree pa v of
                            Result r t -> Result r t
                            NoResult _ -> Result a (Just v)
                            UnpickleError e -> UnpickleError e
                        }

-- | 'xp2Tuple'. (/compat/)
xpPair :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xpPair l r = "xpPair" <??> xp2Tuple l r

-- | 'xp2Tuple'.
(<#>) :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
(<#>) l r = "(<#>)" <??> xp2Tuple l r

-- | 'xp3Tuple'. (/compat/)
xpTriple  :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xpTriple l m r = "xpTriple" <??> xp3Tuple l m r


-- | When unpickling, don't consume the matched element(s), noop when pickling.
xpPeek :: PU t a -> PU t a
xpPeek xp = PU { pickleTree = pickleTree xp
               , unpickleTree = \xs ->
                  case unpickleTree xp xs of
                    Result r _ -> Result r (Just xs)
                    x          -> x
               }

-- | Noop when pickling.
--
-- When unpickling, only give access to the first element.
xpIsolate :: PU [t] a -> PU [t] a
xpIsolate xp = ("xpIsolate","") <?+>
               PU { pickleTree = pickleTree xp
               , unpickleTree = \xs' -> case xs' of
                 [] -> NoResult "entity"
                 (x:xs) -> case unpickleTree xp [x] of
                     Result r t -> Result r (remList $ mbToList t ++ xs)
                     NoResult e -> missingE $ Text.unpack e
                     y          -> y
               } where
  mbToList Nothing = []
  mbToList (Just r) = r

-- | Select a single element from the list and apply unpickler to it.
--
-- Returns no value when no element matches the predicate.
--
-- Fails when the unpickler doesn't return a value.
--
-- When pickling, this is a noop.
xpFindFirst :: (t -> Bool) -> PU [t] a -> PU [t] a
xpFindFirst p xp = ("xpFindFirst","") <?+>
                 PU { pickleTree = pickleTree xp
                    , unpickleTree = \xs -> case break p xs of
                        (_, []) -> NoResult "entity"
                        (ys,z:zs) -> do
                            leftover . remList $ ys ++ zs
                            child' xp [z]
                    }

-- | Ignore input/output and replace with constant values.
xpConst :: a -> PU t () -> PU t a
xpConst c xp = ("xpConst" ,"") <?> xpWrap (const c) (const ()) xp

-- | Convert text to/from any type that implements 'Read' and 'Show'. Fails on
-- unpickle if 'read' fails.
xpPrim :: (Show a, Read a) => PU Text a
xpPrim = PU { unpickleTree = \x -> case reads $ Text.unpack x of
                 []       -> UnpickleError $ ("xpPrim","") <++>
                               upe ("Couldn't read " ++ show x ++ ".")
                 (r,rest):_  -> Result r (Text.pack <$> remList rest)
            ,  pickleTree = Text.pack . show
            }

-- | When unpickling, tries to apply the pickler to all elements returning and
-- consuming only matched elements.
xpFindMatches :: PU [b] a -> PU [b] [a]
xpFindMatches xp = PU { unpickleTree = doUnpickleTree
                     , pickleTree = \xs -> pickleTree xp =<< xs
                     } where
  doUnpickleTree xs =
    let (ls, rs) = partitionEithers . for xs $ \x ->
          case unpickleTree xp [x] of
            NoResult _ -> Left x
            Result r Nothing -> Right $ Result r Nothing
            Result _r (Just _) -> Right $ leftoverE ""
            UnpickleError e -> Right $ UnpickleError e
        in leftover (remList ls) >> sequence rs

-- | 'xpAll'. (/compat/)
xpList0 :: PU [a] b -> PU [a] [b]
xpList0 = xpAll

-- | Like 'xpList', but only succeed during unpickling if at least a minimum
-- number of elements are unpickled.
xpListMinLen :: Int -> PU [a] b -> PU [a] [b]
xpListMinLen ml = xpWrapEither testLength id . xpList
  where
    testLength as
      | length as < ml = Left $ "Expecting at least " ++ show ml ++ " elements"
    testLength as = Right as


-- | When unpickling, successively applies pickler to single elements until it
-- doesn't return anything; returns all matched elements.
xpSeqWhile :: PU [a] b -> PU [a] [b]
xpSeqWhile pu = ("xpSeqWhile", "") <?+> PU {
          unpickleTree = doUnpickle
        , pickleTree = concatMap $ pickleTree pu
        }
  where
    doUnpickle [] = Result [] Nothing
    doUnpickle es@(elt:re) =
                case unpickleTree pu [elt] of
                    Result val _ -> case doUnpickle re of
                                      Result xs r -> Result (val:xs) r
                                      e           -> e
                    NoResult _   -> Result [] (Just es)
                    UnpickleError e -> UnpickleError e

-- | 'xpSeqWhile'. (/compat/)
xpList :: PU [a] b -> PU [a] [b]
xpList = xpSeqWhile

-- | Standard pickler for maps
--
-- This pickler converts a map into a list of pairs of the form
--
-- > <elt attr="key">value</elt>
xpMap :: Ord k =>
     Name  -- ^ Element name (elt)
     -> Name  -- ^ Attribute name (attr)
     -> PU Text k -- ^ Pickler for keys (key)
     -> PU [Node] a  -- ^ Pickler for values (value)
     -> PU [Node] (M.Map k a)
xpMap en an xpk xpv
    = xpWrap M.fromList
             M.toList
              $
      xpAll $
      xpElem en
          (xpAttr an xpk)
          xpv


-- | Like xpWrap, except it strips @Right@ (and treats @Left@ as a failure)
-- during unpickling.
--
-- Not to be confused with 'xpEither'.
xpWrapEither :: Show e => (a -> Either e b) -> (b -> a) -> PU t a -> PU t b
xpWrapEither a2b b2a pua = ("xpWrapEither","") <?+>
     PU {
        unpickleTree = \t -> case unpickleTree pua t of
            Result val rest -> case a2b val of
                Left e -> UnpickleError . upe  $ "Function returned Left "
                                                 ++ show e
                Right r -> Result r rest
            NoResult e -> NoResult e
            UnpickleError e -> UnpickleError e
        ,
        pickleTree = pickleTree pua . b2a
    }

-- | Execute one of a list of picklers. The /selector function/ is used during
-- pickling, and the integer returned is taken as a 0-based index to select a
-- pickler from /pickler options/. Unpickling is done by trying each list
-- element in order until one returns a @Result@ (the /selector/ is not used).
--
-- This is typically used to handle each constructor of a data type. However,
-- it can be used wherever multiple serialization strategies apply to a single
-- type.
--
-- /NB/: This function will ignore all errors as long as one of the branches
-- returns a result. Also, it will produce an error when all branches return
-- @NoResult@.  Use 'xpChoice' for a saner version of this function.
xpAlt :: (a -> Int)  -- ^ Selector function
      -> [PU t a]    -- ^ List of picklers
      -> PU t a
xpAlt selector picklers = PU {
        unpickleTree = doUnpickle,
        pickleTree = \value -> pickleTree (picklers !! selector value) value
    }
  where
    eitherResult (Result r t) = Right (Result r t)
    eitherResult (UnpickleError e) = Left e
    eitherResult (NoResult e) = Left . missing $ Text.unpack e
    splitResults v = partitionEithers $ map (eitherResult . flip unpickleTree v)
                                     picklers
    doUnpickle v = case splitResults v of
        (_, Result r t:_) -> Result r t
        (es, []) -> ("xpAlt", "") <++.> UnpickleError (Variants es)
        _ -> error "xpAlt: splitResults returned impossible result"


-- | Execute one of a list of picklers. The /selector function/ is used during
-- pickling, and the integer returned is taken as a 0-based index to select a
-- pickler from /pickler options/. Unpickling is done by trying each list
-- element in order until one returns a @Result@ or an @Error@.
--
-- This is typically used to handle each constructor of a data type. However,
-- it can be used wherever multiple serialization strategies apply to a single
-- type.
--
-- This function is similar to 'xpAlt' but it will stop unpickling on the first
-- error. It will return @NoResult@ iff all of the picklers return @NoResult@
-- (or the list of picklers is empty).
xpChoice :: (a -> Int)  -- ^ Selector function
      -> [PU t a]    -- ^ List of picklers
      -> PU t a
xpChoice selector picklers =
    PU { unpickleTree = go picklers (1 :: Integer)
       , pickleTree = \value -> pickleTree (picklers !! selector value) value
       }
  where
    go [] _ _ = NoResult "entity"
    go (p:ps) i v = case unpickleTree p v of
        r@Result{} -> r
        UnpickleError e -> UnpickleError $ ("xpChoice", Text.pack $ show i)
                                           <++> e
        NoResult _ -> go ps (i+1) v

-- | Try the left pickler first and if that doesn't produce anything the right
-- one. Wrapping the result in @Left@ or @Right@, respectively.
--
-- Not to be confused with 'xpWrapEither'.
xpEither :: PU n t1 -> PU n t2 -> PU n (Either t1 t2)
xpEither xpl xpr = PU {
        unpickleTree = doUnpickle,
        pickleTree = \v -> case v of
          Left  l -> pickleTree xpl l
          Right r -> pickleTree xpr r
    }
  where
    doUnpickle t = case unpickleTree xpl t of
                    Result r s -> Result (Left r) s
                    NoResult e1 -> case unpickleTree xpr t of
                      Result r s -> Result (Right r) s
                      NoResult e2 -> UnpickleError $ ("xpEither","")
                                        <++> Variants [ missing $ Text.unpack e1
                                                      , missing $ Text.unpack e2
                                                      ]
                      UnpickleError e -> UnpickleError $ ("xpEither","Right")
                                           <++> e
                    UnpickleError e -> UnpickleError $ ("xpEither","Left")
                                           <++> e

-- | Pickler that during pickling always uses the first pickler, and during
-- unpickling tries the first, and on failure then tries the second.
xpTryCatch :: PU t a -> PU t a -> PU t a
xpTryCatch pu1 pu2 = PU
    { unpickleTree = \t -> case unpickleTree pu1 t of
             Result val1 rest -> Result val1 rest
             NoResult e1 -> case unpickleTree pu2 t of
                 Result val2 rest -> Result val2 rest
                 NoResult e2 -> NoResult $ Text.concat [e1, " / ", e2]
                 UnpickleError e2 -> UnpickleError $ ("xpTryCatch","Right")
                                    <++> e2
             UnpickleError e1 -> case unpickleTree pu2 t of
                 Result val2 rest -> Result val2 rest
                 NoResult e2 -> UnpickleError
                                $ Variants [ e1
                                           , upe $ " / not found:"
                                                   ++ Text.unpack e2
                                           ]
                 UnpickleError e2 -> UnpickleError $ ("xpTryCatch","")
                                <++> Variants [e1, e2]
    , pickleTree = pickleTree pu1
    }

-- | The zero pickler.
--
-- Encodes nothing, always fails during unpickling. (Same as @'xpThrow'
-- \"got xpZero\"@).
xpZero :: PU [t] a
xpZero = ("xpZero","") <?> xpThrow "got xpZero"

-- | No output when pickling, always generates an error with the specified
-- message on unpickling.
xpThrow :: Monoid m
        => String    -- ^ Error message
        -> PU m a
xpThrow msg = PU
  { unpickleTree = \_ -> UnpickleError $ ("xpThrow",Text.pack msg) <++> upe msg
  , pickleTree = const mempty
  }

-- | Add an attribute with a fixed value.
xpAddFixedAttr :: Name -> Text -> PU [Attribute] b -> PU [Attribute] b
xpAddFixedAttr name val pa
    = xpWrap snd ((,) ()) $
      xp2Tuple (xpAttrFixed name val) pa

xpFst :: Monoid b => PU t (a, b) -> PU t a
xpFst = xpWrap fst (\x -> (x, mempty))

xpSnd :: Monoid a => PU t (a, b) -> PU t b
xpSnd = xpWrap snd (\y -> (mempty, y))

-- | Instead of failing the pickler will return no result.
xpMayFail :: PU t a -> PU t a
xpMayFail xp = PU { pickleTree = pickleTree xp
                  , unpickleTree = \v -> case unpickleTree xp v of
                      UnpickleError _ -> NoResult "failed with xpMayFail"
                      x -> x
                  }

-- | Run unpickler and consume and discard remaining elements.
--
-- When pickling, this is a noop.
xpClean :: PU t a -> PU t a
xpClean xp = PU { unpickleTree = \x -> case unpickleTree xp x of
                     Result r _ -> Result r Nothing
                     e -> e
                , pickleTree = pickleTree xp
                }
