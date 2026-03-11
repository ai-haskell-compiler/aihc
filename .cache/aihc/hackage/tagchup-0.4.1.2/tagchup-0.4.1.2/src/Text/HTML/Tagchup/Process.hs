module Text.HTML.Tagchup.Process (
   Encoding, Encoded,
   evalDecodeAdaptive, decodeAdaptive, decodeTagAdaptive,
   getEmbeddedEncoding,
   getXMLEncoding,
   findMetaEncoding,
   getMetaHTTPHeaders,
   getHeadTags,
   partAttrs,
   parts,
   takeBeforeMatchingClose,
   takeUntilMatchingClose,
   ) where

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.HTML.Tagchup.Tag.Match as Match

import qualified Text.XML.Basic.ProcessingInstruction as PI
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Tag as TagX
import qualified Text.HTML.Basic.Tag as TagH
import qualified Text.HTML.Basic.Character as HTMLChar
import qualified Text.HTML.Basic.String as HTMLString

import Text.HTML.Basic.String (Encoded, )

import Control.Monad.Trans.State (State, put, get, evalState, )
import Control.Monad.HT ((<=<), )
import Control.Monad (msum, guard, )
import Control.Applicative ((<|>))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as ListMatch
import qualified Data.Foldable as Fold
import Data.Traversable (traverse, )
import Data.List.HT (viewL, takeUntil, switchR, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, mapMaybe, )


-- * analyse soup


type Encoding = String



evalDecodeAdaptive ::
   State (Encoded -> String) a -> a
evalDecodeAdaptive =
   flip evalState id

{- |
Selects a decoder dynamically according
to xml-encoding and meta-http-equiv tags.
The @?xml@ tag should only appear at the beginning of a document,
but we respect it at every occurence.

> import qualified Text.XML.HXT.DOM.Unicode as Unicode

> evalDecodeAdaptive .
> decodeAdaptive
>    (maybe Unicode.latin1ToUnicode (fst.) .
>     Unicode.getDecodingFct)
-}
decodeAdaptive ::
   (Name.Attribute name, Name.Tag name) =>
   (Encoding -> Encoded -> String) ->
   [Tag.T name [HTMLChar.T]] ->
   State (Encoded -> String) [Tag.T name String]
decodeAdaptive getDecoder =
   traverse (decodeTagAdaptive getDecoder)

{- |
@decodeTagAdaptive decoderSelector tag@ generates a state monad,
with a decoder as state.
It decodes encoding specific byte sequences
using the current decoder
and XML references using a fixed table.
-}
decodeTagAdaptive ::
   (Name.Attribute name, Name.Tag name) =>
   (Encoding -> Encoded -> String) ->
   Tag.T name [HTMLChar.T] ->
   State (Encoded -> String) (Tag.T name String)
decodeTagAdaptive getDecoder tag0 =
   do decoder <- get
      let tag1 =
             -- this is less elegant than using maybeCData but lazier
             maybe
                (fmap (HTMLString.decode decoder) tag0)
                (\(name, s) ->
                   Tag.special name $
                      if TagH.cdataName == name
                        then decoder s
                        else s)
                (Tag.maybeSpecial tag0)
      Fold.mapM_ (put . getDecoder) $
         (do openTag <- Tag.maybeOpen tag1
             uncurry TagH.maybeMetaEncoding openTag <|>
                uncurry TagH.maybeMetaCharset openTag)
         <|>
         (uncurry TagX.maybeXMLEncoding =<< Tag.maybeProcessing tag1)
      return tag1


getEmbeddedEncoding ::
   (Name.Attribute name, Name.Tag name) =>
   [Tag.T name String] -> Maybe Encoding
getEmbeddedEncoding leadingTags =
   let xmlEncoding = do
         (t,_) <- viewL leadingTags
         (name, PI.Known attrs) <- Tag.maybeProcessing t
         guard (TagX.xmlName == name)
         Attr.lookup Attr.encodingName attrs

   in  msum $
         xmlEncoding :
         map
            (\tag ->
               uncurry TagH.maybeMetaCharset tag <|>
               uncurry TagH.maybeMetaEncoding tag)
            (mapMaybe Tag.maybeOpen $ getHeadTags leadingTags)


{- |
Check whether the first tag is an @xml@ processing instruction tag
and return the value of its @encoding@ attribute.
-}
getXMLEncoding ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag.T name String] -> Maybe String
getXMLEncoding tags =
   do (t,_) <- viewL tags
      uncurry TagX.maybeXMLEncoding =<< Tag.maybeProcessing t

{- |
Rather the same as @wraxml:HTML.Tree.findMetaEncoding@
-}
findMetaEncoding ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag.T name String] -> Maybe String
findMetaEncoding =
   msum .
   map (uncurry TagH.maybeMetaEncoding <=< Tag.maybeOpen) .
   getHeadTags


{- |
Extract META tags which contain HTTP-EQUIV attribute
and present these values like HTTP headers.
-}
getMetaHTTPHeaders ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag.T name string] -> [(string, string)]
getMetaHTTPHeaders =
   mapMaybe (uncurry TagH.maybeMetaHTTPHeader <=< Tag.maybeOpen) .
   getHeadTags


getHeadTags ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag.T name string] -> [Tag.T name string]
getHeadTags =
   takeWhile (not . Match.closeLit "head") .
   drop 1 .
   dropWhile (not . Match.openNameLit "head") .
   takeWhile (not . Match.openNameLit "body")


-- * transform soup

{- |
Modify attributes and tags of certain parts.
For limitations, see 'parts'.
-}
partAttrs ::
   (Name.Tag name) =>
   (Tag.Name name -> Bool) ->
   (([Attr.T name string], [Tag.T name string]) ->
    ([Attr.T name string], [Tag.T name string])) ->
   [Tag.T name string] -> [Tag.T name string]
partAttrs p f =
   concatMap
      (either
          (\((name,attrs),part) ->
              let (newAttrs, newPart) = f (attrs, part)
              in  Tag.Open name newAttrs : newPart ++ [Tag.Close name])
          id) .
   parts p

{- |
Extract parts from the tag soup
that are enclosed in corresponding open and close tags.
If a close tag is missing, the soup end is considered as end of the part.
However nested tags are not supported,
e.g. in @\<a\>\<a\>\<\/a\>\<\/a\>@ the second @\<a\>@ is considered
to be enclosed in the first @\<a\>@ and the first @\<\/a\>@,
and the second @\<\/a\>@ is ignored.
-}
parts ::
   (Name.Tag name) =>
   (Tag.Name name -> Bool) ->
   [Tag.T name string] ->
   [Either
       ((Tag.Name name, [Attr.T name string]), [Tag.T name string])
       [Tag.T name string]]
parts p =
   let recourse ts =
          let (prefix0,suffix0) = break (Match.open p (const True)) ts
          in  Right prefix0 :
              fromMaybe []
                 (do (t, suffix1) <- viewL suffix0
                     (name, attrs) <- Tag.maybeOpen t
                     let (part,suffix2) = break (Match.close (name==)) suffix1
                     return $ Left ((name, attrs), part) : recourse (drop 1 suffix2))
   in  recourse



nestDiff :: (Eq name) => TagH.Name name -> Tag.T name string -> Int
nestDiff name tag =
   fromEnum (Match.open (name==) (const True) tag)
   -
   fromEnum (Match.close (name==) tag)

countNesting :: (a -> Int) -> [a] -> [Int]
countNesting p = NonEmpty.tail . NonEmpty.scanl (+) 0 . map p


{-
Could be moved to utility-ht, but there seem to be several useful variants
with respect to whether opening and closing element should be included.
-}
{- |
> Process> let parenDiff c = case c of '(' -> 1; ')' -> -1; _ -> 0
> Process> takeBeforeMatch parenDiff "((abc)de)f"
> "((abc)de"
-}
takeBeforeMatch :: (a -> Int) -> [a] -> [a]
takeBeforeMatch p xs =
   flip ListMatch.take xs $ takeWhile (>0) $ countNesting p xs

{- |
Take all tags until the one that matches the opening tag.
The matching closing tag is not included in the list.
The list must begin with the according opening tag.
Nesting of the considered tag is respected,
but the nesting of other tags is ignored.
-}
takeBeforeMatchingClose ::
   (Eq name) => TagH.Name name -> [Tag.T name string] -> [Tag.T name string]
takeBeforeMatchingClose name = takeBeforeMatch $ nestDiff name


{- |
> Process> takeUntilMatch parenDiff "((abc)de)f"
> Just "((abc)de)"
-}
takeUntilMatch :: (a -> Int) -> [a] -> Maybe [a]
takeUntilMatch p xs =
   (\ys -> switchR (Just []) (\_ y -> toMaybe (y==0) $ ListMatch.take ys xs) ys) $
   takeUntil (==0) $ countNesting p xs

{- |
This is like 'takeBeforeMatchingClose'
but the matching close tag is included in the result.
-}
takeUntilMatchingClose ::
   (Eq name) =>
   TagH.Name name -> [Tag.T name string] -> Maybe [Tag.T name string]
takeUntilMatchingClose name = takeUntilMatch $ nestDiff name
