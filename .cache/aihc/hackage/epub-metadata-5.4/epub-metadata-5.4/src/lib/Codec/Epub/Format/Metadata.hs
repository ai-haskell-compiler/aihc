{-# LANGUAGE FlexibleContexts #-}

{- | Module for pretty-printing epub metadata info

   Used internally by Codec.Epub.Format
-}
module Codec.Epub.Format.Metadata
   ( formatMetadata
   )
   where

import Control.Monad.Writer.Lazy ( MonadWriter, execWriter )
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Text.Printf ( printf )

import Codec.Epub.Format.Util
import Codec.Epub.Data.Metadata


tellTitle :: MonadWriter (Seq Char) m => Title -> m ()
tellTitle (Title Nothing Nothing Nothing text) =
   tellSeq $ printf "title: %s\n" text
tellTitle title =
   tellSeq $ printf "title\n%s%s%s%s"
      (formatSubline "text" (Just $ titleText title))
      (formatSubline "lang" (titleLang title))
      (formatSubline "title-type" (titleType title))
      (formatSubline "display-seq" (show `fmap` titleSeq title))


tellCreator :: MonadWriter (Seq Char) m => Creator -> m ()
tellCreator (Creator Nothing Nothing Nothing creator) =
   tellSeq $ printf "creator: %s\n" creator
tellCreator (Creator role fileAs dseq creator) =
   tellSeq $ printf "creator\n%s%s%s%s"
      (formatSubline "text" (Just creator))
      (formatSubline "file-as" fileAs)
      (formatSubline "role" role)
      (formatSubline "display-seq" (show `fmap` dseq))


tellContributor :: MonadWriter (Seq Char) m => Creator -> m ()
tellContributor (Creator Nothing Nothing Nothing contributor) =
   tellSeq $ printf "contributor: %s\n" contributor
tellContributor (Creator role fileAs dseq contributor) =
   tellSeq $ printf "contributor\n%s%s%s%s"
      (formatSubline "text" (Just contributor))
      (formatSubline "file-as" fileAs)
      (formatSubline "role" role)
      (formatSubline "display-seq" (show `fmap` dseq))


tellSource :: MonadWriter (Seq Char) m => Source -> m ()
tellSource (Source Nothing Nothing Nothing source) =
  tellSeq $ printf "source: %s\n" source
tellSource (Source idType' scheme sourceOf source) =
  tellSeq $ printf "source\n%s%s%s%s"
    (formatSubline "text" (Just source))
    (formatSubline "identifier-type" idType')
    (formatSubline "scheme" scheme)
    (formatSubline "source-of" sourceOf)


tellDate :: MonadWriter (Seq Char) m => (DateEvent, DateValue) -> m ()
tellDate (event', DateValue date') =
   tellSeq $ printf "date\n%s%s"
      (formatSubline "event" (Just . dateEventToString $ event'))
      (formatSubline "text" (Just date'))


tellId :: MonadWriter (Seq Char) m => Identifier -> m ()
tellId ident =
   tellSeq $ printf "identifier\n%s%s%s%s"
      (formatSubline "id" (idId ident))
      (formatSubline "identifier-type" (idType ident))
      (formatSubline "scheme" (idScheme ident))
      (formatSubline "text" (Just . idText $ ident))


tellDescription :: MonadWriter (Seq Char) m => Description -> m ()
tellDescription (Description Nothing text) =
   tellSeq $ printf "description: %s\n" text
tellDescription (Description lang text) =
   tellSeq $ printf "description\n%s%s"
      (formatSubline "lang" lang)
      (formatSubline "text" (Just text))


tellSimpleString :: MonadWriter (Seq Char) m => String -> String -> m ()
tellSimpleString label = tellSeq . (printf "%s: %s\n" label)


tellSimpleMbString :: MonadWriter (Seq Char) m => String
   -> Maybe String -> m ()
tellSimpleMbString _     Nothing  = return ()
tellSimpleMbString label (Just s) = tellSimpleString label s


tellMetadata :: MonadWriter (Seq Char) m => Metadata -> m ()
tellMetadata (Metadata ids titles langs contributors creators dates sources mType coverage desc format publisher relation rights subjects) = do
   mapM_ tellId ids
   mapM_ tellTitle titles
   mapM_ (tellSimpleString "language") langs
   mapM_ tellContributor contributors
   mapM_ tellCreator creators
   mapM_ tellDate $ Map.toList dates
   mapM_ tellSource sources
   tellSimpleMbString "type" mType
   mapM_ (tellSimpleString "coverage") coverage
   mapM_ tellDescription desc
   mapM_ (tellSimpleString "format") format
   mapM_ (tellSimpleString "publisher") publisher
   mapM_ (tellSimpleString "relation") relation
   mapM_ (tellSimpleString "rights") rights
   mapM_ (tellSimpleString "subject") subjects


{- | Format an epub Metadata structure for pretty printing
-}
formatMetadata :: Metadata -> String
formatMetadata meta = Foldable.toList . execWriter
   $ tellMetadata meta
