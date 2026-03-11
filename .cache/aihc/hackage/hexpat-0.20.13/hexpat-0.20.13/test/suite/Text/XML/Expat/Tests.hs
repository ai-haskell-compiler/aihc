{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}

module Text.XML.Expat.Tests
  ( TCursor
  , TNode
  , testTagSet
  , testTextSet
  , testAttrSet )
where

import           Control.Applicative
import           Control.Monad (liftM)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M
import           Test.QuickCheck
import           Text.XML.Expat.Cursor (Cursor)
import           Text.XML.Expat.Tree

------------------------------------------------------------------------------

type TCursor = Cursor ByteString ByteString
type TNode = Node ByteString ByteString


testTagSet :: [ByteString]
testTagSet = [ "apple"
             , "banana"
             , "cauliflower"
             , "duck"
             , "eel"
             , "ferret"
             , "grape" ]

testTextSet :: [ByteString]
testTextSet = [ "zoo"
              , "yellow"
              , "xylophone"
              , "wet"
              , "vulture"
              , "ululate"
              , "tympani" ]

testAttrSet :: [ByteString]
testAttrSet = [ "sheep"
              , "ram"
              , "quail"
              , "penguin"
              , "ox"
              , "narwhal" ]


instance Arbitrary TNode where
    arbitrary = mkElem 0
      where
        depth :: Int -> Gen TNode
        depth n = do
            prob <- (choose (0, 1) :: Gen Float)
            if prob < 0.75 then mkElem n else mkText


        mkAttr = do
            key <- elements testAttrSet
            val <- elements testAttrSet
            return (key,val)

        mkText = liftM Text $ elements testTextSet

        mkElem n = do
            nchildren <- if n > 3
                           then return 0
                           else choose ((0,6) :: (Int,Int))
            nattrs    <- choose ((0,4) :: (Int,Int))
            attrs     <- M.toList . M.fromList    -- remove duplicate attributes
                      <$> sequence (replicate nattrs mkAttr)
            children  <- sequence $ replicate nchildren (depth (n+1))
            tagname   <- elements testTagSet

            return $ Element tagname attrs children

