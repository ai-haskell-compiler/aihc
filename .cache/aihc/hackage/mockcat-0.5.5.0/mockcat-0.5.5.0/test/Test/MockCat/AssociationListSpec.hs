{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.MockCat.AssociationListSpec (spec) where

import Test.Hspec
import Test.MockCat.AssociationList (AssociationList, empty, insert, update, (!?), member)

spec :: Spec
spec = do
  it "empty" do
    let e = empty :: AssociationList String Int
    e `shouldBe` []

  describe "insert" do
    it "not exist" do
      let l = empty :: AssociationList String Int
      insert "key" 100 l `shouldBe` [("key", 100)]

    it "exist" do
      let l = insert "key" 100 empty :: AssociationList String Int
      insert "key" 120 l `shouldBe` [("key", 120)]
  
  describe "update" do
    it "not exist" do
      let l = empty :: AssociationList String Int
      update (+ 20) "key" l `shouldBe` []

    it "exist" do
      let l = insert "key" 100 empty :: AssociationList String Int
      update (+ 20) "key" l `shouldBe` [("key", 120)]
  
  describe "lookup" do
    it "not exist" do
      let l = empty :: AssociationList String Int
      l !? "key" `shouldBe` Nothing

    it "exist" do
      let l = insert "key" 100 empty :: AssociationList String Int
      l !? "key" `shouldBe` Just 100

  describe "member" do
    it "not exist" do
      let l = empty :: AssociationList String Int
      member "key" l `shouldBe` False

    it "exist" do
      let l = insert "key" 100 empty :: AssociationList String Int
      member "key" l `shouldBe` True