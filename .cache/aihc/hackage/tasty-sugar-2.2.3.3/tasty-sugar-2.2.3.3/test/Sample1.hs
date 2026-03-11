{-# LANGUAGE QuasiQuotes #-}

module Sample1 where

import System.FilePath ( (</>) )

import Test.Tasty.Sugar

import Text.RawString.QQ


sample1 :: CUBE -> FilePath -> [CandidateFile]
sample1 cube testInpPath =
  fmap (makeCandidate cube testInpPath [])
  $ filter (not . null)
  $ lines [r|
global-max-good.c
global-max-good.ppc.o
global-max-good.ppc.exe
global-max-good.ppc.expected
global-max-good.x86.exe
global-max-good.x86.expected
jumpfar.c
jumpfar.h
jumpfar.ppc.exe
jumpfar.ppc.o
jumpfar.ppc.expected
jumpfar.x86.exe
jumpfar.x86.expected
looping.c
looping.ppc.exe
looping.ppc.expected
looping.x86.exe
looping.x86.expected
looping-around.c
looping-around.ppc.exe
looping-around.ppc.expected
looping-around.x86.exe
looping-around.expected
Makefile
README.org
switching
switching_stuff
switching.c
switching.h
switching.hh
switching_llvm.c
switching_llvm.h
switching_llvm.x86.exe
switching_many.c
switching_many_llvm.c
switching_many_llvm.x86.exe
switching_many.ppc.exe
switching.ppc.base-expected
switching.ppc.o
switching.ppc.base.o
switching.ppc.extra.o
switching.ppc.other
switching.ppc.exe
switching.x86.base-expected
switching.x86.exe
switching-refined.x86.o
switching.x86.orig
switching.x86.refined-expected
switching.x86.refined-expected-orig
switching.x86.refined-last-actual
tailrecurse.c
tailrecurse.expected
tailrecurse.expected.expected
tailrecurse.food.expected
tailrecurse.ppc.exe
tailrecurse.x86.exe
tailrecurse.x86.expected
|]
