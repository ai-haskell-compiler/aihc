eg/minimal: \
  eg/minimal.hs \
  mk/toplibs
eg/minimal.o: \
  src/Test/Framework/Providers/LeanCheck.hs \
  eg/minimal.hs
mk/All.o: \
  src/Test/Framework/Providers/LeanCheck.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Test/Framework/Providers/LeanCheck.hs \
  mk/Toplibs.hs
src/Test/Framework/Providers/LeanCheck: \
  mk/toplibs
src/Test/Framework/Providers/LeanCheck.o: \
  src/Test/Framework/Providers/LeanCheck.hs
test/test.o: \
  test/test.hs \
  src/Test/Framework/Providers/LeanCheck.hs
test/test: \
  test/test.hs \
  mk/toplibs
