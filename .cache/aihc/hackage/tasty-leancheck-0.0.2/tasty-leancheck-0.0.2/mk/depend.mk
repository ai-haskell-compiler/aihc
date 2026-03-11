eg/minimal: \
  eg/minimal.hs \
  mk/toplibs
eg/minimal.o: \
  src/Test/Tasty/LeanCheck.hs \
  eg/minimal.hs
mk/All.o: \
  src/Test/Tasty/LeanCheck.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Test/Tasty/LeanCheck.hs \
  mk/Toplibs.hs
src/Test/Tasty/LeanCheck: \
  mk/toplibs
src/Test/Tasty/LeanCheck.o: \
  src/Test/Tasty/LeanCheck.hs
test/test.o: \
  test/test.hs \
  src/Test/Tasty/LeanCheck.hs
test/test: \
  test/test.hs \
  mk/toplibs
