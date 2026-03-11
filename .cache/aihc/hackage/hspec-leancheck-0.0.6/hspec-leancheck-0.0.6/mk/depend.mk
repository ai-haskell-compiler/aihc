eg/minimal: \
  eg/minimal.hs \
  mk/toplibs
eg/minimal.o: \
  src/Test/Hspec/LeanCheck.hs \
  eg/minimal.hs
eg/should: \
  eg/should.hs \
  mk/toplibs
eg/should.o: \
  src/Test/Hspec/LeanCheck.hs \
  eg/should.hs
mk/All.o: \
  src/Test/Hspec/LeanCheck.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Test/Hspec/LeanCheck.hs \
  mk/Toplibs.hs
src/Test/Hspec/LeanCheck: \
  mk/toplibs
src/Test/Hspec/LeanCheck.o: \
  src/Test/Hspec/LeanCheck.hs
test/should.o: \
  test/should.hs \
  src/Test/Hspec/LeanCheck.hs
test/should: \
  test/should.hs \
  mk/toplibs
test/test.o: \
  test/test.hs \
  src/Test/Hspec/LeanCheck.hs
test/test: \
  test/test.hs \
  mk/toplibs
