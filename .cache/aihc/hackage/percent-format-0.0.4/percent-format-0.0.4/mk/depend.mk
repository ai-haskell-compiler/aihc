bench/examples: \
  bench/examples.hs \
  mk/toplibs
bench/examples.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  bench/examples.hs
bench/speculate: \
  bench/speculate.hs \
  mk/toplibs
bench/speculate.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  bench/speculate.hs
mk/All.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  mk/All.hs
mk/Toplibs.o: \
  test/Test.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  mk/Toplibs.hs
src/Text/PercentFormat: \
  mk/toplibs
src/Text/PercentFormat.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
src/Text/PercentFormat/Quotient.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Quotient.hs
src/Text/PercentFormat/Spec.o: \
  src/Text/PercentFormat/Spec.hs
src/Text/PercentFormat/Utils.o: \
  src/Text/PercentFormat/Utils.hs
test/number.o: \
  test/Test.hs \
  test/number.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
test/number: \
  test/Test.hs \
  test/number.hs \
  mk/toplibs
test/prop.o: \
  test/Test.hs \
  test/prop.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
test/prop: \
  test/Test.hs \
  test/prop.hs \
  mk/toplibs
test/quotient.o: \
  test/Test.hs \
  test/quotient.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Quotient.hs
test/quotient: \
  test/Test.hs \
  test/quotient.hs \
  mk/toplibs
test/scientific.o: \
  test/Test.hs \
  test/scientific.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
test/scientific: \
  test/Test.hs \
  test/scientific.hs \
  mk/toplibs
test/spec.o: \
  test/Test.hs \
  test/spec.hs \
  src/Text/PercentFormat/Spec.hs
test/spec: \
  test/Test.hs \
  test/spec.hs \
  mk/toplibs
test/Test.o: \
  test/Test.hs
test/unit.o: \
  test/unit.hs \
  test/Test.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
test/unit: \
  test/unit.hs \
  test/Test.hs \
  mk/toplibs
