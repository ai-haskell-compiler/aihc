-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Internal.FFI (
  -- * Invokers
    ffienumfloatIOV
  , ffienumfixedIOV
  , ffiuintIOV
  , ffiuintuintIOV
  , ffienumIOV
  , ffiuintPtrcharIOV
  , ffienumuintuintuintuintuintIOV
  , ffienumuintuintuintuintuintuintuintuintIOV
  , ffienumuintuintuintuintuintuintuintuintuintuintuintIOV
  , ffienumclampfIOV
  , ffiIOV
  , ffisizeiPtruintPtrbooleanIOboolean
  , ffiintIOV
  , ffienumintenumsizeiuintuintIOV
  , ffihandleARBhandleARBIOV
  , ffiuintenumIOV
  , ffienumuintIOV
  , ffienumuintuintIOV
  , ffiuintuintPtrcharIOV
  , ffihandleARBuintPtrcharARBIOV
  , ffienumuintuintintptrIOV
  , ffienumuintuintintptrsizeiptrIOV
  , ffienumuintsizeiPtruintIOV
  , ffienumuintsizeiPtruintPtrintptrPtrsizeiptrIOV
  , ffiuintuintuintPtrcharIOV
  , ffiuintuintintbooleanintenumenumIOV
  , ffiuintuintintbooleanintenumintIOV
  , ffiuintsizeiPtruintIOV
  , ffienumenumIOuint
  , ffienumenumuintIOV
  , ffienumIOuint
  , ffienumenumenumIOuint
  , ffiuintuintintptrsizeiIOV
  , ffiuintsizeiPtruintPtrintptrPtrsizeiIOV
  , ffiuintuintenumintptrARBIOV
  , ffiuintuintenumenumuintIOV
  , ffibytebytebyteIOV
  , ffiPtrbyteIOV
  , ffidoubledoubledoubleIOV
  , ffiPtrdoubleIOV
  , ffifloatfloatfloatIOV
  , ffiPtrfloatIOV
  , ffiintintintIOV
  , ffiPtrintIOV
  , ffishortshortshortIOV
  , ffiPtrshortIOV
  , ffienumsizeiPtrVIOV
  , ffisizeisizeifloatfloatfloatfloatPtrubyteIOV
  , ffisizeisizeifixedfixedfixedfixedPtrubyteIOV
  , ffifloatfloatfloatfloatIOV
  , ffifixedfixedfixedfixedIOV
  , ffienumenumIOV
  , ffiuintenumenumIOV
  , ffienumenumenumenumIOV
  , ffiuintenumenumenumenumIOV
  , ffienumintIOV
  , ffiintintintintintintintintbitfieldenumIOV
  , ffiuintuintintintintintintintintintbitfieldenumIOV
  , ffienumuintuint64EXTsizeiptrIOV
  , ffienumsizeiptrPtrVenumIOV
  , ffienumsizeiptrARBPtrVenumIOV
  , ffienumintptrsizeiptrbooleanIOV
  , ffienumenumintIOV
  , ffienumsizeiptrPtrVbitfieldIOV
  , ffienumintptrsizeiptrPtrVIOV
  , ffienumintptrARBsizeiptrARBPtrVIOV
  , ffisizeienumPtrVIOV
  , ffienumIOenum
  , ffiuintenumIOenum
  , ffibitfieldIOV
  , ffienumenumenumenumPtrVIOV
  , ffienumenumintptrsizeiptrenumenumPtrVIOV
  , ffienumintfloatintIOV
  , ffienumintPtrfloatIOV
  , ffienumintPtrintIOV
  , ffienumintPtruintIOV
  , ffiintintintintIOV
  , ffiuintuintuintuintIOV
  , ffidoubleIOV
  , ffifloatIOV
  , fficlampfIOV
  , ffifixedIOV
  , ffiuintenumenumenumPtrVIOV
  , ffiuintenumintptrsizeiptrenumenumPtrVIOV
  , ffiuintenumsizeiptrsizeiptrenumenumPtrVIOV
  , ffiuintenumintfloatintIOV
  , ffiuintenumintPtrfloatIOV
  , ffiuintenumintPtrintIOV
  , ffiuintenumintPtruintIOV
  , ffisizeisizeiPtruintIOV
  , ffiuintintenumenumPtrVIOV
  , ffiuintintintintintsizeisizeisizeienumenumPtrVIOV
  , ffisyncbitfielduint64IOenum
  , ffienumPtrdoubleIOV
  , ffienumPtrfloatIOV
  , ffienumPtrfixedIOV
  , ffifloatfloatfloatfloatfloatfloatIOV
  , ffiPtrfloatPtrfloatIOV
  , ffihalfNVhalfNVhalfNVIOV
  , ffiPtrhalfNVIOV
  , ffiubyteubyteubyteIOV
  , ffiPtrubyteIOV
  , ffiuintuintuintIOV
  , ffiPtruintIOV
  , ffiushortushortushortIOV
  , ffiPtrushortIOV
  , ffifixedfixedfixedIOV
  , ffiPtrfixedIOV
  , ffibytebytebytebyteIOV
  , ffidoubledoubledoubledoubleIOV
  , ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffiPtrfloatPtrfloatPtrfloatIOV
  , ffihalfNVhalfNVhalfNVhalfNVIOV
  , ffishortshortshortshortIOV
  , ffiubyteubyteubyteubyteIOV
  , ffiubyteubyteubyteubytefloatfloatIOV
  , ffiPtrubytePtrfloatIOV
  , ffiubyteubyteubyteubytefloatfloatfloatIOV
  , ffiushortushortushortushortIOV
  , ffiintenumsizeiIOV
  , ffienumuintuintuintuintuintuintIOV
  , ffienumuintuintuintuintuintuintuintuintuintIOV
  , ffienumuintuintuintuintuintuintuintuintuintuintuintuintIOV
  , ffibooleanbooleanbooleanbooleanIOV
  , ffiuintbooleanbooleanbooleanbooleanIOV
  , ffienumPtruintIOV
  , ffiintenumsizeiPtrVIOV
  , ffiintenumsizeisizeiPtrVIOV
  , ffiintenumintPtrPtrVintIOV
  , ffiintenumPtrPtrVIOV
  , ffienumsizeisizeienumenumPtrVIOV
  , ffienumenumsizeienumenumPtrVIOV
  , ffienumenumPtrfloatIOV
  , ffienumenumPtrintIOV
  , ffienumenumenumenumenumenumIOV
  , ffienumenumenumenumenumenumenumbooleanbooleanbooleanIOV
  , ffienumPtrintIOV
  , ffihandleARBIOV
  , ffiuintsizeiPtrPtrcharPtrintIOV
  , ffienumenumintenumsizeiintsizeiPtrVIOV
  , ffienumenumintenumsizeisizeiintsizeiPtrVIOV
  , ffienumenumintenumsizeisizeisizeiintsizeiPtrVIOV
  , ffienumenumintintsizeienumsizeiPtrVIOV
  , ffienumenumintintintsizeisizeienumsizeiPtrVIOV
  , ffienumenumintintintintsizeisizeisizeienumsizeiPtrVIOV
  , ffienumintenumsizeiintsizeiPtrVIOV
  , ffienumintenumsizeisizeiintsizeiPtrVIOV
  , ffienumintenumsizeisizeisizeiintsizeiPtrVIOV
  , ffienumintintsizeienumsizeiPtrVIOV
  , ffienumintintintsizeisizeienumsizeiPtrVIOV
  , ffienumintintintintsizeisizeisizeienumsizeiPtrVIOV
  , ffiuintenumintenumsizeiintsizeiPtrVIOV
  , ffiuintenumintenumsizeisizeiintsizeiPtrVIOV
  , ffiuintenumintenumsizeisizeisizeiintsizeiPtrVIOV
  , ffiuintintintsizeienumsizeiPtrVIOV
  , ffiuintenumintintsizeienumsizeiPtrVIOV
  , ffiuintintintintsizeisizeienumsizeiPtrVIOV
  , ffiuintenumintintintsizeisizeienumsizeiPtrVIOV
  , ffiuintintintintintsizeisizeisizeienumsizeiPtrVIOV
  , ffiuintenumintintintintsizeisizeisizeienumsizeiPtrVIOV
  , ffienumenumsizeisizeienumenumPtrVIOV
  , ffienumenumfloatIOV
  , ffienumenumfixedIOV
  , ffienumenumPtrfixedIOV
  , ffienumenumintptrintptrsizeiptrIOV
  , ffienumsizeiintintsizeiIOV
  , ffienumenumintintsizeiIOV
  , ffienumenumintintsizeisizeiIOV
  , ffiuintenumintintintintuintenumintintintintsizeisizeisizeiIOV
  , ffienumenumintenumintintsizeiintIOV
  , ffienumenumintenumintintsizeisizeiintIOV
  , ffienumenumintintintintsizeiIOV
  , ffienumenumintintintintintsizeisizeiIOV
  , ffienumenumintintintintintintsizeisizeiIOV
  , ffiuintuintintptrintptrsizeiptrIOV
  , ffiintintsizeisizeienumIOV
  , ffienumintenumintintsizeiintIOV
  , ffienumintenumintintsizeisizeiintIOV
  , ffienumintintintintsizeiIOV
  , ffienumintintintintintsizeisizeiIOV
  , ffienumintintintintintintsizeisizeiIOV
  , ffiuintenumintenumintintsizeiintIOV
  , ffiuintenumintenumintintsizeisizeiintIOV
  , ffiuintuintintsizeiIOV
  , ffiuintintintintintsizeiIOV
  , ffiuintenumintintintintsizeiIOV
  , ffiuintintintintintintsizeisizeiIOV
  , ffiuintenumintintintintintsizeisizeiIOV
  , ffiuintintintintintintintsizeisizeiIOV
  , ffiuintenumintintintintintintsizeisizeiIOV
  , ffisizeienumPtrVuintenumenumPtrfloatIOV
  , ffibooleanIOV
  , ffisizeiPtrfloatIOV
  , ffisizeiPtruintIOV
  , ffiuintPtruintIOV
  , ffiIOuint
  , ffiIOhandleARB
  , ffienumsizeiPtruintIOV
  , ffienumIOhandleARB
  , ffienumPtrcharIOuint
  , ffienumsizeiPtrPtrcharIOuint
  , ffiPtrVPtrVbitfieldIOsync
  , ffiDEBUGPROCPtrVIOV
  , ffiDEBUGPROCAMDPtrVIOV
  , ffiDEBUGPROCARBPtrVIOV
  , ffiDEBUGPROCKHRPtrVIOV
  , ffienumenumenumsizeiPtruintbooleanIOV
  , ffienumenumsizeiPtruintbooleanIOV
  , ffienumenumuintenumsizeiPtrcharIOV
  , ffienumenumuintsizeiPtrcharIOV
  , ffienumdoubledoubleintintdoubledoubleintintdoubledoubleintintPtrdoubleIOV
  , ffienumfloatfloatintintfloatfloatintintfloatfloatintintPtrfloatIOV
  , ffiuintsizeiIOV
  , ffiintPtrcharIOV
  , ffienumuintPtruintIOV
  , ffisyncIOV
  , fficlampdclampdIOV
  , ffidoubledoubleIOV
  , ffiuintsizeiPtrfloatIOV
  , ffiuintsizeiPtrdoubleIOV
  , ffiuintdoubledoubleIOV
  , ffiuintfloatfloatIOV
  , ffifloatfloatIOV
  , fficlampfclampfIOV
  , ffifixedfixedIOV
  , ffienumsizeiPtrfloatIOV
  , ffienumsizeiPtrenumIOV
  , ffiuintuintuintuintuintuintIOV
  , ffiintptrIOV
  , ffienumintsizeiIOV
  , ffienumPtrVIOV
  , ffienumintsizeisizeiIOV
  , ffienumintsizeisizeiuintIOV
  , ffisizeiPtrenumIOV
  , ffiintPtrenumPtrintIOV
  , ffienumPtruint64PtrsizeiuintIOV
  , ffienumuintPtrintptrPtrsizeiuintIOV
  , ffiPtruint64PtrsizeiPtruintPtruintuintIOV
  , ffiuintPtrintptrPtrsizeiPtruintPtruintuintIOV
  , ffienumsizeiIOV
  , ffienumsizeienumPtrVIOV
  , ffienumsizeienumPtrVintIOV
  , ffienumenumPtrVIOV
  , ffienumsizeienumPtrVsizeiIOV
  , ffienumsizeienumPtrVsizeiuintIOV
  , ffienumsizeienumPtrVsizeiintIOV
  , ffienumsizeienumPtrVsizeiintuintIOV
  , ffisizeisizeienumenumPtrVIOV
  , ffienumuintuintintsizeiIOV
  , ffienumuintuintsizeiIOV
  , ffienumuintuintsizeienumPtrVIOV
  , ffienumuintuintsizeienumPtrVintIOV
  , ffifloatfloatfloatfloatfloatIOV
  , ffiintintintintintIOV
  , ffishortshortshortshortshortIOV
  , ffiuintuintfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffifixedfixedfixedfixedfixedIOV
  , ffienumuintsizeiIOV
  , ffienumeglImageOESIOV
  , ffisizeiIOV
  , ffisizeiPtrVIOV
  , ffisizeisizeiPtrbooleanIOV
  , ffiintPtrPtrbooleanintIOV
  , ffiPtrbooleanIOV
  , ffienumintintIOV
  , ffienumintintintintIOV
  , ffiintintIOV
  , ffienumuintPtrfloatIOV
  , ffienumPtrPtrVIOV
  , ffiPtruintintPtrintIOV
  , ffiuintenumPtrcharPtrintIOV
  , ffiuintenumintenumPtrintIOV
  , ffienumintintintintsizeisizeisizeienumenumPtrVIOV
  , ffiuintIOboolean
  , ffisizeienumPtrfloatIOV
  , ffisizeienumPtrfixedIOV
  , ffienumbitfieldIOsync
  , ffiPtruintIOint
  , ffienumintptrsizeiptrIOV
  , ffiuintintptrsizeiptrIOV
  , ffienumintPtrPtrVintIOV
  , ffihalfNVIOV
  , ffiuintsizeiPtrenumIOV
  , ffienumenumenumuintIOV
  , ffienumuintsizeiPtrfloatIOV
  , ffienumuintuintPtrfloatIOV
  , ffienumenumuintintIOV
  , ffienumenumenumuintintIOV
  , ffienumenumenumuintintintintIOV
  , ffienumenumenumuintintsizeiIOV
  , ffienumenumenumuintintintIOV
  , ffienumenumuintintenumIOV
  , ffienumenumuintintintIOV
  , ffienumenumuintintintintintIOV
  , ffienumenumuintintsizeiintsizeiIOV
  , ffienumenumuintintintsizeiIOV
  , ffidoubledoubledoubledoubledoubledoubleIOV
  , ffifixedfixedfixedfixedfixedfixedIOV
  , ffisizeiIOuint
  , ffiuintIOuint
  , ffienumenumenumuintIOuint
  , ffiuintuintenumPtrintIOV
  , ffiuintuintsizeiPtrsizeiPtrintPtrenumPtrcharIOV
  , ffihandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV
  , ffiuintenumuintsizeiPtrsizeiPtrcharIOV
  , ffiuintenumuintenumPtrintIOV
  , ffiuintuintsizeiPtrsizeiPtrcharIOV
  , ffiuintsizeiPtruintenumPtrintIOV
  , ffiuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV
  , ffihandleARBsizeiPtrsizeiPtrhandleARBIOV
  , ffiuintsizeiPtrsizeiPtruintIOV
  , ffiuintPtrcharIOint
  , ffihandleARBPtrcharARBIOint
  , ffienumuintPtrbooleanIOV
  , ffienumPtrbooleanIOV
  , ffienumenumPtrint64IOV
  , ffienumenumPtruint64EXTIOV
  , ffienumenumPtrPtrVIOV
  , ffienumenumenumPtrVIOV
  , ffienumenumenumenumPtrfloatIOV
  , ffienumenumenumenumPtrintIOV
  , ffienumenumenumPtrfloatIOV
  , ffienumenumenumPtrintIOV
  , ffienumuintIOuint
  , ffienumenumintPtrVIOV
  , ffienumintPtrVIOV
  , ffiuintintsizeiPtrVIOV
  , ffiuintenumintPtrVIOV
  , ffiuintintintintintsizeisizeisizeisizeiPtrVIOV
  , ffiuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint
  , ffiuintsizeiPtrenumPtruintPtruintPtrsizeiPtrcharIOuint
  , ffienumuintPtrdoubleIOV
  , ffiuintsizeiPtrsizeiPtrcharIOV
  , ffiPtrintsizeiPtruintIOV
  , ffiIOenum
  , ffiuintenumPtrintIOV
  , ffienumenumuintuintsizeiPtrfloatIOV
  , ffiuintIOsizei
  , ffienumbooleanenumenumPtrVIOV
  , ffiuintintbooleanintenumIOuint64
  , ffihandleARBsizeiPtrsizeiPtrcharARBIOV
  , ffiIOint
  , ffienumuintPtrint64IOV
  , ffienumPtrint64IOV
  , ffienumuintPtrintIOV
  , ffienumuintPtruint64EXTIOV
  , ffienumPtruint64EXTIOV
  , ffienumenumsizeienumsizeiPtrintIOV
  , ffienumenumenumsizeiPtrint64IOV
  , ffienumenumenumsizeiPtrintIOV
  , ffiuintenumPtrbooleanIOV
  , ffiuintenumPtrfloatIOV
  , ffienumuintenumPtrfloatIOV
  , ffienumuintenumPtrintIOV
  , ffienumuintenumsizeisizeibooleanPtrVIOV
  , ffienumenumPtrdoubleIOV
  , ffienumenumenumPtrdoubleIOV
  , ffienumenumintenumenumPtrVIOV
  , ffienumenumintenumPtrfloatIOV
  , ffienumenumintenumPtrintIOV
  , ffienumenumenumPtruintIOV
  , ffiuintenumPtrint64IOV
  , ffiuintenumPtruint64EXTIOV
  , ffiuintenumPtrPtrVIOV
  , ffiuintintptrsizeiptrPtrVIOV
  , ffiuintenumenumPtrintIOV
  , ffiuintenumuintPtrintIOV
  , ffiuintenumuintPtruintIOV
  , ffiuintenumuintPtrdoubleIOV
  , ffiuintenumuintPtrfloatIOV
  , ffiuintenumenumPtrVIOV
  , ffiintPtrcharsizeiPtrintPtrcharIOV
  , ffiintPtrcharenumPtrintIOV
  , ffienumuintsizeiPtrsizeiPtrcharIOV
  , ffihandleARBenumPtrfloatIOV
  , ffihandleARBenumPtrintIOV
  , ffiPtrVsizeiPtrsizeiPtrcharIOV
  , ffiuintenumPtruintIOV
  , ffiuintPtrubyteIOV
  , ffiuintPtrfloatIOV
  , ffiuintsizeisizeiIOfloat
  , ffibitfielduintsizeisizeiPtrfloatIOV
  , ffibitfieldsizeienumPtrVuintsizeiPtrfloatIOV
  , ffienumsizeienumPtrVuintfloatfloatenumPtrfloatIOV
  , ffiuintuintuintPtrcharuintPtrcharPtruintPtruintPtruintPtruintPtruint64IOV
  , ffiuintenumsizeiPtruintPtrintIOV
  , ffiuintuintenumPtrVIOV
  , ffiuintPtrintPtrintsizeiPtruintIOV
  , ffiuintuintsizeiPtrVPtruintIOV
  , ffiPtrcharPtruintIOV
  , ffiuintuintPtrcharPtruintPtruintPtruintPtruintIOV
  , ffienumPtrushortIOV
  , ffienumintPtrfixedIOV
  , ffienumuintPtrPtrVIOV
  , ffiuintsizeiPtrsizeiPtrenumPtrVIOV
  , ffiuintsizeiPtrubytePtrdoubleIOV
  , ffiuintsizeiPtrubytePtrfloatIOV
  , ffienumuintenumPtrdoubleIOV
  , ffiuintenumPtrcharIOuint
  , ffiuintenumPtrcharIOint
  , ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrfloatIOV
  , ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrintIOV
  , ffiuintenumPtrubyteIOV
  , ffiuintuintenumintptrIOV
  , ffiuintenumPtruint64IOV
  , ffienumenumenumPtrVPtrVPtrVIOV
  , ffienumenumPtrintPtrintIOV
  , ffienumIOushort
  , ffienumIOPtrubyte
  , ffienumuintIOPtrubyte
  , ffisyncenumsizeiPtrsizeiPtrintIOV
  , ffienumintenumenumPtrVIOV
  , ffienumintenumPtrfloatIOV
  , ffienumintenumPtrintIOV
  , ffienumintenumPtrfixedIOV
  , ffienumenumPtruintIOV
  , ffiuintIOuint64
  , ffiuintintenumenumsizeiPtrVIOV
  , ffiuintenumintenumenumPtrVIOV
  , ffiuintintenumPtrfloatIOV
  , ffiuintenumintenumPtrfloatIOV
  , ffiuintintenumPtrintIOV
  , ffiuintenumenumPtruintIOV
  , ffiuintenumenumPtrfloatIOV
  , ffiuintuintIOuint64
  , ffiuintintintintintsizeisizeisizeienumenumsizeiPtrVIOV
  , ffiuintuintPtrintIOV
  , ffiuintenumuintPtrint64IOV
  , ffiuintPtrcharIOuint
  , ffiuintintIOint
  , ffiuintsizeiPtrPtrcharPtruintIOV
  , ffiuintintIOintptr
  , ffiuintintPtrdoubleIOV
  , ffiuintintPtrfloatIOV
  , ffihandleARBintPtrfloatIOV
  , ffiuintintPtrint64IOV
  , ffiuintintPtrint64EXTIOV
  , ffiuintintPtrintIOV
  , ffihandleARBintPtrintIOV
  , ffiuintintPtruint64IOV
  , ffiuintintPtruint64EXTIOV
  , ffiuintintPtruintIOV
  , ffiuintuintenumPtrint64IOV
  , ffiuintuintenumPtrPtrVIOV
  , ffiuintenumPtrdoubleIOV
  , ffiuintenumPtrint64EXTIOV
  , ffiuintuintenumPtrdoubleIOV
  , ffiuintuintenumPtrfloatIOV
  , ffienumenumenumsizeiPtrVIOV
  , ffienumintsizeiPtrVIOV
  , ffienumbooleanenumenumsizeiPtrVIOV
  , ffienumenumsizeiPtrdoubleIOV
  , ffienumenumsizeiPtrfloatIOV
  , ffienumenumsizeiPtrintIOV
  , ffienumsizeiPtrushortIOV
  , ffisizeiPtrubyteIOV
  , ffienumenumenumsizeiPtrVsizeiPtrVPtrVIOV
  , ffienumintenumenumsizeiPtrVIOV
  , ffiuintintsizeiPtrdoubleIOV
  , ffiuintintsizeiPtrfloatIOV
  , ffiuintintsizeiPtrint64IOV
  , ffiuintintsizeiPtrintIOV
  , ffiuintintsizeiPtruint64IOV
  , ffiuintintsizeiPtruintIOV
  , ffibyteIOV
  , ffishortIOV
  , ffiubyteIOV
  , ffiushortIOV
  , ffienumsizeienumbooleanIOV
  , ffienumintptrbitfieldIOsync
  , ffienumsizeisizeiPtrVIOV
  , ffisizeiPtrcharIOV
  , ffisizeiPtrintIOV
  , ffiuintuintuintfloatIOV
  , ffiuintsizeiPtrenumintintsizeisizeiIOV
  , ffienumsizeiPtrenumintintsizeisizeiIOV
  , ffiuintintIOV
  , ffiuintintintintintsizeisizeisizeiIOV
  , ffienumIOboolean
  , ffienumuintIOboolean
  , ffiuint64IOboolean
  , ffiintPtrcharIOboolean
  , ffiuintuintfloatfloatIOboolean
  , ffiuintfloatfloatIOboolean
  , ffisyncIOboolean
  , ffiuintenumIOboolean
  , ffienumuintsizeiPtrcharIOV
  , ffiintushortIOV
  , ffiuintuintPtrPtrVPtrsizeiPtruintPtruintuintIOV
  , ffiuintenumfloatIOV
  , ffiuintenumintIOV
  , ffienumuintsizeiPtrubyteIOV
  , ffiintsizeiIOV
  , ffiuint64IOV
  , ffiuint64enumIOV
  , ffienumdoubledoubleintintPtrdoubleIOV
  , ffienumfloatfloatintintPtrfloatIOV
  , ffienumfixedfixedintintfixedIOV
  , ffienumdoubledoubleintintdoubledoubleintintPtrdoubleIOV
  , ffienumfloatfloatintintfloatfloatintintPtrfloatIOV
  , ffienumfixedfixedintintfixedfixedintintfixedIOV
  , ffienumenumIOPtrV
  , ffienumintptrsizeiptrbitfieldIOPtrV
  , ffienumuintenumsizeisizeiintintbooleanPtrVIOV
  , ffiintdoubledoubleIOV
  , ffiintfloatfloatIOV
  , ffiintfixedfixedIOV
  , ffiintdoubledoubleintdoubledoubleIOV
  , ffiintfloatfloatintfloatfloatIOV
  , ffiintfixedfixedfixedfixedIOV
  , ffiuintenumIOPtrV
  , ffiuintintptrsizeiptrbitfieldIOPtrV
  , ffiuintIOPtrV
  , ffiuintintbitfieldPtrintPtrenumIOPtrV
  , ffiuintuintdoubledoubleintintPtrdoubleIOV
  , ffiuintuintfloatfloatintintPtrfloatIOV
  , ffiuintuintdoubledoubleintintdoubledoubleintintPtrdoubleIOV
  , ffiuintuintfloatfloatintintfloatfloatintintPtrfloatIOV
  , ffienumdoubledoubledoubledoubledoubledoubleIOV
  , ffiintPtrubyteIOV
  , ffiintPtruintIOV
  , ffiintPtrushortIOV
  , ffienumdoubledoubledoubledoubleIOV
  , ffienumfloatfloatfloatfloatIOV
  , ffienumdoubledoubledoubleIOV
  , ffienumfloatfloatfloatIOV
  , ffienumenumbooleanIOV
  , ffienumPtrintPtrsizeisizeiIOV
  , ffienumPtrVsizeisizeiIOV
  , ffienumPtrVsizeisizeisizeiintIOV
  , ffienumPtrVsizeisizeiintIOV
  , ffienumintptrintptrsizeisizeiIOV
  , ffienumPtrsizeienumPtrPtrVsizeiIOV
  , ffienumPtrsizeienumPtrPtrVsizeiPtrintIOV
  , ffienumenumPtrVsizeisizeiIOV
  , ffienumenumPtrVsizeisizeisizeiintIOV
  , ffienumenumPtrVsizeisizeiintIOV
  , ffienumenumintptrintptrsizeisizeiIOV
  , ffienumuintuintPtrintPtrsizeisizeiIOV
  , ffiPtrenumPtrintPtrsizeisizeiintIOV
  , ffiPtrenumPtrsizeienumPtrPtrVsizeiintIOV
  , ffienumbyteIOV
  , ffienumPtrbyteIOV
  , ffienumdoubleIOV
  , ffienumhalfNVIOV
  , ffienumPtrhalfNVIOV
  , ffienumshortIOV
  , ffienumPtrshortIOV
  , ffienumbytebyteIOV
  , ffienumdoubledoubleIOV
  , ffienumfloatfloatIOV
  , ffienumhalfNVhalfNVIOV
  , ffienumshortshortIOV
  , ffienumfixedfixedIOV
  , ffienumbytebytebyteIOV
  , ffienumhalfNVhalfNVhalfNVIOV
  , ffienumintintintIOV
  , ffienumshortshortshortIOV
  , ffienumfixedfixedfixedIOV
  , ffienumbytebytebytebyteIOV
  , ffienumhalfNVhalfNVhalfNVhalfNVIOV
  , ffienumshortshortshortshortIOV
  , ffienumfixedfixedfixedfixedIOV
  , ffienumintenumsizeiPtrVIOV
  , ffienumenumenumfloatIOV
  , ffienumenumenumintIOV
  , ffienumenumenumdoubleIOV
  , ffienumenumintintsizeiintenumenumPtrVIOV
  , ffienumenumintintsizeisizeiintenumenumPtrVIOV
  , ffienumenumintintsizeisizeisizeiintenumenumPtrVIOV
  , ffienumenumintintsizeienumenumPtrVIOV
  , ffienumenumintintintsizeisizeienumenumPtrVIOV
  , ffienumenumintintintintsizeisizeisizeienumenumPtrVIOV
  , ffiuintsizeiptrPtrVenumIOV
  , ffiuintintptrsizeiptrbooleanIOV
  , ffiuintsizeiptrPtrVbitfieldIOV
  , ffiuintenumenumuintIOV
  , ffiuintuintsizeiPtrfloatIOV
  , ffiuintuintuintPtrfloatIOV
  , ffiuintenumuintintIOV
  , ffiuintenumenumuintintIOV
  , ffiuintenumenumuintintintIOV
  , ffiuintenumuintintenumIOV
  , ffiuintenumuintintintIOV
  , ffiuintenumuintdoubledoubledoubledoubleIOV
  , ffiuintenumuintfloatfloatfloatfloatIOV
  , ffiuintenumuintintintintintIOV
  , ffiuintenumuintuintuintuintuintIOV
  , ffiuintenumuintsizeiPtrfloatIOV
  , ffiuintenumuintsizeiPtrintIOV
  , ffiuintenumuintsizeiPtruintIOV
  , ffiuintenumenumsizeiPtrVIOV
  , ffiuintenumsizeisizeiIOV
  , ffiuintsizeienumsizeisizeiIOV
  , ffiuintsizeisizeienumsizeisizeiIOV
  , ffienumintPtrcharintPtrcharIOV
  , ffisizeiPtrVenumIOuint
  , ffiPtrVsizeiPtrcharIOV
  , ffienumuintenumIOenum
  , ffiuintuintenumIOV
  , ffiuintsizeiPtrubytesizeienumPtrVIOV
  , ffiuintsizeienumPtrVIOV
  , ffiuintenumPtrVbitfielduintsizeiuintfloatIOenum
  , ffienumPtrVbitfielduintfloatPtruintIOenum
  , ffiuintenumPtrVbitfielduintsizeienumuintfloatIOV
  , ffiuintenumPtrVbitfieldsizeienumPtrVenumuintfloatIOV
  , ffiuintenumsizeiptrPtrVsizeiuintsizeiuintfloatIOenum
  , ffienumintuintIOV
  , ffiuintenumsizeiPtrVIOV
  , ffiuintsizeisizeisizeiPtrubytesizeienumPtrVIOV
  , ffiuintsizeisizeienumPtrVIOV
  , ffienumenumintPtrfloatIOV
  , ffiuintsizeisizeifloatPtrfloatPtrfloatPtrfloatPtrfloatIOboolean
  , ffiPtrintIOint
  , ffiuintuint64EXTuintuintenumenumuintenumuintenumuintenumuintIOV
  , ffiuintuint64EXTuintuintenumenumuintuintenumuintuintIOV
  , ffifloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffisizeiPtruintPtrfloatIOV
  , ffisizeiPtruintPtrclampfIOV
  , ffisizeiPtruintPtrfixedIOV
  , ffiuintenumPtrVsizeiIOV
  , ffiuintenumPtrVintIOV
  , ffienumuintuintsizeiPtrintIOV
  , ffienumuintuintsizeiPtruintIOV
  , ffienumuintuintsizeiPtrfloatIOV
  , ffienumuintdoubledoubledoubledoubleIOV
  , ffienumuintfloatfloatfloatfloatIOV
  , ffienumuintintintintintIOV
  , ffienumuintsizeiPtrintIOV
  , ffiuintsizeiPtrubytedoubledoubledoubledoubleIOV
  , ffiuintsizeiPtrubytefloatfloatfloatfloatIOV
  , ffienumuintsizeiPtrdoubleIOV
  , ffiuintintenumintPtrfloatIOV
  , ffienumenumsizeiPtrVIOV
  , ffiuintintdoubleIOV
  , ffiuintintfloatIOV
  , ffiuintintintIOV
  , ffiuintintint64IOV
  , ffiuintintint64EXTIOV
  , ffiuintintsizeiPtrint64EXTIOV
  , ffiuintintuintIOV
  , ffiuintintuint64IOV
  , ffiuintintuint64EXTIOV
  , ffiuintintsizeiPtruint64EXTIOV
  , ffiuintintdoubledoubleIOV
  , ffiuintintfloatfloatIOV
  , ffiuintintintintIOV
  , ffiuintintint64int64IOV
  , ffiuintintint64EXTint64EXTIOV
  , ffiuintintuintuintIOV
  , ffiuintintuint64uint64IOV
  , ffiuintintuint64EXTuint64EXTIOV
  , ffiuintintdoubledoubledoubleIOV
  , ffiuintintfloatfloatfloatIOV
  , ffiuintintintintintIOV
  , ffiuintintint64int64int64IOV
  , ffiuintintint64EXTint64EXTint64EXTIOV
  , ffiuintintuintuintuintIOV
  , ffiuintintuint64uint64uint64IOV
  , ffiuintintuint64EXTuint64EXTuint64EXTIOV
  , ffiuintintdoubledoubledoubledoubleIOV
  , ffiuintintfloatfloatfloatfloatIOV
  , ffiuintintintintintintIOV
  , ffiuintintint64int64int64int64IOV
  , ffiuintintint64EXTint64EXTint64EXTint64EXTIOV
  , ffiuintintuintuintuintuintIOV
  , ffiuintintuint64uint64uint64uint64IOV
  , ffiuintintuint64EXTuint64EXTuint64EXTuint64EXTIOV
  , ffiuintintsizeibooleanPtrdoubleIOV
  , ffiuintintsizeibooleanPtrfloatIOV
  , ffiPtrfixedPtrintIObitfield
  , ffienumuintenumuintIOV
  , ffishortshortIOV
  , ffiuintbooleanIOV
  , ffiintintsizeisizeienumenumPtrVIOV
  , ffiintintsizeisizeienumenumsizeiPtrVIOV
  , ffiPtrdoublePtrdoubleIOV
  , ffiPtrintPtrintIOV
  , ffiPtrshortPtrshortIOV
  , ffiPtrfixedPtrfixedIOV
  , ffienumIOint
  , ffienumenumsizeisizeiIOV
  , ffienumsizeienumsizeisizeiIOV
  , ffienumsizeisizeienumsizeisizeiIOV
  , ffienumsizeiPtrPtrVIOV
  , ffiuintfloatfloatfloatfloatfloatfloatIOV
  , ffiPtruintPtrfloatPtrfloatIOV
  , ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffiPtruintPtrfloatPtrfloatPtrfloatIOV
  , ffiuintubyteubyteubyteubytefloatfloatfloatIOV
  , ffiPtruintPtrubytePtrfloatIOV
  , ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffiPtruintPtrfloatPtrfloatPtrfloatPtrfloatIOV
  , ffiuintfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffiuintfloatfloatfloatfloatfloatIOV
  , ffiuintfloatfloatfloatIOV
  , ffiPtruintPtrfloatIOV
  , ffifloatbooleanIOV
  , fficlampxbooleanIOV
  , fficlampfbooleanIOV
  , ffiuintbitfieldIOV
  , ffiintintsizeisizeiIOV
  , ffiuintsizeiPtrintIOV
  , ffiuintintintsizeisizeiIOV
  , ffiuintPtrintIOV
  , ffiuintbooleanuintintPtruintIOV
  , ffienumenumsizeisizeienumenumPtrVPtrVIOV
  , ffiuintenumPtrVIOV
  , ffisizeiPtruintenumPtrVsizeiIOV
  , ffienumuintuintuintIOV
  , ffienumuintuintuintuintIOV
  , ffihandleARBsizeiPtrPtrcharARBPtrintIOV
  , ffiuintuintuintuintbitfieldIOV
  , ffisizeiuintIOV
  , ffisizeienumPtrVuintenumuintenumPtrfloatIOV
  , ffiuintenumuintIOV
  , ffienumenumintuintIOV
  , ffienumenumenumIOV
  , ffisizeienumPtrVuintintuintenumPtrfloatIOV
  , ffisizeienumPtrVuintenumuintenumenumPtrfloatIOV
  , ffiuintenumuintenumIOV
  , ffisizeienumPtrVuintintuintenumenumPtrfloatIOV
  , ffiuintintuintenumIOV
  , ffiuintuintenumenumenumenumIOV
  , ffienumenumuintintptrsizeiptrIOV
  , ffibytebyteIOV
  , ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffiPtrfloatPtrfloatPtrfloatPtrfloatIOV
  , ffifloatfloatubyteubyteubyteubytefloatfloatfloatIOV
  , ffiPtrfloatPtrubytePtrfloatIOV
  , ffihalfNVhalfNVIOV
  , ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV
  , ffienumenumdoubleIOV
  , ffienumintintsizeiintenumenumPtrVIOV
  , ffienumintintsizeisizeiintenumenumPtrVIOV
  , ffienumsizeienumsizeisizeibooleanIOV
  , ffienumsizeisizeiintsizeisizeibooleanIOV
  , ffienumintintsizeisizeisizeiintenumenumPtrVIOV
  , ffienumintenumsizeisizeisizeiintenumenumPtrVIOV
  , ffienumsizeienumsizeisizeisizeibooleanIOV
  , ffienumsizeisizeiintsizeisizeisizeibooleanIOV
  , ffienumintenumsizeisizeisizeisizeiintenumenumPtrVIOV
  , ffienumintintintintsizeisizeisizeibooleanIOV
  , ffienumsizeienumsizeiIOV
  , ffienumsizeienumsizeisizeisizeiIOV
  , ffienumenumsizeisizeisizeisizeibitfieldIOV
  , ffienumintintsizeienumenumPtrVIOV
  , ffienumintintintsizeisizeienumenumPtrVIOV
  , ffienumintintintintintsizeisizeisizeisizeienumenumPtrVIOV
  , ffiuintenumuintintptrsizeiptrIOV
  , ffiuintenumenumuintintptrsizeiptrIOV
  , ffiuintenumintintsizeiintenumenumPtrVIOV
  , ffiuintenumintintsizeisizeiintenumenumPtrVIOV
  , ffiuintenumsizeisizeiintsizeisizeibooleanIOV
  , ffiuintenumsizeiintsizeisizeibooleanIOV
  , ffiuintenumintintsizeisizeisizeiintenumenumPtrVIOV
  , ffiuintenumsizeisizeiintsizeisizeisizeibooleanIOV
  , ffiuintenumsizeiintsizeisizeisizeibooleanIOV
  , ffiuintintintintintsizeisizeisizeibooleanIOV
  , ffiuintenumenumfloatIOV
  , ffiuintenumenumintIOV
  , ffiuintsizeienumsizeiIOV
  , ffiuintenumsizeienumsizeiIOV
  , ffiuintenumsizeienumsizeisizeiIOV
  , ffiuintsizeienumsizeisizeibooleanIOV
  , ffiuintenumsizeienumsizeisizeibooleanIOV
  , ffiuintsizeienumsizeisizeisizeiIOV
  , ffiuintenumsizeienumsizeisizeisizeiIOV
  , ffiuintsizeienumsizeisizeisizeibooleanIOV
  , ffiuintenumsizeienumsizeisizeisizeibooleanIOV
  , ffiuintenumenumsizeisizeisizeisizeibitfieldIOV
  , ffiuintintintsizeienumenumPtrVIOV
  , ffiuintenumintintsizeienumenumPtrVIOV
  , ffiuintintintintsizeisizeienumenumPtrVIOV
  , ffiuintenumintintintsizeisizeienumenumPtrVIOV
  , ffiuintenumintintintintsizeisizeisizeienumenumPtrVIOV
  , ffiuintenumuintenumuintuintuintuintIOV
  , ffienumuintenumenumIOV
  , ffisizeiPtrintenumIOV
  , ffiuintuintuintintptrsizeiptrIOV
  , ffisizeiPtrintsizeiPtrintenumIOV
  , ffiuintsizeiPtrPtrcharenumIOV
  , ffiuintsizeiPtrintenumIOV
  , ffiintdoubleIOV
  , ffiintsizeiPtrdoubleIOV
  , ffiintfloatIOV
  , ffiintsizeiPtrfloatIOV
  , ffiintint64IOV
  , ffiintint64EXTIOV
  , ffiintsizeiPtrint64IOV
  , ffiintsizeiPtrint64EXTIOV
  , ffiintsizeiPtrintIOV
  , ffiintuintIOV
  , ffiintuint64IOV
  , ffiintuint64EXTIOV
  , ffiintsizeiPtruint64IOV
  , ffiintsizeiPtruint64EXTIOV
  , ffiintsizeiPtruintIOV
  , ffiintint64int64IOV
  , ffiintint64EXTint64EXTIOV
  , ffiintuintuintIOV
  , ffiintuint64uint64IOV
  , ffiintuint64EXTuint64EXTIOV
  , ffiintdoubledoubledoubleIOV
  , ffiintfloatfloatfloatIOV
  , ffiintint64int64int64IOV
  , ffiintint64EXTint64EXTint64EXTIOV
  , ffiintuintuintuintIOV
  , ffiintuint64uint64uint64IOV
  , ffiintuint64EXTuint64EXTuint64EXTIOV
  , ffiintdoubledoubledoubledoubleIOV
  , ffiintfloatfloatfloatfloatIOV
  , ffiintint64int64int64int64IOV
  , ffiintint64EXTint64EXTint64EXTint64EXTIOV
  , ffiintuintuintuintuintIOV
  , ffiintuint64uint64uint64uint64IOV
  , ffiintuint64EXTuint64EXTuint64EXTuint64EXTIOV
  , ffiintsizeibooleanPtrdoubleIOV
  , ffiintsizeibooleanPtrfloatIOV
  , ffiuintuintsizeiPtrVenumIOV
  , ffiuintbitfielduintIOV
  , ffivdpauSurfaceNVenumsizeiPtrsizeiPtrintIOV
  , ffiPtrVPtrVIOV
  , ffivdpauSurfaceNVIOboolean
  , ffisizeiPtrvdpauSurfaceNVIOV
  , ffiPtrVenumsizeiPtruintIOvdpauSurfaceNV
  , ffivdpauSurfaceNVenumIOV
  , ffivdpauSurfaceNVIOV
  , ffiuintenumsizeiuintuintIOV
  , ffiuintenumuintPtrVIOV
  , ffiuintPtrbyteIOV
  , ffiuintPtrdoubleIOV
  , ffiuintPtrshortIOV
  , ffiuintPtrushortIOV
  , ffiuintuintintenumbooleanuintIOV
  , ffiuintuintintenumuintIOV
  , ffiuintuintuintintptrsizeiIOV
  , ffiuintuintintenumsizeiintptrIOV
  , ffiuintuintsizeiintptrIOV
  , ffiuintuintenumsizeiintptrIOV
  , ffiuintuintenumintenumsizeiintptrIOV
  , ffiuintuintuintintenumsizeiintptrIOV
  , ffiuintuintuintintenumbooleansizeiintptrIOV
  , ffiuintuintsizeiPtruintPtrintptrPtrsizeiIOV
  , ffiuintdoubleIOV
  , ffiuintfloatIOV
  , ffiuinthalfNVIOV
  , ffiuintPtrhalfNVIOV
  , ffiuintshortIOV
  , ffiuinthalfNVhalfNVIOV
  , ffiuintshortshortIOV
  , ffiuintdoubledoubledoubleIOV
  , ffiuinthalfNVhalfNVhalfNVIOV
  , ffiuintshortshortshortIOV
  , ffiuintubyteubyteubyteubyteIOV
  , ffiuintdoubledoubledoubledoubleIOV
  , ffiuintfloatfloatfloatfloatIOV
  , ffiuinthalfNVhalfNVhalfNVhalfNVIOV
  , ffiuintshortshortshortshortIOV
  , ffiuintintenumbooleansizeiuintuintIOV
  , ffiuintintenumbooleanuintIOV
  , ffiuintintenumbooleansizeiIOV
  , ffiuintuintuintuintuintIOV
  , ffiuintintenumuintIOV
  , ffiuintintenumsizeiIOV
  , ffiuintintenumsizeiPtrVIOV
  , ffiuintint64EXTIOV
  , ffiuintPtrint64EXTIOV
  , ffiuintuint64EXTIOV
  , ffiuintPtruint64EXTIOV
  , ffiuintint64EXTint64EXTIOV
  , ffiuintuint64EXTuint64EXTIOV
  , ffiuintint64EXTint64EXTint64EXTIOV
  , ffiuintuint64EXTuint64EXTuint64EXTIOV
  , ffiuintint64EXTint64EXTint64EXTint64EXTIOV
  , ffiuintuint64EXTuint64EXTuint64EXTuint64EXTIOV
  , ffiuintenumbooleanuintIOV
  , ffiuintenumbooleanPtruintIOV
  , ffiuintintenumbooleansizeiPtrVIOV
  , ffiuintsizeiPtrhalfNVIOV
  , ffiuintsizeiPtrshortIOV
  , ffiuintsizeiPtrubyteIOV
  , ffiuintPtruintPtruint64EXTIOenum
  , ffisyncbitfielduint64IOV
  , ffiuintsizeiPtruintPtrfloatIOV
  , ffiintPtrbyteIOV
  , ffiintPtrdoubleIOV
  , ffiintPtrfloatIOV
  , ffiintPtrintIOV
  , ffiintPtrshortIOV
  , ffienumsizeiPtrintIOV
) where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr
import Graphics.GL.Types
import Numeric.Fixed
import Numeric.Half

foreign import CALLCONV "dynamic" dynenumfloatIOV :: FunPtr (GLenum -> GLfloat -> IO ()) -> GLenum -> GLfloat -> IO ()

ffienumfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> IO ()) -> GLenum -> GLfloat -> m ()
ffienumfloatIOV fp v0 v1 = liftIO (dynenumfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumfixedIOV :: FunPtr (GLenum -> GLfixed -> IO ()) -> GLenum -> GLfixed -> IO ()

ffienumfixedIOV :: MonadIO m => FunPtr (GLenum -> GLfixed -> IO ()) -> GLenum -> GLfixed -> m ()
ffienumfixedIOV fp v0 v1 = liftIO (dynenumfixedIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintIOV :: FunPtr (GLuint -> IO ()) -> GLuint -> IO ()

ffiuintIOV :: MonadIO m => FunPtr (GLuint -> IO ()) -> GLuint -> m ()
ffiuintIOV fp v0 = liftIO (dynuintIOV fp v0)

foreign import CALLCONV "dynamic" dynuintuintIOV :: FunPtr (GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> IO ()

ffiuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> m ()
ffiuintuintIOV fp v0 v1 = liftIO (dynuintuintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumIOV :: FunPtr (GLenum -> IO ()) -> GLenum -> IO ()

ffienumIOV :: MonadIO m => FunPtr (GLenum -> IO ()) -> GLenum -> m ()
ffienumIOV fp v0 = liftIO (dynenumIOV fp v0)

foreign import CALLCONV "dynamic" dynuintPtrcharIOV :: FunPtr (GLuint -> Ptr GLchar -> IO ()) -> GLuint -> Ptr GLchar -> IO ()

ffiuintPtrcharIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLchar -> IO ()) -> GLuint -> Ptr GLchar -> m ()
ffiuintPtrcharIOV fp v0 v1 = liftIO (dynuintPtrcharIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintuintuintuintuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintuintuintuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynenumuintuintuintuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynenumclampfIOV :: FunPtr (GLenum -> GLclampf -> IO ()) -> GLenum -> GLclampf -> IO ()

ffienumclampfIOV :: MonadIO m => FunPtr (GLenum -> GLclampf -> IO ()) -> GLenum -> GLclampf -> m ()
ffienumclampfIOV fp v0 v1 = liftIO (dynenumclampfIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynIOV :: FunPtr (IO ()) -> IO ()

ffiIOV :: MonadIO m => FunPtr (IO ()) -> m ()
ffiIOV fp  = liftIO (dynIOV fp )

foreign import CALLCONV "dynamic" dynsizeiPtruintPtrbooleanIOboolean :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean) -> GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean

ffisizeiPtruintPtrbooleanIOboolean :: MonadIO m => FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean) -> GLsizei -> Ptr GLuint -> Ptr GLboolean -> m GLboolean
ffisizeiPtruintPtrbooleanIOboolean fp v0 v1 v2 = liftIO (dynsizeiPtruintPtrbooleanIOboolean fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintIOV :: FunPtr (GLint -> IO ()) -> GLint -> IO ()

ffiintIOV :: MonadIO m => FunPtr (GLint -> IO ()) -> GLint -> m ()
ffiintIOV fp v0 = liftIO (dynintIOV fp v0)

foreign import CALLCONV "dynamic" dynenumintenumsizeiuintuintIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ()

ffienumintenumsizeiuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> m ()
ffienumintenumsizeiuintuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumintenumsizeiuintuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynhandleARBhandleARBIOV :: FunPtr (GLhandleARB -> GLhandleARB -> IO ()) -> GLhandleARB -> GLhandleARB -> IO ()

ffihandleARBhandleARBIOV :: MonadIO m => FunPtr (GLhandleARB -> GLhandleARB -> IO ()) -> GLhandleARB -> GLhandleARB -> m ()
ffihandleARBhandleARBIOV fp v0 v1 = liftIO (dynhandleARBhandleARBIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintenumIOV :: FunPtr (GLuint -> GLenum -> IO ()) -> GLuint -> GLenum -> IO ()

ffiuintenumIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> IO ()) -> GLuint -> GLenum -> m ()
ffiuintenumIOV fp v0 v1 = liftIO (dynuintenumIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintIOV :: FunPtr (GLenum -> GLuint -> IO ()) -> GLenum -> GLuint -> IO ()

ffienumuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> IO ()) -> GLenum -> GLuint -> m ()
ffienumuintIOV fp v0 v1 = liftIO (dynenumuintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> IO ()

ffienumuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> m ()
ffienumuintuintIOV fp v0 v1 v2 = liftIO (dynenumuintuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintPtrcharIOV :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> Ptr GLchar -> IO ()

ffiuintuintPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> Ptr GLchar -> m ()
ffiuintuintPtrcharIOV fp v0 v1 v2 = liftIO (dynuintuintPtrcharIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynhandleARBuintPtrcharARBIOV :: FunPtr (GLhandleARB -> GLuint -> Ptr GLcharARB -> IO ()) -> GLhandleARB -> GLuint -> Ptr GLcharARB -> IO ()

ffihandleARBuintPtrcharARBIOV :: MonadIO m => FunPtr (GLhandleARB -> GLuint -> Ptr GLcharARB -> IO ()) -> GLhandleARB -> GLuint -> Ptr GLcharARB -> m ()
ffihandleARBuintPtrcharARBIOV fp v0 v1 v2 = liftIO (dynhandleARBuintPtrcharARBIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintuintintptrIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ()) -> GLenum -> GLuint -> GLuint -> GLintptr -> IO ()

ffienumuintuintintptrIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ()) -> GLenum -> GLuint -> GLuint -> GLintptr -> m ()
ffienumuintuintintptrIOV fp v0 v1 v2 v3 = liftIO (dynenumuintuintintptrIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintuintintptrsizeiptrIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ()

ffienumuintuintintptrsizeiptrIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> m ()
ffienumuintuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtruintIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ()

ffienumuintsizeiPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> m ()
ffienumuintsizeiPtruintIOV fp v0 v1 v2 v3 = liftIO (dynenumuintsizeiPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtruintPtrintptrPtrsizeiptrIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> IO ()

ffienumuintsizeiPtruintPtrintptrPtrsizeiptrIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> m ()
ffienumuintsizeiPtruintPtrintptrPtrsizeiptrIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintsizeiPtruintPtrintptrPtrsizeiptrIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintuintuintPtrcharIOV :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ()

ffiuintuintuintPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLuint -> Ptr GLchar -> m ()
ffiuintuintuintPtrcharIOV fp v0 v1 v2 v3 = liftIO (dynuintuintuintPtrcharIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintintbooleanintenumenumIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> IO ()) -> GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> IO ()

ffiuintuintintbooleanintenumenumIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> IO ()) -> GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> m ()
ffiuintuintintbooleanintenumenumIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintintbooleanintenumenumIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintuintintbooleanintenumintIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> IO ()) -> GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> IO ()

ffiuintuintintbooleanintenumintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> IO ()) -> GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> m ()
ffiuintuintintbooleanintenumintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintintbooleanintenumintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintsizeiPtruintIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> IO ()

ffiuintsizeiPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> m ()
ffiuintsizeiPtruintIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumIOuint :: FunPtr (GLenum -> GLenum -> IO GLuint) -> GLenum -> GLenum -> IO GLuint

ffienumenumIOuint :: MonadIO m => FunPtr (GLenum -> GLenum -> IO GLuint) -> GLenum -> GLenum -> m GLuint
ffienumenumIOuint fp v0 v1 = liftIO (dynenumenumIOuint fp v0 v1)

foreign import CALLCONV "dynamic" dynenumenumuintIOV :: FunPtr (GLenum -> GLenum -> GLuint -> IO ()) -> GLenum -> GLenum -> GLuint -> IO ()

ffienumenumuintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> IO ()) -> GLenum -> GLenum -> GLuint -> m ()
ffienumenumuintIOV fp v0 v1 v2 = liftIO (dynenumenumuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumIOuint :: FunPtr (GLenum -> IO GLuint) -> GLenum -> IO GLuint

ffienumIOuint :: MonadIO m => FunPtr (GLenum -> IO GLuint) -> GLenum -> m GLuint
ffienumIOuint fp v0 = liftIO (dynenumIOuint fp v0)

foreign import CALLCONV "dynamic" dynenumenumenumIOuint :: FunPtr (GLenum -> GLenum -> GLenum -> IO GLuint) -> GLenum -> GLenum -> GLenum -> IO GLuint

ffienumenumenumIOuint :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> IO GLuint) -> GLenum -> GLenum -> GLenum -> m GLuint
ffienumenumenumIOuint fp v0 v1 v2 = liftIO (dynenumenumenumIOuint fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintintptrsizeiIOV :: FunPtr (GLuint -> GLuint -> GLintptr -> GLsizei -> IO ()) -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ()

ffiuintuintintptrsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLintptr -> GLsizei -> IO ()) -> GLuint -> GLuint -> GLintptr -> GLsizei -> m ()
ffiuintuintintptrsizeiIOV fp v0 v1 v2 v3 = liftIO (dynuintuintintptrsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtruintPtrintptrPtrsizeiIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ()

ffiuintsizeiPtruintPtrintptrPtrsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> m ()
ffiuintsizeiPtruintPtrintptrPtrsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintsizeiPtruintPtrintptrPtrsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintenumintptrARBIOV :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptrARB -> IO ()) -> GLuint -> GLuint -> GLenum -> GLintptrARB -> IO ()

ffiuintuintenumintptrARBIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> GLintptrARB -> IO ()) -> GLuint -> GLuint -> GLenum -> GLintptrARB -> m ()
ffiuintuintenumintptrARBIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumintptrARBIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintenumenumuintIOV :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> IO ()) -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> IO ()

ffiuintuintenumenumuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> IO ()) -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> m ()
ffiuintuintenumenumuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintenumenumuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynbytebytebyteIOV :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLbyte -> GLbyte -> GLbyte -> IO ()

ffibytebytebyteIOV :: MonadIO m => FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLbyte -> GLbyte -> GLbyte -> m ()
ffibytebytebyteIOV fp v0 v1 v2 = liftIO (dynbytebytebyteIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrbyteIOV :: FunPtr (Ptr GLbyte -> IO ()) -> Ptr GLbyte -> IO ()

ffiPtrbyteIOV :: MonadIO m => FunPtr (Ptr GLbyte -> IO ()) -> Ptr GLbyte -> m ()
ffiPtrbyteIOV fp v0 = liftIO (dynPtrbyteIOV fp v0)

foreign import CALLCONV "dynamic" dyndoubledoubledoubleIOV :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffidoubledoubledoubleIOV :: MonadIO m => FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> GLdouble -> m ()
ffidoubledoubledoubleIOV fp v0 v1 v2 = liftIO (dyndoubledoubledoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrdoubleIOV :: FunPtr (Ptr GLdouble -> IO ()) -> Ptr GLdouble -> IO ()

ffiPtrdoubleIOV :: MonadIO m => FunPtr (Ptr GLdouble -> IO ()) -> Ptr GLdouble -> m ()
ffiPtrdoubleIOV fp v0 = liftIO (dynPtrdoubleIOV fp v0)

foreign import CALLCONV "dynamic" dynfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatIOV fp v0 v1 v2 = liftIO (dynfloatfloatfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrfloatIOV :: FunPtr (Ptr GLfloat -> IO ()) -> Ptr GLfloat -> IO ()

ffiPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLfloat -> IO ()) -> Ptr GLfloat -> m ()
ffiPtrfloatIOV fp v0 = liftIO (dynPtrfloatIOV fp v0)

foreign import CALLCONV "dynamic" dynintintintIOV :: FunPtr (GLint -> GLint -> GLint -> IO ()) -> GLint -> GLint -> GLint -> IO ()

ffiintintintIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLint -> IO ()) -> GLint -> GLint -> GLint -> m ()
ffiintintintIOV fp v0 v1 v2 = liftIO (dynintintintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrintIOV :: FunPtr (Ptr GLint -> IO ()) -> Ptr GLint -> IO ()

ffiPtrintIOV :: MonadIO m => FunPtr (Ptr GLint -> IO ()) -> Ptr GLint -> m ()
ffiPtrintIOV fp v0 = liftIO (dynPtrintIOV fp v0)

foreign import CALLCONV "dynamic" dynshortshortshortIOV :: FunPtr (GLshort -> GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> GLshort -> IO ()

ffishortshortshortIOV :: MonadIO m => FunPtr (GLshort -> GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> GLshort -> m ()
ffishortshortshortIOV fp v0 v1 v2 = liftIO (dynshortshortshortIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrshortIOV :: FunPtr (Ptr GLshort -> IO ()) -> Ptr GLshort -> IO ()

ffiPtrshortIOV :: MonadIO m => FunPtr (Ptr GLshort -> IO ()) -> Ptr GLshort -> m ()
ffiPtrshortIOV fp v0 = liftIO (dynPtrshortIOV fp v0)

foreign import CALLCONV "dynamic" dynenumsizeiPtrVIOV :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumsizeiPtrVIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeisizeifloatfloatfloatfloatPtrubyteIOV :: FunPtr (GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Ptr GLubyte -> IO ()) -> GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Ptr GLubyte -> IO ()

ffisizeisizeifloatfloatfloatfloatPtrubyteIOV :: MonadIO m => FunPtr (GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Ptr GLubyte -> IO ()) -> GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Ptr GLubyte -> m ()
ffisizeisizeifloatfloatfloatfloatPtrubyteIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynsizeisizeifloatfloatfloatfloatPtrubyteIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynsizeisizeifixedfixedfixedfixedPtrubyteIOV :: FunPtr (GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> IO ()) -> GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> IO ()

ffisizeisizeifixedfixedfixedfixedPtrubyteIOV :: MonadIO m => FunPtr (GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> IO ()) -> GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> m ()
ffisizeisizeifixedfixedfixedfixedPtrubyteIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynsizeisizeifixedfixedfixedfixedPtrubyteIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatIOV fp v0 v1 v2 v3 = liftIO (dynfloatfloatfloatfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynfixedfixedfixedfixedIOV :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffifixedfixedfixedfixedIOV :: MonadIO m => FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
ffifixedfixedfixedfixedIOV fp v0 v1 v2 v3 = liftIO (dynfixedfixedfixedfixedIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumIOV :: FunPtr (GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> IO ()

ffienumenumIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> m ()
ffienumenumIOV fp v0 v1 = liftIO (dynenumenumIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintenumenumIOV :: FunPtr (GLuint -> GLenum -> GLenum -> IO ()) -> GLuint -> GLenum -> GLenum -> IO ()

ffiuintenumenumIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> IO ()) -> GLuint -> GLenum -> GLenum -> m ()
ffiuintenumenumIOV fp v0 v1 v2 = liftIO (dynuintenumenumIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumenumenumIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()

ffienumenumenumenumIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
ffienumenumenumenumIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumenumenumIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()

ffiuintenumenumenumenumIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
ffiuintenumenumenumenumIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumenumenumenumIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintIOV :: FunPtr (GLenum -> GLint -> IO ()) -> GLenum -> GLint -> IO ()

ffienumintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> IO ()) -> GLenum -> GLint -> m ()
ffienumintIOV fp v0 v1 = liftIO (dynenumintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintintintintintintintintbitfieldenumIOV :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ()) -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ()

ffiintintintintintintintintbitfieldenumIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ()) -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> m ()
ffiintintintintintintintintbitfieldenumIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynintintintintintintintintbitfieldenumIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintuintintintintintintintintintbitfieldenumIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ()) -> GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ()

ffiuintuintintintintintintintintintbitfieldenumIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ()) -> GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> m ()
ffiuintuintintintintintintintintintbitfieldenumIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynuintuintintintintintintintintintbitfieldenumIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynenumuintuint64EXTsizeiptrIOV :: FunPtr (GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> IO ()) -> GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> IO ()

ffienumuintuint64EXTsizeiptrIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> IO ()) -> GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> m ()
ffienumuintuint64EXTsizeiptrIOV fp v0 v1 v2 v3 = liftIO (dynenumuintuint64EXTsizeiptrIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumsizeiptrPtrVenumIOV :: FunPtr (GLenum -> GLsizeiptr -> Ptr () -> GLenum -> IO ()) -> GLenum -> GLsizeiptr -> Ptr () -> GLenum -> IO ()

ffienumsizeiptrPtrVenumIOV :: MonadIO m => FunPtr (GLenum -> GLsizeiptr -> Ptr () -> GLenum -> IO ()) -> GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
ffienumsizeiptrPtrVenumIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeiptrPtrVenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumsizeiptrARBPtrVenumIOV :: FunPtr (GLenum -> GLsizeiptrARB -> Ptr () -> GLenum -> IO ()) -> GLenum -> GLsizeiptrARB -> Ptr () -> GLenum -> IO ()

ffienumsizeiptrARBPtrVenumIOV :: MonadIO m => FunPtr (GLenum -> GLsizeiptrARB -> Ptr () -> GLenum -> IO ()) -> GLenum -> GLsizeiptrARB -> Ptr () -> GLenum -> m ()
ffienumsizeiptrARBPtrVenumIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeiptrARBPtrVenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintptrsizeiptrbooleanIOV :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> IO ()) -> GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> IO ()

ffienumintptrsizeiptrbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> IO ()) -> GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> m ()
ffienumintptrsizeiptrbooleanIOV fp v0 v1 v2 v3 = liftIO (dynenumintptrsizeiptrbooleanIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumintIOV :: FunPtr (GLenum -> GLenum -> GLint -> IO ()) -> GLenum -> GLenum -> GLint -> IO ()

ffienumenumintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> IO ()) -> GLenum -> GLenum -> GLint -> m ()
ffienumenumintIOV fp v0 v1 v2 = liftIO (dynenumenumintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumsizeiptrPtrVbitfieldIOV :: FunPtr (GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ()) -> GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ()

ffienumsizeiptrPtrVbitfieldIOV :: MonadIO m => FunPtr (GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ()) -> GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> m ()
ffienumsizeiptrPtrVbitfieldIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeiptrPtrVbitfieldIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintptrsizeiptrPtrVIOV :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> IO ()) -> GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> IO ()

ffienumintptrsizeiptrPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> IO ()) -> GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
ffienumintptrsizeiptrPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumintptrsizeiptrPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintptrARBsizeiptrARBPtrVIOV :: FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> IO ()) -> GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> IO ()

ffienumintptrARBsizeiptrARBPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> IO ()) -> GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr () -> m ()
ffienumintptrARBsizeiptrARBPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumintptrARBsizeiptrARBPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynsizeienumPtrVIOV :: FunPtr (GLsizei -> GLenum -> Ptr () -> IO ()) -> GLsizei -> GLenum -> Ptr () -> IO ()

ffisizeienumPtrVIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr () -> IO ()) -> GLsizei -> GLenum -> Ptr () -> m ()
ffisizeienumPtrVIOV fp v0 v1 v2 = liftIO (dynsizeienumPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumIOenum :: FunPtr (GLenum -> IO GLenum) -> GLenum -> IO GLenum

ffienumIOenum :: MonadIO m => FunPtr (GLenum -> IO GLenum) -> GLenum -> m GLenum
ffienumIOenum fp v0 = liftIO (dynenumIOenum fp v0)

foreign import CALLCONV "dynamic" dynuintenumIOenum :: FunPtr (GLuint -> GLenum -> IO GLenum) -> GLuint -> GLenum -> IO GLenum

ffiuintenumIOenum :: MonadIO m => FunPtr (GLuint -> GLenum -> IO GLenum) -> GLuint -> GLenum -> m GLenum
ffiuintenumIOenum fp v0 v1 = liftIO (dynuintenumIOenum fp v0 v1)

foreign import CALLCONV "dynamic" dynbitfieldIOV :: FunPtr (GLbitfield -> IO ()) -> GLbitfield -> IO ()

ffibitfieldIOV :: MonadIO m => FunPtr (GLbitfield -> IO ()) -> GLbitfield -> m ()
ffibitfieldIOV fp v0 = liftIO (dynbitfieldIOV fp v0)

foreign import CALLCONV "dynamic" dynenumenumenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumenumenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumintptrsizeiptrenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintptrsizeiptrenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintptrsizeiptrenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumintptrsizeiptrenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintfloatintIOV :: FunPtr (GLenum -> GLint -> GLfloat -> GLint -> IO ()) -> GLenum -> GLint -> GLfloat -> GLint -> IO ()

ffienumintfloatintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLfloat -> GLint -> IO ()) -> GLenum -> GLint -> GLfloat -> GLint -> m ()
ffienumintfloatintIOV fp v0 v1 v2 v3 = liftIO (dynenumintfloatintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintPtrfloatIOV :: FunPtr (GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLint -> Ptr GLfloat -> IO ()

ffienumintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLint -> Ptr GLfloat -> m ()
ffienumintPtrfloatIOV fp v0 v1 v2 = liftIO (dynenumintPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumintPtrintIOV :: FunPtr (GLenum -> GLint -> Ptr GLint -> IO ()) -> GLenum -> GLint -> Ptr GLint -> IO ()

ffienumintPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr GLint -> IO ()) -> GLenum -> GLint -> Ptr GLint -> m ()
ffienumintPtrintIOV fp v0 v1 v2 = liftIO (dynenumintPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumintPtruintIOV :: FunPtr (GLenum -> GLint -> Ptr GLuint -> IO ()) -> GLenum -> GLint -> Ptr GLuint -> IO ()

ffienumintPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr GLuint -> IO ()) -> GLenum -> GLint -> Ptr GLuint -> m ()
ffienumintPtruintIOV fp v0 v1 v2 = liftIO (dynenumintPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintintintintIOV :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ()) -> GLint -> GLint -> GLint -> GLint -> IO ()

ffiintintintintIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLint -> GLint -> IO ()) -> GLint -> GLint -> GLint -> GLint -> m ()
ffiintintintintIOV fp v0 v1 v2 v3 = liftIO (dynintintintintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintuintuintIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintuintuintuintIOV fp v0 v1 v2 v3 = liftIO (dynuintuintuintuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dyndoubleIOV :: FunPtr (GLdouble -> IO ()) -> GLdouble -> IO ()

ffidoubleIOV :: MonadIO m => FunPtr (GLdouble -> IO ()) -> GLdouble -> m ()
ffidoubleIOV fp v0 = liftIO (dyndoubleIOV fp v0)

foreign import CALLCONV "dynamic" dynfloatIOV :: FunPtr (GLfloat -> IO ()) -> GLfloat -> IO ()

ffifloatIOV :: MonadIO m => FunPtr (GLfloat -> IO ()) -> GLfloat -> m ()
ffifloatIOV fp v0 = liftIO (dynfloatIOV fp v0)

foreign import CALLCONV "dynamic" dynclampfIOV :: FunPtr (GLclampf -> IO ()) -> GLclampf -> IO ()

fficlampfIOV :: MonadIO m => FunPtr (GLclampf -> IO ()) -> GLclampf -> m ()
fficlampfIOV fp v0 = liftIO (dynclampfIOV fp v0)

foreign import CALLCONV "dynamic" dynfixedIOV :: FunPtr (GLfixed -> IO ()) -> GLfixed -> IO ()

ffifixedIOV :: MonadIO m => FunPtr (GLfixed -> IO ()) -> GLfixed -> m ()
ffifixedIOV fp v0 = liftIO (dynfixedIOV fp v0)

foreign import CALLCONV "dynamic" dynuintenumenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumenumenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumenumenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumintptrsizeiptrenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintptrsizeiptrenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintptrsizeiptrenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumintptrsizeiptrenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumsizeiptrsizeiptrenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumsizeiptrsizeiptrenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumsizeiptrsizeiptrenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumsizeiptrsizeiptrenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumintfloatintIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLfloat -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLfloat -> GLint -> IO ()

ffiuintenumintfloatintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLfloat -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLfloat -> GLint -> m ()
ffiuintenumintfloatintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumintfloatintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumintPtrfloatIOV :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr GLfloat -> IO ()

ffiuintenumintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr GLfloat -> m ()
ffiuintenumintPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintenumintPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumintPtrintIOV :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr GLint -> IO ()

ffiuintenumintPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr GLint -> m ()
ffiuintenumintPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumintPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumintPtruintIOV :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr GLuint -> IO ()

ffiuintenumintPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr GLuint -> m ()
ffiuintenumintPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumintPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynsizeisizeiPtruintIOV :: FunPtr (GLsizei -> GLsizei -> Ptr GLuint -> IO ()) -> GLsizei -> GLsizei -> Ptr GLuint -> IO ()

ffisizeisizeiPtruintIOV :: MonadIO m => FunPtr (GLsizei -> GLsizei -> Ptr GLuint -> IO ()) -> GLsizei -> GLsizei -> Ptr GLuint -> m ()
ffisizeisizeiPtruintIOV fp v0 v1 v2 = liftIO (dynsizeisizeiPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintenumenumPtrVIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintintenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintintenumenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintenumenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintintintintsizeisizeisizeienumenumPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintintintintintsizeisizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynsyncbitfielduint64IOenum :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum) -> GLsync -> GLbitfield -> GLuint64 -> IO GLenum

ffisyncbitfielduint64IOenum :: MonadIO m => FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum) -> GLsync -> GLbitfield -> GLuint64 -> m GLenum
ffisyncbitfielduint64IOenum fp v0 v1 v2 = liftIO (dynsyncbitfielduint64IOenum fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtrdoubleIOV :: FunPtr (GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> Ptr GLdouble -> IO ()

ffienumPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> Ptr GLdouble -> m ()
ffienumPtrdoubleIOV fp v0 v1 = liftIO (dynenumPtrdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumPtrfloatIOV :: FunPtr (GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> Ptr GLfloat -> IO ()

ffienumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> Ptr GLfloat -> m ()
ffienumPtrfloatIOV fp v0 v1 = liftIO (dynenumPtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumPtrfixedIOV :: FunPtr (GLenum -> Ptr GLfixed -> IO ()) -> GLenum -> Ptr GLfixed -> IO ()

ffienumPtrfixedIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLfixed -> IO ()) -> GLenum -> Ptr GLfixed -> m ()
ffienumPtrfixedIOV fp v0 v1 = liftIO (dynenumPtrfixedIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynPtrfloatPtrfloatIOV :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLfloat -> IO ()

ffiPtrfloatPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLfloat -> m ()
ffiPtrfloatPtrfloatIOV fp v0 v1 = liftIO (dynPtrfloatPtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynhalfNVhalfNVhalfNVIOV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()

ffihalfNVhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
ffihalfNVhalfNVhalfNVIOV fp v0 v1 v2 = liftIO (dynhalfNVhalfNVhalfNVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrhalfNVIOV :: FunPtr (Ptr GLhalfNV -> IO ()) -> Ptr GLhalfNV -> IO ()

ffiPtrhalfNVIOV :: MonadIO m => FunPtr (Ptr GLhalfNV -> IO ()) -> Ptr GLhalfNV -> m ()
ffiPtrhalfNVIOV fp v0 = liftIO (dynPtrhalfNVIOV fp v0)

foreign import CALLCONV "dynamic" dynubyteubyteubyteIOV :: FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> IO ()

ffiubyteubyteubyteIOV :: MonadIO m => FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> m ()
ffiubyteubyteubyteIOV fp v0 v1 v2 = liftIO (dynubyteubyteubyteIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrubyteIOV :: FunPtr (Ptr GLubyte -> IO ()) -> Ptr GLubyte -> IO ()

ffiPtrubyteIOV :: MonadIO m => FunPtr (Ptr GLubyte -> IO ()) -> Ptr GLubyte -> m ()
ffiPtrubyteIOV fp v0 = liftIO (dynPtrubyteIOV fp v0)

foreign import CALLCONV "dynamic" dynuintuintuintIOV :: FunPtr (GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> m ()
ffiuintuintuintIOV fp v0 v1 v2 = liftIO (dynuintuintuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtruintIOV :: FunPtr (Ptr GLuint -> IO ()) -> Ptr GLuint -> IO ()

ffiPtruintIOV :: MonadIO m => FunPtr (Ptr GLuint -> IO ()) -> Ptr GLuint -> m ()
ffiPtruintIOV fp v0 = liftIO (dynPtruintIOV fp v0)

foreign import CALLCONV "dynamic" dynushortushortushortIOV :: FunPtr (GLushort -> GLushort -> GLushort -> IO ()) -> GLushort -> GLushort -> GLushort -> IO ()

ffiushortushortushortIOV :: MonadIO m => FunPtr (GLushort -> GLushort -> GLushort -> IO ()) -> GLushort -> GLushort -> GLushort -> m ()
ffiushortushortushortIOV fp v0 v1 v2 = liftIO (dynushortushortushortIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrushortIOV :: FunPtr (Ptr GLushort -> IO ()) -> Ptr GLushort -> IO ()

ffiPtrushortIOV :: MonadIO m => FunPtr (Ptr GLushort -> IO ()) -> Ptr GLushort -> m ()
ffiPtrushortIOV fp v0 = liftIO (dynPtrushortIOV fp v0)

foreign import CALLCONV "dynamic" dynfixedfixedfixedIOV :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffifixedfixedfixedIOV :: MonadIO m => FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> m ()
ffifixedfixedfixedIOV fp v0 v1 v2 = liftIO (dynfixedfixedfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrfixedIOV :: FunPtr (Ptr GLfixed -> IO ()) -> Ptr GLfixed -> IO ()

ffiPtrfixedIOV :: MonadIO m => FunPtr (Ptr GLfixed -> IO ()) -> Ptr GLfixed -> m ()
ffiPtrfixedIOV fp v0 = liftIO (dynPtrfixedIOV fp v0)

foreign import CALLCONV "dynamic" dynbytebytebytebyteIOV :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()

ffibytebytebytebyteIOV :: MonadIO m => FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> m ()
ffibytebytebytebyteIOV fp v0 v1 v2 v3 = liftIO (dynbytebytebytebyteIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dyndoubledoubledoubledoubleIOV :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffidoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffidoubledoubledoubledoubleIOV fp v0 v1 v2 v3 = liftIO (dyndoubledoubledoubledoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynPtrfloatPtrfloatPtrfloatIOV :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()

ffiPtrfloatPtrfloatPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
ffiPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 = liftIO (dynPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynhalfNVhalfNVhalfNVhalfNVIOV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()

ffihalfNVhalfNVhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
ffihalfNVhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 = liftIO (dynhalfNVhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynshortshortshortshortIOV :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()

ffishortshortshortshortIOV :: MonadIO m => FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
ffishortshortshortshortIOV fp v0 v1 v2 v3 = liftIO (dynshortshortshortshortIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynubyteubyteubyteubyteIOV :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()

ffiubyteubyteubyteubyteIOV :: MonadIO m => FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> m ()
ffiubyteubyteubyteubyteIOV fp v0 v1 v2 v3 = liftIO (dynubyteubyteubyteubyteIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynubyteubyteubyteubytefloatfloatIOV :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> IO ()

ffiubyteubyteubyteubytefloatfloatIOV :: MonadIO m => FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> m ()
ffiubyteubyteubyteubytefloatfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynubyteubyteubyteubytefloatfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynPtrubytePtrfloatIOV :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ()) -> Ptr GLubyte -> Ptr GLfloat -> IO ()

ffiPtrubytePtrfloatIOV :: MonadIO m => FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ()) -> Ptr GLubyte -> Ptr GLfloat -> m ()
ffiPtrubytePtrfloatIOV fp v0 v1 = liftIO (dynPtrubytePtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynubyteubyteubyteubytefloatfloatfloatIOV :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiubyteubyteubyteubytefloatfloatfloatIOV :: MonadIO m => FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiubyteubyteubyteubytefloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynubyteubyteubyteubytefloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynushortushortushortushortIOV :: FunPtr (GLushort -> GLushort -> GLushort -> GLushort -> IO ()) -> GLushort -> GLushort -> GLushort -> GLushort -> IO ()

ffiushortushortushortushortIOV :: MonadIO m => FunPtr (GLushort -> GLushort -> GLushort -> GLushort -> IO ()) -> GLushort -> GLushort -> GLushort -> GLushort -> m ()
ffiushortushortushortushortIOV fp v0 v1 v2 v3 = liftIO (dynushortushortushortushortIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintenumsizeiIOV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ()) -> GLint -> GLenum -> GLsizei -> IO ()

ffiintenumsizeiIOV :: MonadIO m => FunPtr (GLint -> GLenum -> GLsizei -> IO ()) -> GLint -> GLenum -> GLsizei -> m ()
ffiintenumsizeiIOV fp v0 v1 v2 = liftIO (dynintenumsizeiIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintuintuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumuintuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintuintuintuintuintuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintuintuintuintuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO (dynenumuintuintuintuintuintuintuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)

foreign import CALLCONV "dynamic" dynbooleanbooleanbooleanbooleanIOV :: FunPtr (GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()) -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()

ffibooleanbooleanbooleanbooleanIOV :: MonadIO m => FunPtr (GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()) -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
ffibooleanbooleanbooleanbooleanIOV fp v0 v1 v2 v3 = liftIO (dynbooleanbooleanbooleanbooleanIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintbooleanbooleanbooleanbooleanIOV :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()) -> GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()

ffiuintbooleanbooleanbooleanbooleanIOV :: MonadIO m => FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()) -> GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
ffiuintbooleanbooleanbooleanbooleanIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintbooleanbooleanbooleanbooleanIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumPtruintIOV :: FunPtr (GLenum -> Ptr GLuint -> IO ()) -> GLenum -> Ptr GLuint -> IO ()

ffienumPtruintIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLuint -> IO ()) -> GLenum -> Ptr GLuint -> m ()
ffienumPtruintIOV fp v0 v1 = liftIO (dynenumPtruintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintenumsizeiPtrVIOV :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiintenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
ffiintenumsizeiPtrVIOV fp v0 v1 v2 v3 = liftIO (dynintenumsizeiPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintenumsizeisizeiPtrVIOV :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ()) -> GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ()

ffiintenumsizeisizeiPtrVIOV :: MonadIO m => FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ()) -> GLint -> GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
ffiintenumsizeisizeiPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynintenumsizeisizeiPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintenumintPtrPtrVintIOV :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ()) -> GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ()

ffiintenumintPtrPtrVintIOV :: MonadIO m => FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ()) -> GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
ffiintenumintPtrPtrVintIOV fp v0 v1 v2 v3 v4 = liftIO (dynintenumintPtrPtrVintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintenumPtrPtrVIOV :: FunPtr (GLint -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLint -> GLenum -> Ptr (Ptr ()) -> IO ()

ffiintenumPtrPtrVIOV :: MonadIO m => FunPtr (GLint -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLint -> GLenum -> Ptr (Ptr ()) -> m ()
ffiintenumPtrPtrVIOV fp v0 v1 v2 = liftIO (dynintenumPtrPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumsizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumsizeienumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumsizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumPtrfloatIOV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffienumenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffienumenumPtrfloatIOV fp v0 v1 v2 = liftIO (dynenumenumPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumPtrintIOV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> Ptr GLint -> IO ()

ffienumenumPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> Ptr GLint -> m ()
ffienumenumPtrintIOV fp v0 v1 v2 = liftIO (dynenumenumPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumenumenumenumenumIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()

ffienumenumenumenumenumenumIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
ffienumenumenumenumenumenumIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumenumenumenumenumIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumenumenumenumenumenumbooleanbooleanbooleanIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> IO ()

ffienumenumenumenumenumenumenumbooleanbooleanbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> m ()
ffienumenumenumenumenumenumenumbooleanbooleanbooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumenumenumenumenumenumenumbooleanbooleanbooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumPtrintIOV :: FunPtr (GLenum -> Ptr GLint -> IO ()) -> GLenum -> Ptr GLint -> IO ()

ffienumPtrintIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLint -> IO ()) -> GLenum -> Ptr GLint -> m ()
ffienumPtrintIOV fp v0 v1 = liftIO (dynenumPtrintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynhandleARBIOV :: FunPtr (GLhandleARB -> IO ()) -> GLhandleARB -> IO ()

ffihandleARBIOV :: MonadIO m => FunPtr (GLhandleARB -> IO ()) -> GLhandleARB -> m ()
ffihandleARBIOV fp v0 = liftIO (dynhandleARBIOV fp v0)

foreign import CALLCONV "dynamic" dynuintsizeiPtrPtrcharPtrintIOV :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()) -> GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()

ffiuintsizeiPtrPtrcharPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()) -> GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> m ()
ffiuintsizeiPtrPtrcharPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrPtrcharPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumintenumsizeiintsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumenumintenumsizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffienumenumintenumsizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumenumintenumsizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumenumintenumsizeisizeiintsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumenumintenumsizeisizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffienumenumintenumsizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumenumintenumsizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumenumintenumsizeisizeisizeiintsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumenumintenumsizeisizeisizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffienumenumintenumsizeisizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumenumintenumsizeisizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumenumintintsizeienumsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumenumintintsizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumenumintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumenumintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumenumintintintsizeisizeienumsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumenumintintintsizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumenumintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumenumintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumenumintintintintsizeisizeisizeienumsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumenumintintintintsizeisizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumenumintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynenumenumintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynenumintenumsizeiintsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumintenumsizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffienumintenumsizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumintenumsizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintenumsizeisizeiintsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumintenumsizeisizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffienumintenumsizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumintenumsizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumintenumsizeisizeisizeiintsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumintenumsizeisizeisizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffienumintenumsizeisizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumintenumsizeisizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumintintsizeienumsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumintintsizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintintintsizeisizeienumsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumintintintsizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumintintintintsizeisizeisizeienumsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumintintintintsizeisizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynenumintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynuintenumintenumsizeiintsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffiuintenumintenumsizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffiuintenumintenumsizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumintenumsizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumintenumsizeisizeiintsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffiuintenumintenumsizeisizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffiuintenumintenumsizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumintenumsizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumintenumsizeisizeisizeiintsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()

ffiuintenumintenumsizeisizeisizeiintsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> m ()
ffiuintenumintenumsizeisizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintenumintenumsizeisizeisizeiintsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintintintsizeienumsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintintintsizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumintintsizeienumsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintenumintintsizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintenumintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumintintsizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintintintintsizeisizeienumsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintintintintsizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumintintintsizeisizeienumsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintenumintintintsizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintenumintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintenumintintintsizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintintintintintsizeisizeisizeienumsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintintintintintsizeisizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynuintenumintintintintsizeisizeisizeienumsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintenumintintintintsizeisizeisizeienumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintenumintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynuintenumintintintintsizeisizeisizeienumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynenumenumsizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumfloatIOV :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ()) -> GLenum -> GLenum -> GLfloat -> IO ()

ffienumenumfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLfloat -> IO ()) -> GLenum -> GLenum -> GLfloat -> m ()
ffienumenumfloatIOV fp v0 v1 v2 = liftIO (dynenumenumfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumfixedIOV :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ()) -> GLenum -> GLenum -> GLfixed -> IO ()

ffienumenumfixedIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLfixed -> IO ()) -> GLenum -> GLenum -> GLfixed -> m ()
ffienumenumfixedIOV fp v0 v1 v2 = liftIO (dynenumenumfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumPtrfixedIOV :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ()) -> GLenum -> GLenum -> Ptr GLfixed -> IO ()

ffienumenumPtrfixedIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ()) -> GLenum -> GLenum -> Ptr GLfixed -> m ()
ffienumenumPtrfixedIOV fp v0 v1 v2 = liftIO (dynenumenumPtrfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumintptrintptrsizeiptrIOV :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ()

ffienumenumintptrintptrsizeiptrIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> m ()
ffienumenumintptrintptrsizeiptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumintptrintptrsizeiptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumsizeiintintsizeiIOV :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ()

ffienumsizeiintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> m ()
ffienumsizeiintintsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumsizeiintintsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumintintsizeiIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ()

ffienumenumintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> m ()
ffienumenumintintsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumintintsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumintintsizeisizeiIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumenumintintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffienumenumintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumintintintintuintenumintintintintsizeisizeisizeiIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ()

ffiuintenumintintintintuintenumintintintintsizeisizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> m ()
ffiuintenumintintintintuintenumintintintintsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 = liftIO (dynuintenumintintintintuintenumintintintintsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14)

foreign import CALLCONV "dynamic" dynenumenumintenumintintsizeiintIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()

ffienumenumintenumintintsizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> m ()
ffienumenumintenumintintsizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumenumintenumintintsizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumenumintenumintintsizeisizeiintIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

ffienumenumintenumintintsizeisizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> m ()
ffienumenumintenumintintsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumenumintenumintintsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumenumintintintintsizeiIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

ffienumenumintintintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
ffienumenumintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumintintintintintsizeisizeiIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumenumintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffienumenumintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumenumintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumenumintintintintintintsizeisizeiIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumenumintintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffienumenumintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumenumintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintuintintptrintptrsizeiptrIOV :: FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ()

ffiuintuintintptrintptrsizeiptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> m ()
ffiuintuintintptrintptrsizeiptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintintptrintptrsizeiptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintintsizeisizeienumIOV :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ()

ffiintintsizeisizeienumIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> m ()
ffiintintsizeisizeienumIOV fp v0 v1 v2 v3 v4 = liftIO (dynintintsizeisizeienumIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintenumintintsizeiintIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()) -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()

ffienumintenumintintsizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()) -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> m ()
ffienumintenumintintsizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumintenumintintsizeiintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintenumintintsizeisizeiintIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

ffienumintenumintintsizeisizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> m ()
ffienumintenumintintsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumintenumintintsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumintintintintsizeiIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

ffienumintintintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
ffienumintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumintintintintintsizeisizeiIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffienumintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumintintintintintintsizeisizeiIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumintintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffienumintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumintenumintintsizeiintIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()

ffiuintenumintenumintintsizeiintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> m ()
ffiuintenumintenumintintsizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumintenumintintsizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumintenumintintsizeisizeiintIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

ffiuintenumintenumintintsizeisizeiintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> m ()
ffiuintenumintenumintintsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumintenumintintsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintuintintsizeiIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLsizei -> IO ()) -> GLuint -> GLuint -> GLint -> GLsizei -> IO ()

ffiuintuintintsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLsizei -> IO ()) -> GLuint -> GLuint -> GLint -> GLsizei -> m ()
ffiuintuintintsizeiIOV fp v0 v1 v2 v3 = liftIO (dynuintuintintsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintintintintsizeiIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

ffiuintintintintintsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
ffiuintintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumintintintintsizeiIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

ffiuintenumintintintintsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> m ()
ffiuintenumintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumintintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintintintintintintsizeisizeiIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiuintintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiuintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumintintintintintsizeisizeiIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiuintenumintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiuintenumintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintintintintintintintsizeisizeiIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiuintintintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiuintintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumintintintintintintsizeisizeiIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiuintenumintintintintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiuintenumintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintenumintintintintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynsizeienumPtrVuintenumenumPtrfloatIOV :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffisizeienumPtrVuintenumenumPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffisizeienumPtrVuintenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynsizeienumPtrVuintenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynbooleanIOV :: FunPtr (GLboolean -> IO ()) -> GLboolean -> IO ()

ffibooleanIOV :: MonadIO m => FunPtr (GLboolean -> IO ()) -> GLboolean -> m ()
ffibooleanIOV fp v0 = liftIO (dynbooleanIOV fp v0)

foreign import CALLCONV "dynamic" dynsizeiPtrfloatIOV :: FunPtr (GLsizei -> Ptr GLfloat -> IO ()) -> GLsizei -> Ptr GLfloat -> IO ()

ffisizeiPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLfloat -> IO ()) -> GLsizei -> Ptr GLfloat -> m ()
ffisizeiPtrfloatIOV fp v0 v1 = liftIO (dynsizeiPtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynsizeiPtruintIOV :: FunPtr (GLsizei -> Ptr GLuint -> IO ()) -> GLsizei -> Ptr GLuint -> IO ()

ffisizeiPtruintIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLuint -> IO ()) -> GLsizei -> Ptr GLuint -> m ()
ffisizeiPtruintIOV fp v0 v1 = liftIO (dynsizeiPtruintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtruintIOV :: FunPtr (GLuint -> Ptr GLuint -> IO ()) -> GLuint -> Ptr GLuint -> IO ()

ffiuintPtruintIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLuint -> IO ()) -> GLuint -> Ptr GLuint -> m ()
ffiuintPtruintIOV fp v0 v1 = liftIO (dynuintPtruintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynIOuint :: FunPtr (IO GLuint) -> IO GLuint

ffiIOuint :: MonadIO m => FunPtr (IO GLuint) -> m GLuint
ffiIOuint fp  = liftIO (dynIOuint fp )

foreign import CALLCONV "dynamic" dynIOhandleARB :: FunPtr (IO GLhandleARB) -> IO GLhandleARB

ffiIOhandleARB :: MonadIO m => FunPtr (IO GLhandleARB) -> m GLhandleARB
ffiIOhandleARB fp  = liftIO (dynIOhandleARB fp )

foreign import CALLCONV "dynamic" dynenumsizeiPtruintIOV :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ()) -> GLenum -> GLsizei -> Ptr GLuint -> IO ()

ffienumsizeiPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ()) -> GLenum -> GLsizei -> Ptr GLuint -> m ()
ffienumsizeiPtruintIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumIOhandleARB :: FunPtr (GLenum -> IO GLhandleARB) -> GLenum -> IO GLhandleARB

ffienumIOhandleARB :: MonadIO m => FunPtr (GLenum -> IO GLhandleARB) -> GLenum -> m GLhandleARB
ffienumIOhandleARB fp v0 = liftIO (dynenumIOhandleARB fp v0)

foreign import CALLCONV "dynamic" dynenumPtrcharIOuint :: FunPtr (GLenum -> Ptr GLchar -> IO GLuint) -> GLenum -> Ptr GLchar -> IO GLuint

ffienumPtrcharIOuint :: MonadIO m => FunPtr (GLenum -> Ptr GLchar -> IO GLuint) -> GLenum -> Ptr GLchar -> m GLuint
ffienumPtrcharIOuint fp v0 v1 = liftIO (dynenumPtrcharIOuint fp v0 v1)

foreign import CALLCONV "dynamic" dynenumsizeiPtrPtrcharIOuint :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint) -> GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint

ffienumsizeiPtrPtrcharIOuint :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint) -> GLenum -> GLsizei -> Ptr (Ptr GLchar) -> m GLuint
ffienumsizeiPtrPtrcharIOuint fp v0 v1 v2 = liftIO (dynenumsizeiPtrPtrcharIOuint fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrVPtrVbitfieldIOsync :: FunPtr (Ptr () -> Ptr () -> GLbitfield -> IO GLsync) -> Ptr () -> Ptr () -> GLbitfield -> IO GLsync

ffiPtrVPtrVbitfieldIOsync :: MonadIO m => FunPtr (Ptr () -> Ptr () -> GLbitfield -> IO GLsync) -> Ptr () -> Ptr () -> GLbitfield -> m GLsync
ffiPtrVPtrVbitfieldIOsync fp v0 v1 v2 = liftIO (dynPtrVPtrVbitfieldIOsync fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynDEBUGPROCPtrVIOV :: FunPtr (GLDEBUGPROC -> Ptr () -> IO ()) -> GLDEBUGPROC -> Ptr () -> IO ()

ffiDEBUGPROCPtrVIOV :: MonadIO m => FunPtr (GLDEBUGPROC -> Ptr () -> IO ()) -> GLDEBUGPROC -> Ptr () -> m ()
ffiDEBUGPROCPtrVIOV fp v0 v1 = liftIO (dynDEBUGPROCPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynDEBUGPROCAMDPtrVIOV :: FunPtr (GLDEBUGPROCAMD -> Ptr () -> IO ()) -> GLDEBUGPROCAMD -> Ptr () -> IO ()

ffiDEBUGPROCAMDPtrVIOV :: MonadIO m => FunPtr (GLDEBUGPROCAMD -> Ptr () -> IO ()) -> GLDEBUGPROCAMD -> Ptr () -> m ()
ffiDEBUGPROCAMDPtrVIOV fp v0 v1 = liftIO (dynDEBUGPROCAMDPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynDEBUGPROCARBPtrVIOV :: FunPtr (GLDEBUGPROCARB -> Ptr () -> IO ()) -> GLDEBUGPROCARB -> Ptr () -> IO ()

ffiDEBUGPROCARBPtrVIOV :: MonadIO m => FunPtr (GLDEBUGPROCARB -> Ptr () -> IO ()) -> GLDEBUGPROCARB -> Ptr () -> m ()
ffiDEBUGPROCARBPtrVIOV fp v0 v1 = liftIO (dynDEBUGPROCARBPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynDEBUGPROCKHRPtrVIOV :: FunPtr (GLDEBUGPROCKHR -> Ptr () -> IO ()) -> GLDEBUGPROCKHR -> Ptr () -> IO ()

ffiDEBUGPROCKHRPtrVIOV :: MonadIO m => FunPtr (GLDEBUGPROCKHR -> Ptr () -> IO ()) -> GLDEBUGPROCKHR -> Ptr () -> m ()
ffiDEBUGPROCKHRPtrVIOV fp v0 v1 = liftIO (dynDEBUGPROCKHRPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumenumenumsizeiPtruintbooleanIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ()

ffienumenumenumsizeiPtruintbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> m ()
ffienumenumenumsizeiPtruintbooleanIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumenumsizeiPtruintbooleanIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumsizeiPtruintbooleanIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ()

ffienumenumsizeiPtruintbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> m ()
ffienumenumsizeiPtruintbooleanIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumsizeiPtruintbooleanIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumuintenumsizeiPtrcharIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ()

ffienumenumuintenumsizeiPtrcharIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> m ()
ffienumenumuintenumsizeiPtrcharIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumuintenumsizeiPtrcharIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumuintsizeiPtrcharIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ()

ffienumenumuintsizeiPtrcharIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> m ()
ffienumenumuintsizeiPtrcharIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumuintsizeiPtrcharIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumdoubledoubleintintdoubledoubleintintdoubledoubleintintPtrdoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()

ffienumdoubledoubleintintdoubledoubleintintdoubledoubleintintPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
ffienumdoubledoubleintintdoubledoubleintintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO (dynenumdoubledoubleintintdoubledoubleintintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13)

foreign import CALLCONV "dynamic" dynenumfloatfloatintintfloatfloatintintfloatfloatintintPtrfloatIOV :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()

ffienumfloatfloatintintfloatfloatintintfloatfloatintintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
ffienumfloatfloatintintfloatfloatintintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO (dynenumfloatfloatintintfloatfloatintintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13)

foreign import CALLCONV "dynamic" dynuintsizeiIOV :: FunPtr (GLuint -> GLsizei -> IO ()) -> GLuint -> GLsizei -> IO ()

ffiuintsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> IO ()) -> GLuint -> GLsizei -> m ()
ffiuintsizeiIOV fp v0 v1 = liftIO (dynuintsizeiIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrcharIOV :: FunPtr (GLint -> Ptr GLchar -> IO ()) -> GLint -> Ptr GLchar -> IO ()

ffiintPtrcharIOV :: MonadIO m => FunPtr (GLint -> Ptr GLchar -> IO ()) -> GLint -> Ptr GLchar -> m ()
ffiintPtrcharIOV fp v0 v1 = liftIO (dynintPtrcharIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintPtruintIOV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ()) -> GLenum -> GLuint -> Ptr GLuint -> IO ()

ffienumuintPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ()) -> GLenum -> GLuint -> Ptr GLuint -> m ()
ffienumuintPtruintIOV fp v0 v1 v2 = liftIO (dynenumuintPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsyncIOV :: FunPtr (GLsync -> IO ()) -> GLsync -> IO ()

ffisyncIOV :: MonadIO m => FunPtr (GLsync -> IO ()) -> GLsync -> m ()
ffisyncIOV fp v0 = liftIO (dynsyncIOV fp v0)

foreign import CALLCONV "dynamic" dynclampdclampdIOV :: FunPtr (GLclampd -> GLclampd -> IO ()) -> GLclampd -> GLclampd -> IO ()

fficlampdclampdIOV :: MonadIO m => FunPtr (GLclampd -> GLclampd -> IO ()) -> GLclampd -> GLclampd -> m ()
fficlampdclampdIOV fp v0 v1 = liftIO (dynclampdclampdIOV fp v0 v1)

foreign import CALLCONV "dynamic" dyndoubledoubleIOV :: FunPtr (GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> IO ()

ffidoubledoubleIOV :: MonadIO m => FunPtr (GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> m ()
ffidoubledoubleIOV fp v0 v1 = liftIO (dyndoubledoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintsizeiPtrfloatIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffiuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffiuintsizeiPtrfloatIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtrdoubleIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLuint -> GLsizei -> Ptr GLdouble -> IO ()

ffiuintsizeiPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLuint -> GLsizei -> Ptr GLdouble -> m ()
ffiuintsizeiPtrdoubleIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintdoubledoubleIOV :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLdouble -> GLdouble -> IO ()

ffiuintdoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLdouble -> GLdouble -> m ()
ffiuintdoubledoubleIOV fp v0 v1 v2 = liftIO (dynuintdoubledoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatIOV fp v0 v1 v2 = liftIO (dynuintfloatfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> IO ()

ffifloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> m ()
ffifloatfloatIOV fp v0 v1 = liftIO (dynfloatfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynclampfclampfIOV :: FunPtr (GLclampf -> GLclampf -> IO ()) -> GLclampf -> GLclampf -> IO ()

fficlampfclampfIOV :: MonadIO m => FunPtr (GLclampf -> GLclampf -> IO ()) -> GLclampf -> GLclampf -> m ()
fficlampfclampfIOV fp v0 v1 = liftIO (dynclampfclampfIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynfixedfixedIOV :: FunPtr (GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> IO ()

ffifixedfixedIOV :: MonadIO m => FunPtr (GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> m ()
ffifixedfixedIOV fp v0 v1 = liftIO (dynfixedfixedIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumsizeiPtrfloatIOV :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLsizei -> Ptr GLfloat -> IO ()

ffienumsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLsizei -> Ptr GLfloat -> m ()
ffienumsizeiPtrfloatIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumsizeiPtrenumIOV :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> IO ()) -> GLenum -> GLsizei -> Ptr GLenum -> IO ()

ffienumsizeiPtrenumIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr GLenum -> IO ()) -> GLenum -> GLsizei -> Ptr GLenum -> m ()
ffienumsizeiPtrenumIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtrenumIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintuintuintuintuintIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynintptrIOV :: FunPtr (GLintptr -> IO ()) -> GLintptr -> IO ()

ffiintptrIOV :: MonadIO m => FunPtr (GLintptr -> IO ()) -> GLintptr -> m ()
ffiintptrIOV fp v0 = liftIO (dynintptrIOV fp v0)

foreign import CALLCONV "dynamic" dynenumintsizeiIOV :: FunPtr (GLenum -> GLint -> GLsizei -> IO ()) -> GLenum -> GLint -> GLsizei -> IO ()

ffienumintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLsizei -> IO ()) -> GLenum -> GLint -> GLsizei -> m ()
ffienumintsizeiIOV fp v0 v1 v2 = liftIO (dynenumintsizeiIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtrVIOV :: FunPtr (GLenum -> Ptr () -> IO ()) -> GLenum -> Ptr () -> IO ()

ffienumPtrVIOV :: MonadIO m => FunPtr (GLenum -> Ptr () -> IO ()) -> GLenum -> Ptr () -> m ()
ffienumPtrVIOV fp v0 v1 = liftIO (dynenumPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumintsizeisizeiIOV :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLint -> GLsizei -> GLsizei -> m ()
ffienumintsizeisizeiIOV fp v0 v1 v2 v3 = liftIO (dynenumintsizeisizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintsizeisizeiuintIOV :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ()) -> GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ()

ffienumintsizeisizeiuintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ()) -> GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> m ()
ffienumintsizeisizeiuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumintsizeisizeiuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynsizeiPtrenumIOV :: FunPtr (GLsizei -> Ptr GLenum -> IO ()) -> GLsizei -> Ptr GLenum -> IO ()

ffisizeiPtrenumIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLenum -> IO ()) -> GLsizei -> Ptr GLenum -> m ()
ffisizeiPtrenumIOV fp v0 v1 = liftIO (dynsizeiPtrenumIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrenumPtrintIOV :: FunPtr (GLint -> Ptr GLenum -> Ptr GLint -> IO ()) -> GLint -> Ptr GLenum -> Ptr GLint -> IO ()

ffiintPtrenumPtrintIOV :: MonadIO m => FunPtr (GLint -> Ptr GLenum -> Ptr GLint -> IO ()) -> GLint -> Ptr GLenum -> Ptr GLint -> m ()
ffiintPtrenumPtrintIOV fp v0 v1 v2 = liftIO (dynintPtrenumPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtruint64PtrsizeiuintIOV :: FunPtr (GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> IO ()) -> GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> IO ()

ffienumPtruint64PtrsizeiuintIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> IO ()) -> GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> m ()
ffienumPtruint64PtrsizeiuintIOV fp v0 v1 v2 v3 = liftIO (dynenumPtruint64PtrsizeiuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintPtrintptrPtrsizeiuintIOV :: FunPtr (GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> IO ()) -> GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> IO ()

ffienumuintPtrintptrPtrsizeiuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> IO ()) -> GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> m ()
ffienumuintPtrintptrPtrsizeiuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintPtrintptrPtrsizeiuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynPtruint64PtrsizeiPtruintPtruintuintIOV :: FunPtr (Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()) -> Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()

ffiPtruint64PtrsizeiPtruintPtruintuintIOV :: MonadIO m => FunPtr (Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()) -> Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> m ()
ffiPtruint64PtrsizeiPtruintPtruintuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynPtruint64PtrsizeiPtruintPtruintuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintPtrintptrPtrsizeiPtruintPtruintuintIOV :: FunPtr (GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()) -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()

ffiuintPtrintptrPtrsizeiPtruintPtruintuintIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()) -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> m ()
ffiuintPtrintptrPtrsizeiPtruintPtruintuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintPtrintptrPtrsizeiPtruintPtruintuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumsizeiIOV :: FunPtr (GLenum -> GLsizei -> IO ()) -> GLenum -> GLsizei -> IO ()

ffienumsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> IO ()) -> GLenum -> GLsizei -> m ()
ffienumsizeiIOV fp v0 v1 = liftIO (dynenumsizeiIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> IO ()

ffienumsizeienumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> m ()
ffienumsizeienumPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeienumPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVintIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ()

ffienumsizeienumPtrVintIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> m ()
ffienumsizeienumPtrVintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumsizeienumPtrVintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumPtrVIOV fp v0 v1 v2 = liftIO (dynenumenumPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVsizeiIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ()

ffienumsizeienumPtrVsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> m ()
ffienumsizeienumPtrVsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumsizeienumPtrVsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVsizeiuintIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> IO ()

ffienumsizeienumPtrVsizeiuintIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> m ()
ffienumsizeienumPtrVsizeiuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumsizeienumPtrVsizeiuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVsizeiintIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> IO ()

ffienumsizeienumPtrVsizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> m ()
ffienumsizeienumPtrVsizeiintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumsizeienumPtrVsizeiintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVsizeiintuintIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> IO ()

ffienumsizeienumPtrVsizeiintuintIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> m ()
ffienumsizeienumPtrVsizeiintuintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumsizeienumPtrVsizeiintuintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynsizeisizeienumenumPtrVIOV :: FunPtr (GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffisizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintuintintsizeiIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> IO ()

ffienumuintuintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> m ()
ffienumuintuintintsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintuintintsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintuintsizeiIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> IO ()

ffienumuintuintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> m ()
ffienumuintuintsizeiIOV fp v0 v1 v2 v3 = liftIO (dynenumuintuintsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintuintsizeienumPtrVIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> IO ()

ffienumuintuintsizeienumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> m ()
ffienumuintuintsizeienumPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintuintsizeienumPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumuintuintsizeienumPtrVintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ()

ffienumuintuintsizeienumPtrVintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> m ()
ffienumuintuintsizeienumPtrVintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumuintuintsizeienumPtrVintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintintintintintIOV :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

ffiintintintintintIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLint -> GLint -> GLint -> GLint -> GLint -> m ()
ffiintintintintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynintintintintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynshortshortshortshortshortIOV :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()

ffishortshortshortshortshortIOV :: MonadIO m => FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
ffishortshortshortshortshortIOV fp v0 v1 v2 v3 v4 = liftIO (dynshortshortshortshortshortIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintuintfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintuintfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintuintfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynfixedfixedfixedfixedfixedIOV :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffifixedfixedfixedfixedfixedIOV :: MonadIO m => FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
ffifixedfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4 = liftIO (dynfixedfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintsizeiIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLsizei -> IO ()

ffienumuintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLsizei -> m ()
ffienumuintsizeiIOV fp v0 v1 v2 = liftIO (dynenumuintsizeiIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumeglImageOESIOV :: FunPtr (GLenum -> GLeglImageOES -> IO ()) -> GLenum -> GLeglImageOES -> IO ()

ffienumeglImageOESIOV :: MonadIO m => FunPtr (GLenum -> GLeglImageOES -> IO ()) -> GLenum -> GLeglImageOES -> m ()
ffienumeglImageOESIOV fp v0 v1 = liftIO (dynenumeglImageOESIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynsizeiIOV :: FunPtr (GLsizei -> IO ()) -> GLsizei -> IO ()

ffisizeiIOV :: MonadIO m => FunPtr (GLsizei -> IO ()) -> GLsizei -> m ()
ffisizeiIOV fp v0 = liftIO (dynsizeiIOV fp v0)

foreign import CALLCONV "dynamic" dynsizeiPtrVIOV :: FunPtr (GLsizei -> Ptr () -> IO ()) -> GLsizei -> Ptr () -> IO ()

ffisizeiPtrVIOV :: MonadIO m => FunPtr (GLsizei -> Ptr () -> IO ()) -> GLsizei -> Ptr () -> m ()
ffisizeiPtrVIOV fp v0 v1 = liftIO (dynsizeiPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynsizeisizeiPtrbooleanIOV :: FunPtr (GLsizei -> GLsizei -> Ptr GLboolean -> IO ()) -> GLsizei -> GLsizei -> Ptr GLboolean -> IO ()

ffisizeisizeiPtrbooleanIOV :: MonadIO m => FunPtr (GLsizei -> GLsizei -> Ptr GLboolean -> IO ()) -> GLsizei -> GLsizei -> Ptr GLboolean -> m ()
ffisizeisizeiPtrbooleanIOV fp v0 v1 v2 = liftIO (dynsizeisizeiPtrbooleanIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintPtrPtrbooleanintIOV :: FunPtr (GLint -> Ptr (Ptr GLboolean) -> GLint -> IO ()) -> GLint -> Ptr (Ptr GLboolean) -> GLint -> IO ()

ffiintPtrPtrbooleanintIOV :: MonadIO m => FunPtr (GLint -> Ptr (Ptr GLboolean) -> GLint -> IO ()) -> GLint -> Ptr (Ptr GLboolean) -> GLint -> m ()
ffiintPtrPtrbooleanintIOV fp v0 v1 v2 = liftIO (dynintPtrPtrbooleanintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrbooleanIOV :: FunPtr (Ptr GLboolean -> IO ()) -> Ptr GLboolean -> IO ()

ffiPtrbooleanIOV :: MonadIO m => FunPtr (Ptr GLboolean -> IO ()) -> Ptr GLboolean -> m ()
ffiPtrbooleanIOV fp v0 = liftIO (dynPtrbooleanIOV fp v0)

foreign import CALLCONV "dynamic" dynenumintintIOV :: FunPtr (GLenum -> GLint -> GLint -> IO ()) -> GLenum -> GLint -> GLint -> IO ()

ffienumintintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> IO ()) -> GLenum -> GLint -> GLint -> m ()
ffienumintintIOV fp v0 v1 v2 = liftIO (dynenumintintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumintintintintIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> IO ()

ffienumintintintintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> m ()
ffienumintintintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumintintintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintintIOV :: FunPtr (GLint -> GLint -> IO ()) -> GLint -> GLint -> IO ()

ffiintintIOV :: MonadIO m => FunPtr (GLint -> GLint -> IO ()) -> GLint -> GLint -> m ()
ffiintintIOV fp v0 v1 = liftIO (dynintintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintPtrfloatIOV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> Ptr GLfloat -> IO ()

ffienumuintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> Ptr GLfloat -> m ()
ffienumuintPtrfloatIOV fp v0 v1 v2 = liftIO (dynenumuintPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtrPtrVIOV :: FunPtr (GLenum -> Ptr (Ptr ()) -> IO ()) -> GLenum -> Ptr (Ptr ()) -> IO ()

ffienumPtrPtrVIOV :: MonadIO m => FunPtr (GLenum -> Ptr (Ptr ()) -> IO ()) -> GLenum -> Ptr (Ptr ()) -> m ()
ffienumPtrPtrVIOV fp v0 v1 = liftIO (dynenumPtrPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynPtruintintPtrintIOV :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ()) -> Ptr GLuint -> GLint -> Ptr GLint -> IO ()

ffiPtruintintPtrintIOV :: MonadIO m => FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ()) -> Ptr GLuint -> GLint -> Ptr GLint -> m ()
ffiPtruintintPtrintIOV fp v0 v1 v2 = liftIO (dynPtruintintPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtrcharPtrintIOV :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> IO ()

ffiuintenumPtrcharPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> m ()
ffiuintenumPtrcharPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumPtrcharPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumintenumPtrintIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()

ffiuintenumintenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> m ()
ffiuintenumintenumPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumintenumPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintintintintsizeisizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintintintsizeisizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynenumintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynuintIOboolean :: FunPtr (GLuint -> IO GLboolean) -> GLuint -> IO GLboolean

ffiuintIOboolean :: MonadIO m => FunPtr (GLuint -> IO GLboolean) -> GLuint -> m GLboolean
ffiuintIOboolean fp v0 = liftIO (dynuintIOboolean fp v0)

foreign import CALLCONV "dynamic" dynsizeienumPtrfloatIOV :: FunPtr (GLsizei -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr GLfloat -> IO ()

ffisizeienumPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr GLfloat -> m ()
ffisizeienumPtrfloatIOV fp v0 v1 v2 = liftIO (dynsizeienumPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeienumPtrfixedIOV :: FunPtr (GLsizei -> GLenum -> Ptr GLfixed -> IO ()) -> GLsizei -> GLenum -> Ptr GLfixed -> IO ()

ffisizeienumPtrfixedIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr GLfixed -> IO ()) -> GLsizei -> GLenum -> Ptr GLfixed -> m ()
ffisizeienumPtrfixedIOV fp v0 v1 v2 = liftIO (dynsizeienumPtrfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumbitfieldIOsync :: FunPtr (GLenum -> GLbitfield -> IO GLsync) -> GLenum -> GLbitfield -> IO GLsync

ffienumbitfieldIOsync :: MonadIO m => FunPtr (GLenum -> GLbitfield -> IO GLsync) -> GLenum -> GLbitfield -> m GLsync
ffienumbitfieldIOsync fp v0 v1 = liftIO (dynenumbitfieldIOsync fp v0 v1)

foreign import CALLCONV "dynamic" dynPtruintIOint :: FunPtr (Ptr GLuint -> IO GLint) -> Ptr GLuint -> IO GLint

ffiPtruintIOint :: MonadIO m => FunPtr (Ptr GLuint -> IO GLint) -> Ptr GLuint -> m GLint
ffiPtruintIOint fp v0 = liftIO (dynPtruintIOint fp v0)

foreign import CALLCONV "dynamic" dynenumintptrsizeiptrIOV :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLintptr -> GLsizeiptr -> IO ()

ffienumintptrsizeiptrIOV :: MonadIO m => FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLintptr -> GLsizeiptr -> m ()
ffienumintptrsizeiptrIOV fp v0 v1 v2 = liftIO (dynenumintptrsizeiptrIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintptrsizeiptrIOV :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLintptr -> GLsizeiptr -> IO ()

ffiuintintptrsizeiptrIOV :: MonadIO m => FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLintptr -> GLsizeiptr -> m ()
ffiuintintptrsizeiptrIOV fp v0 v1 v2 = liftIO (dynuintintptrsizeiptrIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumintPtrPtrVintIOV :: FunPtr (GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ()) -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ()

ffienumintPtrPtrVintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ()) -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
ffienumintPtrPtrVintIOV fp v0 v1 v2 v3 = liftIO (dynenumintPtrPtrVintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynhalfNVIOV :: FunPtr (GLhalfNV -> IO ()) -> GLhalfNV -> IO ()

ffihalfNVIOV :: MonadIO m => FunPtr (GLhalfNV -> IO ()) -> GLhalfNV -> m ()
ffihalfNVIOV fp v0 = liftIO (dynhalfNVIOV fp v0)

foreign import CALLCONV "dynamic" dynuintsizeiPtrenumIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ()) -> GLuint -> GLsizei -> Ptr GLenum -> IO ()

ffiuintsizeiPtrenumIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ()) -> GLuint -> GLsizei -> Ptr GLenum -> m ()
ffiuintsizeiPtrenumIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrenumIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumenumuintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> IO ()

ffienumenumenumuintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> m ()
ffienumenumenumuintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtrfloatIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffienumuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffienumuintsizeiPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumuintsizeiPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintuintPtrfloatIOV :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLuint -> Ptr GLfloat -> IO ()

ffienumuintuintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLuint -> Ptr GLfloat -> m ()
ffienumuintuintPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumuintuintPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumuintintIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> IO ()

ffienumenumuintintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> m ()
ffienumenumuintintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumuintintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumuintintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ()

ffienumenumenumuintintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
ffienumenumenumuintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumuintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumenumuintintintintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> IO ()

ffienumenumenumuintintintintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> m ()
ffienumenumenumuintintintintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumenumuintintintintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumenumuintintsizeiIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ()

ffienumenumenumuintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> m ()
ffienumenumenumuintintsizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumenumuintintsizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumenumuintintintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()

ffienumenumenumuintintintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
ffienumenumenumuintintintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumenumuintintintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumuintintenumIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ()

ffienumenumuintintenumIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLenum -> m ()
ffienumenumuintintenumIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumuintintenumIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumuintintintIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()

ffienumenumuintintintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
ffienumenumuintintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumuintintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumuintintintintintIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

ffienumenumuintintintintintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
ffienumenumuintintintintintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumuintintintintintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumuintintsizeiintsizeiIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> IO ()

ffienumenumuintintsizeiintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> m ()
ffienumenumuintintsizeiintsizeiIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumuintintsizeiintsizeiIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumuintintintsizeiIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> IO ()

ffienumenumuintintintsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> m ()
ffienumenumuintintintsizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumuintintintsizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dyndoubledoubledoubledoubledoubledoubleIOV :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffidoubledoubledoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffidoubledoubledoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dyndoubledoubledoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynfixedfixedfixedfixedfixedfixedIOV :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffifixedfixedfixedfixedfixedfixedIOV :: MonadIO m => FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
ffifixedfixedfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynfixedfixedfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynsizeiIOuint :: FunPtr (GLsizei -> IO GLuint) -> GLsizei -> IO GLuint

ffisizeiIOuint :: MonadIO m => FunPtr (GLsizei -> IO GLuint) -> GLsizei -> m GLuint
ffisizeiIOuint fp v0 = liftIO (dynsizeiIOuint fp v0)

foreign import CALLCONV "dynamic" dynuintIOuint :: FunPtr (GLuint -> IO GLuint) -> GLuint -> IO GLuint

ffiuintIOuint :: MonadIO m => FunPtr (GLuint -> IO GLuint) -> GLuint -> m GLuint
ffiuintIOuint fp v0 = liftIO (dynuintIOuint fp v0)

foreign import CALLCONV "dynamic" dynenumenumenumuintIOuint :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO GLuint) -> GLenum -> GLenum -> GLenum -> GLuint -> IO GLuint

ffienumenumenumuintIOuint :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO GLuint) -> GLenum -> GLenum -> GLenum -> GLuint -> m GLuint
ffienumenumenumuintIOuint fp v0 v1 v2 v3 = liftIO (dynenumenumenumuintIOuint fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintenumPtrintIOV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ()

ffiuintuintenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLint -> m ()
ffiuintuintenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtrsizeiPtrintPtrenumPtrcharIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ()

ffiuintuintsizeiPtrsizeiPtrintPtrenumPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> m ()
ffiuintuintsizeiPtrsizeiPtrintPtrenumPtrcharIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintsizeiPtrsizeiPtrintPtrenumPtrcharIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynhandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ()) -> GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ()

ffihandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV :: MonadIO m => FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ()) -> GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> m ()
ffihandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynhandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumuintsizeiPtrsizeiPtrcharIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

ffiuintenumuintsizeiPtrsizeiPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
ffiuintenumuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintenumuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumuintenumPtrintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()

ffiuintenumuintenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> m ()
ffiuintenumuintenumPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintenumPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtrsizeiPtrcharIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

ffiuintuintsizeiPtrsizeiPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
ffiuintuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintsizeiPtruintenumPtrintIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ()

ffiuintsizeiPtruintenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> m ()
ffiuintsizeiPtruintenumPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintsizeiPtruintenumPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ()

ffiuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> m ()
ffiuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynhandleARBsizeiPtrsizeiPtrhandleARBIOV :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> IO ()) -> GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> IO ()

ffihandleARBsizeiPtrsizeiPtrhandleARBIOV :: MonadIO m => FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> IO ()) -> GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> m ()
ffihandleARBsizeiPtrsizeiPtrhandleARBIOV fp v0 v1 v2 v3 = liftIO (dynhandleARBsizeiPtrsizeiPtrhandleARBIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtrsizeiPtruintIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ()

ffiuintsizeiPtrsizeiPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> m ()
ffiuintsizeiPtrsizeiPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrsizeiPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintPtrcharIOint :: FunPtr (GLuint -> Ptr GLchar -> IO GLint) -> GLuint -> Ptr GLchar -> IO GLint

ffiuintPtrcharIOint :: MonadIO m => FunPtr (GLuint -> Ptr GLchar -> IO GLint) -> GLuint -> Ptr GLchar -> m GLint
ffiuintPtrcharIOint fp v0 v1 = liftIO (dynuintPtrcharIOint fp v0 v1)

foreign import CALLCONV "dynamic" dynhandleARBPtrcharARBIOint :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint) -> GLhandleARB -> Ptr GLcharARB -> IO GLint

ffihandleARBPtrcharARBIOint :: MonadIO m => FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint) -> GLhandleARB -> Ptr GLcharARB -> m GLint
ffihandleARBPtrcharARBIOint fp v0 v1 = liftIO (dynhandleARBPtrcharARBIOint fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintPtrbooleanIOV :: FunPtr (GLenum -> GLuint -> Ptr GLboolean -> IO ()) -> GLenum -> GLuint -> Ptr GLboolean -> IO ()

ffienumuintPtrbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLboolean -> IO ()) -> GLenum -> GLuint -> Ptr GLboolean -> m ()
ffienumuintPtrbooleanIOV fp v0 v1 v2 = liftIO (dynenumuintPtrbooleanIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtrbooleanIOV :: FunPtr (GLenum -> Ptr GLboolean -> IO ()) -> GLenum -> Ptr GLboolean -> IO ()

ffienumPtrbooleanIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLboolean -> IO ()) -> GLenum -> Ptr GLboolean -> m ()
ffienumPtrbooleanIOV fp v0 v1 = liftIO (dynenumPtrbooleanIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumenumPtrint64IOV :: FunPtr (GLenum -> GLenum -> Ptr GLint64 -> IO ()) -> GLenum -> GLenum -> Ptr GLint64 -> IO ()

ffienumenumPtrint64IOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLint64 -> IO ()) -> GLenum -> GLenum -> Ptr GLint64 -> m ()
ffienumenumPtrint64IOV fp v0 v1 v2 = liftIO (dynenumenumPtrint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumPtruint64EXTIOV :: FunPtr (GLenum -> GLenum -> Ptr GLuint64EXT -> IO ()) -> GLenum -> GLenum -> Ptr GLuint64EXT -> IO ()

ffienumenumPtruint64EXTIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLuint64EXT -> IO ()) -> GLenum -> GLenum -> Ptr GLuint64EXT -> m ()
ffienumenumPtruint64EXTIOV fp v0 v1 v2 = liftIO (dynenumenumPtruint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumPtrPtrVIOV :: FunPtr (GLenum -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLenum -> GLenum -> Ptr (Ptr ()) -> IO ()

ffienumenumPtrPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLenum -> GLenum -> Ptr (Ptr ()) -> m ()
ffienumenumPtrPtrVIOV fp v0 v1 v2 = liftIO (dynenumenumPtrPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumenumPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumenumPtrfloatIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffienumenumenumenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffienumenumenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumenumPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumenumenumPtrintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ()

ffienumenumenumenumPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
ffienumenumenumenumPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumenumPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumenumPtrfloatIOV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffienumenumenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffienumenumenumPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumPtrintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ()

ffienumenumenumPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
ffienumenumenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintIOuint :: FunPtr (GLenum -> GLuint -> IO GLuint) -> GLenum -> GLuint -> IO GLuint

ffienumuintIOuint :: MonadIO m => FunPtr (GLenum -> GLuint -> IO GLuint) -> GLenum -> GLuint -> m GLuint
ffienumuintIOuint fp v0 v1 = liftIO (dynenumuintIOuint fp v0 v1)

foreign import CALLCONV "dynamic" dynenumenumintPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> Ptr () -> IO ()

ffienumenumintPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> Ptr () -> m ()
ffienumenumintPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumenumintPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintPtrVIOV :: FunPtr (GLenum -> GLint -> Ptr () -> IO ()) -> GLenum -> GLint -> Ptr () -> IO ()

ffienumintPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr () -> IO ()) -> GLenum -> GLint -> Ptr () -> m ()
ffienumintPtrVIOV fp v0 v1 v2 = liftIO (dynenumintPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr () -> IO ()

ffiuintintsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr () -> m ()
ffiuintintsizeiPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumintPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr () -> IO ()

ffiuintenumintPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> Ptr () -> m ()
ffiuintenumintPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintenumintPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintintintintsizeisizeisizeisizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr () -> IO ()

ffiuintintintintintsizeisizeisizeisizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr () -> m ()
ffiuintintintintintsizeisizeisizeisizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintintintintintsizeisizeisizeisizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint) -> GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint

ffiuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint) -> GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> m GLuint
ffiuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintsizeiPtrenumPtruintPtruintPtrsizeiPtrcharIOuint :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> IO GLuint) -> GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> IO GLuint

ffiuintsizeiPtrenumPtruintPtruintPtrsizeiPtrcharIOuint :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> IO GLuint) -> GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> m GLuint
ffiuintsizeiPtrenumPtruintPtruintPtrsizeiPtrcharIOuint fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintsizeiPtrenumPtruintPtruintPtrsizeiPtrcharIOuint fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumuintPtrdoubleIOV :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ()) -> GLenum -> GLuint -> Ptr GLdouble -> IO ()

ffienumuintPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ()) -> GLenum -> GLuint -> Ptr GLdouble -> m ()
ffienumuintPtrdoubleIOV fp v0 v1 v2 = liftIO (dynenumuintPtrdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtrsizeiPtrcharIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

ffiuintsizeiPtrsizeiPtrcharIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
ffiuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynPtrintsizeiPtruintIOV :: FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ()) -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ()

ffiPtrintsizeiPtruintIOV :: MonadIO m => FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ()) -> Ptr GLint -> GLsizei -> Ptr GLuint -> m ()
ffiPtrintsizeiPtruintIOV fp v0 v1 v2 = liftIO (dynPtrintsizeiPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynIOenum :: FunPtr (IO GLenum) -> IO GLenum

ffiIOenum :: MonadIO m => FunPtr (IO GLenum) -> m GLenum
ffiIOenum fp  = liftIO (dynIOenum fp )

foreign import CALLCONV "dynamic" dynuintenumPtrintIOV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> Ptr GLint -> IO ()

ffiuintenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> Ptr GLint -> m ()
ffiuintenumPtrintIOV fp v0 v1 v2 = liftIO (dynuintenumPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumuintuintsizeiPtrfloatIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffienumenumuintuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffienumenumuintuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumuintuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintIOsizei :: FunPtr (GLuint -> IO GLsizei) -> GLuint -> IO GLsizei

ffiuintIOsizei :: MonadIO m => FunPtr (GLuint -> IO GLsizei) -> GLuint -> m GLsizei
ffiuintIOsizei fp v0 = liftIO (dynuintIOsizei fp v0)

foreign import CALLCONV "dynamic" dynenumbooleanenumenumPtrVIOV :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumbooleanenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLboolean -> GLenum -> GLenum -> Ptr () -> m ()
ffienumbooleanenumenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumbooleanenumenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintbooleanintenumIOuint64 :: FunPtr (GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64) -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64

ffiuintintbooleanintenumIOuint64 :: MonadIO m => FunPtr (GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64) -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> m GLuint64
ffiuintintbooleanintenumIOuint64 fp v0 v1 v2 v3 v4 = liftIO (dynuintintbooleanintenumIOuint64 fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynhandleARBsizeiPtrsizeiPtrcharARBIOV :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ()) -> GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ()

ffihandleARBsizeiPtrsizeiPtrcharARBIOV :: MonadIO m => FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ()) -> GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> m ()
ffihandleARBsizeiPtrsizeiPtrcharARBIOV fp v0 v1 v2 v3 = liftIO (dynhandleARBsizeiPtrsizeiPtrcharARBIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynIOint :: FunPtr (IO GLint) -> IO GLint

ffiIOint :: MonadIO m => FunPtr (IO GLint) -> m GLint
ffiIOint fp  = liftIO (dynIOint fp )

foreign import CALLCONV "dynamic" dynenumuintPtrint64IOV :: FunPtr (GLenum -> GLuint -> Ptr GLint64 -> IO ()) -> GLenum -> GLuint -> Ptr GLint64 -> IO ()

ffienumuintPtrint64IOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLint64 -> IO ()) -> GLenum -> GLuint -> Ptr GLint64 -> m ()
ffienumuintPtrint64IOV fp v0 v1 v2 = liftIO (dynenumuintPtrint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtrint64IOV :: FunPtr (GLenum -> Ptr GLint64 -> IO ()) -> GLenum -> Ptr GLint64 -> IO ()

ffienumPtrint64IOV :: MonadIO m => FunPtr (GLenum -> Ptr GLint64 -> IO ()) -> GLenum -> Ptr GLint64 -> m ()
ffienumPtrint64IOV fp v0 v1 = liftIO (dynenumPtrint64IOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintPtrintIOV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> Ptr GLint -> IO ()

ffienumuintPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> Ptr GLint -> m ()
ffienumuintPtrintIOV fp v0 v1 v2 = liftIO (dynenumuintPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintPtruint64EXTIOV :: FunPtr (GLenum -> GLuint -> Ptr GLuint64EXT -> IO ()) -> GLenum -> GLuint -> Ptr GLuint64EXT -> IO ()

ffienumuintPtruint64EXTIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr GLuint64EXT -> IO ()) -> GLenum -> GLuint -> Ptr GLuint64EXT -> m ()
ffienumuintPtruint64EXTIOV fp v0 v1 v2 = liftIO (dynenumuintPtruint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtruint64EXTIOV :: FunPtr (GLenum -> Ptr GLuint64EXT -> IO ()) -> GLenum -> Ptr GLuint64EXT -> IO ()

ffienumPtruint64EXTIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLuint64EXT -> IO ()) -> GLenum -> Ptr GLuint64EXT -> m ()
ffienumPtruint64EXTIOV fp v0 v1 = liftIO (dynenumPtruint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumenumsizeienumsizeiPtrintIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> IO ()

ffienumenumsizeienumsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> m ()
ffienumenumsizeienumsizeiPtrintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumsizeienumsizeiPtrintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumenumsizeiPtrint64IOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint64 -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint64 -> IO ()

ffienumenumenumsizeiPtrint64IOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint64 -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint64 -> m ()
ffienumenumenumsizeiPtrint64IOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumsizeiPtrint64IOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumenumsizeiPtrintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ()

ffienumenumenumsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> m ()
ffienumenumenumsizeiPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumsizeiPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumPtrbooleanIOV :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ()) -> GLuint -> GLenum -> Ptr GLboolean -> IO ()

ffiuintenumPtrbooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ()) -> GLuint -> GLenum -> Ptr GLboolean -> m ()
ffiuintenumPtrbooleanIOV fp v0 v1 v2 = liftIO (dynuintenumPtrbooleanIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtrfloatIOV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> Ptr GLfloat -> IO ()

ffiuintenumPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> Ptr GLfloat -> m ()
ffiuintenumPtrfloatIOV fp v0 v1 v2 = liftIO (dynuintenumPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintenumPtrfloatIOV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ()

ffienumuintenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> m ()
ffienumuintenumPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumuintenumPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintenumPtrintIOV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()

ffienumuintenumPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLint -> m ()
ffienumuintenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynenumuintenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintenumsizeisizeibooleanPtrVIOV :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr () -> IO ()) -> GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr () -> IO ()

ffienumuintenumsizeisizeibooleanPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr () -> IO ()) -> GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr () -> m ()
ffienumuintenumsizeisizeibooleanPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumuintenumsizeisizeibooleanPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumPtrdoubleIOV :: FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> GLenum -> Ptr GLdouble -> IO ()

ffienumenumPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> GLenum -> Ptr GLdouble -> m ()
ffienumenumPtrdoubleIOV fp v0 v1 v2 = liftIO (dynenumenumPtrdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumenumPtrdoubleIOV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ()

ffienumenumenumPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLdouble -> m ()
ffienumenumenumPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumintenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumintenumPtrfloatIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()

ffienumenumintenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> m ()
ffienumenumintenumPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumintenumPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumintenumPtrintIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()

ffienumenumintenumPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> m ()
ffienumenumintenumPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumintenumPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumenumPtruintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ()

ffienumenumenumPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr GLuint -> m ()
ffienumenumenumPtruintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtrint64IOV :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ()) -> GLuint -> GLenum -> Ptr GLint64 -> IO ()

ffiuintenumPtrint64IOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ()) -> GLuint -> GLenum -> Ptr GLint64 -> m ()
ffiuintenumPtrint64IOV fp v0 v1 v2 = liftIO (dynuintenumPtrint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtruint64EXTIOV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ()) -> GLuint -> GLenum -> Ptr GLuint64EXT -> IO ()

ffiuintenumPtruint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ()) -> GLuint -> GLenum -> Ptr GLuint64EXT -> m ()
ffiuintenumPtruint64EXTIOV fp v0 v1 v2 = liftIO (dynuintenumPtruint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtrPtrVIOV :: FunPtr (GLuint -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLuint -> GLenum -> Ptr (Ptr ()) -> IO ()

ffiuintenumPtrPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
ffiuintenumPtrPtrVIOV fp v0 v1 v2 = liftIO (dynuintenumPtrPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintptrsizeiptrPtrVIOV :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> IO ()) -> GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> IO ()

ffiuintintptrsizeiptrPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> IO ()) -> GLuint -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
ffiuintintptrsizeiptrPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintintptrsizeiptrPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumPtrintIOV :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ()

ffiuintenumenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr GLint -> m ()
ffiuintenumenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumuintPtrintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ()

ffiuintenumuintPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLint -> m ()
ffiuintenumuintPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumuintPtruintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ()

ffiuintenumuintPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLuint -> m ()
ffiuintenumuintPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumuintPtrdoubleIOV :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ()

ffiuintenumuintPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLdouble -> m ()
ffiuintenumuintPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumuintPtrfloatIOV :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ()

ffiuintenumuintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLfloat -> m ()
ffiuintenumuintPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumenumPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintPtrcharsizeiPtrintPtrcharIOV :: FunPtr (GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> IO ()) -> GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> IO ()

ffiintPtrcharsizeiPtrintPtrcharIOV :: MonadIO m => FunPtr (GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> IO ()) -> GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> m ()
ffiintPtrcharsizeiPtrintPtrcharIOV fp v0 v1 v2 v3 v4 = liftIO (dynintPtrcharsizeiPtrintPtrcharIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintPtrcharenumPtrintIOV :: FunPtr (GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> IO ()) -> GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> IO ()

ffiintPtrcharenumPtrintIOV :: MonadIO m => FunPtr (GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> IO ()) -> GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> m ()
ffiintPtrcharenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynintPtrcharenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtrsizeiPtrcharIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

ffienumuintsizeiPtrsizeiPtrcharIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
ffienumuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynhandleARBenumPtrfloatIOV :: FunPtr (GLhandleARB -> GLenum -> Ptr GLfloat -> IO ()) -> GLhandleARB -> GLenum -> Ptr GLfloat -> IO ()

ffihandleARBenumPtrfloatIOV :: MonadIO m => FunPtr (GLhandleARB -> GLenum -> Ptr GLfloat -> IO ()) -> GLhandleARB -> GLenum -> Ptr GLfloat -> m ()
ffihandleARBenumPtrfloatIOV fp v0 v1 v2 = liftIO (dynhandleARBenumPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynhandleARBenumPtrintIOV :: FunPtr (GLhandleARB -> GLenum -> Ptr GLint -> IO ()) -> GLhandleARB -> GLenum -> Ptr GLint -> IO ()

ffihandleARBenumPtrintIOV :: MonadIO m => FunPtr (GLhandleARB -> GLenum -> Ptr GLint -> IO ()) -> GLhandleARB -> GLenum -> Ptr GLint -> m ()
ffihandleARBenumPtrintIOV fp v0 v1 v2 = liftIO (dynhandleARBenumPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrVsizeiPtrsizeiPtrcharIOV :: FunPtr (Ptr () -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> Ptr () -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

ffiPtrVsizeiPtrsizeiPtrcharIOV :: MonadIO m => FunPtr (Ptr () -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> Ptr () -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
ffiPtrVsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3 = liftIO (dynPtrVsizeiPtrsizeiPtrcharIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtruintIOV :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> Ptr GLuint -> IO ()

ffiuintenumPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> Ptr GLuint -> m ()
ffiuintenumPtruintIOV fp v0 v1 v2 = liftIO (dynuintenumPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintPtrubyteIOV :: FunPtr (GLuint -> Ptr GLubyte -> IO ()) -> GLuint -> Ptr GLubyte -> IO ()

ffiuintPtrubyteIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLubyte -> IO ()) -> GLuint -> Ptr GLubyte -> m ()
ffiuintPtrubyteIOV fp v0 v1 = liftIO (dynuintPtrubyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtrfloatIOV :: FunPtr (GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> Ptr GLfloat -> IO ()

ffiuintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> Ptr GLfloat -> m ()
ffiuintPtrfloatIOV fp v0 v1 = liftIO (dynuintPtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintsizeisizeiIOfloat :: FunPtr (GLuint -> GLsizei -> GLsizei -> IO GLfloat) -> GLuint -> GLsizei -> GLsizei -> IO GLfloat

ffiuintsizeisizeiIOfloat :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLsizei -> IO GLfloat) -> GLuint -> GLsizei -> GLsizei -> m GLfloat
ffiuintsizeisizeiIOfloat fp v0 v1 v2 = liftIO (dynuintsizeisizeiIOfloat fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynbitfielduintsizeisizeiPtrfloatIOV :: FunPtr (GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> IO ()) -> GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> IO ()

ffibitfielduintsizeisizeiPtrfloatIOV :: MonadIO m => FunPtr (GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> IO ()) -> GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> m ()
ffibitfielduintsizeisizeiPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynbitfielduintsizeisizeiPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynbitfieldsizeienumPtrVuintsizeiPtrfloatIOV :: FunPtr (GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffibitfieldsizeienumPtrVuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffibitfieldsizeienumPtrVuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynbitfieldsizeienumPtrVuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumsizeienumPtrVuintfloatfloatenumPtrfloatIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> IO ()

ffienumsizeienumPtrVuintfloatfloatenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> m ()
ffienumsizeienumPtrVuintfloatfloatenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumsizeienumPtrVuintfloatfloatenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintuintuintPtrcharuintPtrcharPtruintPtruintPtruintPtruintPtruint64IOV :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> IO ()) -> GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> IO ()

ffiuintuintuintPtrcharuintPtrcharPtruintPtruintPtruintPtruintPtruint64IOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> IO ()) -> GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> m ()
ffiuintuintuintPtrcharuintPtrcharPtruintPtruintPtruintPtruintPtruint64IOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintuintuintPtrcharuintPtrcharPtruintPtruintPtruintPtruintPtruint64IOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynuintenumsizeiPtruintPtrintIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> IO ()

ffiuintenumsizeiPtruintPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> m ()
ffiuintenumsizeiPtruintPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumsizeiPtruintPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintenumPtrVIOV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr () -> IO ()

ffiuintuintenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr () -> m ()
ffiuintuintenumPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintPtrintPtrintsizeiPtruintIOV :: FunPtr (GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ()

ffiuintPtrintPtrintsizeiPtruintIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> m ()
ffiuintPtrintPtrintsizeiPtruintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintPtrintPtrintsizeiPtruintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtrVPtruintIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr () -> Ptr GLuint -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr () -> Ptr GLuint -> IO ()

ffiuintuintsizeiPtrVPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr () -> Ptr GLuint -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr () -> Ptr GLuint -> m ()
ffiuintuintsizeiPtrVPtruintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintsizeiPtrVPtruintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynPtrcharPtruintIOV :: FunPtr (Ptr GLchar -> Ptr GLuint -> IO ()) -> Ptr GLchar -> Ptr GLuint -> IO ()

ffiPtrcharPtruintIOV :: MonadIO m => FunPtr (Ptr GLchar -> Ptr GLuint -> IO ()) -> Ptr GLchar -> Ptr GLuint -> m ()
ffiPtrcharPtruintIOV fp v0 v1 = liftIO (dynPtrcharPtruintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintuintPtrcharPtruintPtruintPtruintPtruintIOV :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> IO ()) -> GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> IO ()

ffiuintuintPtrcharPtruintPtruintPtruintPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> IO ()) -> GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> m ()
ffiuintuintPtrcharPtruintPtruintPtruintPtruintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintPtrcharPtruintPtruintPtruintPtruintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumPtrushortIOV :: FunPtr (GLenum -> Ptr GLushort -> IO ()) -> GLenum -> Ptr GLushort -> IO ()

ffienumPtrushortIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLushort -> IO ()) -> GLenum -> Ptr GLushort -> m ()
ffienumPtrushortIOV fp v0 v1 = liftIO (dynenumPtrushortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumintPtrfixedIOV :: FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ()) -> GLenum -> GLint -> Ptr GLfixed -> IO ()

ffienumintPtrfixedIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ()) -> GLenum -> GLint -> Ptr GLfixed -> m ()
ffienumintPtrfixedIOV fp v0 v1 v2 = liftIO (dynenumintPtrfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintPtrPtrVIOV :: FunPtr (GLenum -> GLuint -> Ptr (Ptr ()) -> IO ()) -> GLenum -> GLuint -> Ptr (Ptr ()) -> IO ()

ffienumuintPtrPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> Ptr (Ptr ()) -> IO ()) -> GLenum -> GLuint -> Ptr (Ptr ()) -> m ()
ffienumuintPtrPtrVIOV fp v0 v1 v2 = liftIO (dynenumuintPtrPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtrsizeiPtrenumPtrVIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> IO ()

ffiuintsizeiPtrsizeiPtrenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> m ()
ffiuintsizeiPtrsizeiPtrenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintsizeiPtrsizeiPtrenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintsizeiPtrubytePtrdoubleIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ()

ffiuintsizeiPtrubytePtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> m ()
ffiuintsizeiPtrubytePtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrubytePtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtrubytePtrfloatIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ()

ffiuintsizeiPtrubytePtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> m ()
ffiuintsizeiPtrubytePtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrubytePtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintenumPtrdoubleIOV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLdouble -> IO ()

ffienumuintenumPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLdouble -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLdouble -> m ()
ffienumuintenumPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynenumuintenumPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtrcharIOuint :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLuint) -> GLuint -> GLenum -> Ptr GLchar -> IO GLuint

ffiuintenumPtrcharIOuint :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLuint) -> GLuint -> GLenum -> Ptr GLchar -> m GLuint
ffiuintenumPtrcharIOuint fp v0 v1 v2 = liftIO (dynuintenumPtrcharIOuint fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtrcharIOint :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint) -> GLuint -> GLenum -> Ptr GLchar -> IO GLint

ffiuintenumPtrcharIOint :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint) -> GLuint -> GLenum -> Ptr GLchar -> m GLint
ffiuintenumPtrcharIOint fp v0 v1 v2 = liftIO (dynuintenumPtrcharIOint fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumuintsizeiPtrenumsizeiPtrsizeiPtrfloatIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> IO ()

ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> m ()
ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumuintsizeiPtrenumsizeiPtrsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumuintsizeiPtrenumsizeiPtrsizeiPtrintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()

ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> m ()
ffiuintenumuintsizeiPtrenumsizeiPtrsizeiPtrintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumuintsizeiPtrenumsizeiPtrsizeiPtrintIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumPtrubyteIOV :: FunPtr (GLuint -> GLenum -> Ptr GLubyte -> IO ()) -> GLuint -> GLenum -> Ptr GLubyte -> IO ()

ffiuintenumPtrubyteIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLubyte -> IO ()) -> GLuint -> GLenum -> Ptr GLubyte -> m ()
ffiuintenumPtrubyteIOV fp v0 v1 v2 = liftIO (dynuintenumPtrubyteIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintenumintptrIOV :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLenum -> GLintptr -> IO ()

ffiuintuintenumintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLenum -> GLintptr -> m ()
ffiuintuintenumintptrIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumintptrIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtruint64IOV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ()) -> GLuint -> GLenum -> Ptr GLuint64 -> IO ()

ffiuintenumPtruint64IOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ()) -> GLuint -> GLenum -> Ptr GLuint64 -> m ()
ffiuintenumPtruint64IOV fp v0 v1 v2 = liftIO (dynuintenumPtruint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumenumPtrVPtrVPtrVIOV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> IO ()

ffienumenumenumPtrVPtrVPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> Ptr () -> Ptr () -> Ptr () -> m ()
ffienumenumenumPtrVPtrVPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumenumPtrVPtrVPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumPtrintPtrintIOV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ()

ffienumenumPtrintPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> m ()
ffienumenumPtrintPtrintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumPtrintPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumIOushort :: FunPtr (GLenum -> IO GLushort) -> GLenum -> IO GLushort

ffienumIOushort :: MonadIO m => FunPtr (GLenum -> IO GLushort) -> GLenum -> m GLushort
ffienumIOushort fp v0 = liftIO (dynenumIOushort fp v0)

foreign import CALLCONV "dynamic" dynenumIOPtrubyte :: FunPtr (GLenum -> IO (Ptr GLubyte)) -> GLenum -> IO (Ptr GLubyte)

ffienumIOPtrubyte :: MonadIO m => FunPtr (GLenum -> IO (Ptr GLubyte)) -> GLenum -> m (Ptr GLubyte)
ffienumIOPtrubyte fp v0 = liftIO (dynenumIOPtrubyte fp v0)

foreign import CALLCONV "dynamic" dynenumuintIOPtrubyte :: FunPtr (GLenum -> GLuint -> IO (Ptr GLubyte)) -> GLenum -> GLuint -> IO (Ptr GLubyte)

ffienumuintIOPtrubyte :: MonadIO m => FunPtr (GLenum -> GLuint -> IO (Ptr GLubyte)) -> GLenum -> GLuint -> m (Ptr GLubyte)
ffienumuintIOPtrubyte fp v0 v1 = liftIO (dynenumuintIOPtrubyte fp v0 v1)

foreign import CALLCONV "dynamic" dynsyncenumsizeiPtrsizeiPtrintIOV :: FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()) -> GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()

ffisyncenumsizeiPtrsizeiPtrintIOV :: MonadIO m => FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()) -> GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> m ()
ffisyncenumsizeiPtrsizeiPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynsyncenumsizeiPtrsizeiPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintenumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintenumenumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumintenumenumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintenumPtrfloatIOV :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()

ffienumintenumPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> m ()
ffienumintenumPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumintenumPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintenumPtrintIOV :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()

ffienumintenumPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLint -> GLenum -> Ptr GLint -> m ()
ffienumintenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynenumintenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintenumPtrfixedIOV :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfixed -> IO ()) -> GLenum -> GLint -> GLenum -> Ptr GLfixed -> IO ()

ffienumintenumPtrfixedIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfixed -> IO ()) -> GLenum -> GLint -> GLenum -> Ptr GLfixed -> m ()
ffienumintenumPtrfixedIOV fp v0 v1 v2 v3 = liftIO (dynenumintenumPtrfixedIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumPtruintIOV :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ()) -> GLenum -> GLenum -> Ptr GLuint -> IO ()

ffienumenumPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ()) -> GLenum -> GLenum -> Ptr GLuint -> m ()
ffienumenumPtruintIOV fp v0 v1 v2 = liftIO (dynenumenumPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintIOuint64 :: FunPtr (GLuint -> IO GLuint64) -> GLuint -> IO GLuint64

ffiuintIOuint64 :: MonadIO m => FunPtr (GLuint -> IO GLuint64) -> GLuint -> m GLuint64
ffiuintIOuint64 fp v0 = liftIO (dynuintIOuint64 fp v0)

foreign import CALLCONV "dynamic" dynuintintenumenumsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintintenumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintintenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumintenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintenumintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintenumPtrfloatIOV :: FunPtr (GLuint -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLenum -> Ptr GLfloat -> IO ()

ffiuintintenumPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLenum -> Ptr GLfloat -> m ()
ffiuintintenumPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintintenumPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumintenumPtrfloatIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()

ffiuintenumintenumPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> m ()
ffiuintenumintenumPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumintenumPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintenumPtrintIOV :: FunPtr (GLuint -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLint -> GLenum -> Ptr GLint -> IO ()

ffiuintintenumPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> Ptr GLint -> IO ()) -> GLuint -> GLint -> GLenum -> Ptr GLint -> m ()
ffiuintintenumPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintintenumPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumPtruintIOV :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ()

ffiuintenumenumPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr GLuint -> m ()
ffiuintenumenumPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumPtrfloatIOV :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffiuintenumenumPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffiuintenumenumPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintIOuint64 :: FunPtr (GLuint -> GLuint -> IO GLuint64) -> GLuint -> GLuint -> IO GLuint64

ffiuintuintIOuint64 :: MonadIO m => FunPtr (GLuint -> GLuint -> IO GLuint64) -> GLuint -> GLuint -> m GLuint64
ffiuintuintIOuint64 fp v0 v1 = liftIO (dynuintuintIOuint64 fp v0 v1)

foreign import CALLCONV "dynamic" dynuintintintintintsizeisizeisizeienumenumsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintintintintintsizeisizeisizeienumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintintintintintsizeisizeisizeienumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynuintintintintintsizeisizeisizeienumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynuintuintPtrintIOV :: FunPtr (GLuint -> GLuint -> Ptr GLint -> IO ()) -> GLuint -> GLuint -> Ptr GLint -> IO ()

ffiuintuintPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> Ptr GLint -> IO ()) -> GLuint -> GLuint -> Ptr GLint -> m ()
ffiuintuintPtrintIOV fp v0 v1 v2 = liftIO (dynuintuintPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumuintPtrint64IOV :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint64 -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLint64 -> IO ()

ffiuintenumuintPtrint64IOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint64 -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr GLint64 -> m ()
ffiuintenumuintPtrint64IOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintPtrint64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintPtrcharIOuint :: FunPtr (GLuint -> Ptr GLchar -> IO GLuint) -> GLuint -> Ptr GLchar -> IO GLuint

ffiuintPtrcharIOuint :: MonadIO m => FunPtr (GLuint -> Ptr GLchar -> IO GLuint) -> GLuint -> Ptr GLchar -> m GLuint
ffiuintPtrcharIOuint fp v0 v1 = liftIO (dynuintPtrcharIOuint fp v0 v1)

foreign import CALLCONV "dynamic" dynuintintIOint :: FunPtr (GLuint -> GLint -> IO GLint) -> GLuint -> GLint -> IO GLint

ffiuintintIOint :: MonadIO m => FunPtr (GLuint -> GLint -> IO GLint) -> GLuint -> GLint -> m GLint
ffiuintintIOint fp v0 v1 = liftIO (dynuintintIOint fp v0 v1)

foreign import CALLCONV "dynamic" dynuintsizeiPtrPtrcharPtruintIOV :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLuint -> IO ()) -> GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLuint -> IO ()

ffiuintsizeiPtrPtrcharPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLuint -> IO ()) -> GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLuint -> m ()
ffiuintsizeiPtrPtrcharPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrPtrcharPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintIOintptr :: FunPtr (GLuint -> GLint -> IO GLintptr) -> GLuint -> GLint -> IO GLintptr

ffiuintintIOintptr :: MonadIO m => FunPtr (GLuint -> GLint -> IO GLintptr) -> GLuint -> GLint -> m GLintptr
ffiuintintIOintptr fp v0 v1 = liftIO (dynuintintIOintptr fp v0 v1)

foreign import CALLCONV "dynamic" dynuintintPtrdoubleIOV :: FunPtr (GLuint -> GLint -> Ptr GLdouble -> IO ()) -> GLuint -> GLint -> Ptr GLdouble -> IO ()

ffiuintintPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLdouble -> IO ()) -> GLuint -> GLint -> Ptr GLdouble -> m ()
ffiuintintPtrdoubleIOV fp v0 v1 v2 = liftIO (dynuintintPtrdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtrfloatIOV :: FunPtr (GLuint -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> Ptr GLfloat -> IO ()

ffiuintintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> Ptr GLfloat -> m ()
ffiuintintPtrfloatIOV fp v0 v1 v2 = liftIO (dynuintintPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynhandleARBintPtrfloatIOV :: FunPtr (GLhandleARB -> GLint -> Ptr GLfloat -> IO ()) -> GLhandleARB -> GLint -> Ptr GLfloat -> IO ()

ffihandleARBintPtrfloatIOV :: MonadIO m => FunPtr (GLhandleARB -> GLint -> Ptr GLfloat -> IO ()) -> GLhandleARB -> GLint -> Ptr GLfloat -> m ()
ffihandleARBintPtrfloatIOV fp v0 v1 v2 = liftIO (dynhandleARBintPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtrint64IOV :: FunPtr (GLuint -> GLint -> Ptr GLint64 -> IO ()) -> GLuint -> GLint -> Ptr GLint64 -> IO ()

ffiuintintPtrint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLint64 -> IO ()) -> GLuint -> GLint -> Ptr GLint64 -> m ()
ffiuintintPtrint64IOV fp v0 v1 v2 = liftIO (dynuintintPtrint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtrint64EXTIOV :: FunPtr (GLuint -> GLint -> Ptr GLint64EXT -> IO ()) -> GLuint -> GLint -> Ptr GLint64EXT -> IO ()

ffiuintintPtrint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLint64EXT -> IO ()) -> GLuint -> GLint -> Ptr GLint64EXT -> m ()
ffiuintintPtrint64EXTIOV fp v0 v1 v2 = liftIO (dynuintintPtrint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtrintIOV :: FunPtr (GLuint -> GLint -> Ptr GLint -> IO ()) -> GLuint -> GLint -> Ptr GLint -> IO ()

ffiuintintPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLint -> IO ()) -> GLuint -> GLint -> Ptr GLint -> m ()
ffiuintintPtrintIOV fp v0 v1 v2 = liftIO (dynuintintPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynhandleARBintPtrintIOV :: FunPtr (GLhandleARB -> GLint -> Ptr GLint -> IO ()) -> GLhandleARB -> GLint -> Ptr GLint -> IO ()

ffihandleARBintPtrintIOV :: MonadIO m => FunPtr (GLhandleARB -> GLint -> Ptr GLint -> IO ()) -> GLhandleARB -> GLint -> Ptr GLint -> m ()
ffihandleARBintPtrintIOV fp v0 v1 v2 = liftIO (dynhandleARBintPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtruint64IOV :: FunPtr (GLuint -> GLint -> Ptr GLuint64 -> IO ()) -> GLuint -> GLint -> Ptr GLuint64 -> IO ()

ffiuintintPtruint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLuint64 -> IO ()) -> GLuint -> GLint -> Ptr GLuint64 -> m ()
ffiuintintPtruint64IOV fp v0 v1 v2 = liftIO (dynuintintPtruint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtruint64EXTIOV :: FunPtr (GLuint -> GLint -> Ptr GLuint64EXT -> IO ()) -> GLuint -> GLint -> Ptr GLuint64EXT -> IO ()

ffiuintintPtruint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLuint64EXT -> IO ()) -> GLuint -> GLint -> Ptr GLuint64EXT -> m ()
ffiuintintPtruint64EXTIOV fp v0 v1 v2 = liftIO (dynuintintPtruint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintPtruintIOV :: FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ()) -> GLuint -> GLint -> Ptr GLuint -> IO ()

ffiuintintPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ()) -> GLuint -> GLint -> Ptr GLuint -> m ()
ffiuintintPtruintIOV fp v0 v1 v2 = liftIO (dynuintintPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintenumPtrint64IOV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint64 -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLint64 -> IO ()

ffiuintuintenumPtrint64IOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint64 -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLint64 -> m ()
ffiuintuintenumPtrint64IOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumPtrint64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintenumPtrPtrVIOV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr (Ptr ()) -> IO ()

ffiuintuintenumPtrPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> Ptr (Ptr ()) -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
ffiuintuintenumPtrPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumPtrPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtrdoubleIOV :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ()) -> GLuint -> GLenum -> Ptr GLdouble -> IO ()

ffiuintenumPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ()) -> GLuint -> GLenum -> Ptr GLdouble -> m ()
ffiuintenumPtrdoubleIOV fp v0 v1 v2 = liftIO (dynuintenumPtrdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtrint64EXTIOV :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ()) -> GLuint -> GLenum -> Ptr GLint64EXT -> IO ()

ffiuintenumPtrint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ()) -> GLuint -> GLenum -> Ptr GLint64EXT -> m ()
ffiuintenumPtrint64EXTIOV fp v0 v1 v2 = liftIO (dynuintenumPtrint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintenumPtrdoubleIOV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ()

ffiuintuintenumPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLdouble -> m ()
ffiuintuintenumPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintenumPtrfloatIOV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ()

ffiuintuintenumPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLenum -> Ptr GLfloat -> m ()
ffiuintuintenumPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintuintenumPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumenumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLsizei -> Ptr () -> IO ()

ffienumintsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLsizei -> Ptr () -> m ()
ffienumintsizeiPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumintsizeiPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumbooleanenumenumsizeiPtrVIOV :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumbooleanenumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumbooleanenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumbooleanenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumsizeiPtrdoubleIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ()

ffienumenumsizeiPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> m ()
ffienumenumsizeiPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynenumenumsizeiPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumsizeiPtrfloatIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ()

ffienumenumsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> m ()
ffienumenumsizeiPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumenumsizeiPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumsizeiPtrintIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ()

ffienumenumsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> m ()
ffienumenumsizeiPtrintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumsizeiPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumsizeiPtrushortIOV :: FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ()) -> GLenum -> GLsizei -> Ptr GLushort -> IO ()

ffienumsizeiPtrushortIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ()) -> GLenum -> GLsizei -> Ptr GLushort -> m ()
ffienumsizeiPtrushortIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtrushortIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeiPtrubyteIOV :: FunPtr (GLsizei -> Ptr GLubyte -> IO ()) -> GLsizei -> Ptr GLubyte -> IO ()

ffisizeiPtrubyteIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLubyte -> IO ()) -> GLsizei -> Ptr GLubyte -> m ()
ffisizeiPtrubyteIOV fp v0 v1 = liftIO (dynsizeiPtrubyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumenumenumsizeiPtrVsizeiPtrVPtrVIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> IO ()

ffienumenumenumsizeiPtrVsizeiPtrVPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLenum -> GLsizei -> Ptr () -> GLsizei -> Ptr () -> Ptr () -> m ()
ffienumenumenumsizeiPtrVsizeiPtrVPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumenumenumsizeiPtrVsizeiPtrVPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumintenumenumsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumintenumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumintenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumintenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintsizeiPtrdoubleIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ()

ffiuintintsizeiPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLdouble -> m ()
ffiuintintsizeiPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintsizeiPtrfloatIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ()

ffiuintintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLfloat -> m ()
ffiuintintsizeiPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintsizeiPtrint64IOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ()

ffiuintintsizeiPtrint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLint64 -> m ()
ffiuintintsizeiPtrint64IOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtrint64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintsizeiPtrintIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ()

ffiuintintsizeiPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLint -> m ()
ffiuintintsizeiPtrintIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintsizeiPtruint64IOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ()

ffiuintintsizeiPtruint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
ffiuintintsizeiPtruint64IOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtruint64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintsizeiPtruintIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ()

ffiuintintsizeiPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLuint -> m ()
ffiuintintsizeiPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynbyteIOV :: FunPtr (GLbyte -> IO ()) -> GLbyte -> IO ()

ffibyteIOV :: MonadIO m => FunPtr (GLbyte -> IO ()) -> GLbyte -> m ()
ffibyteIOV fp v0 = liftIO (dynbyteIOV fp v0)

foreign import CALLCONV "dynamic" dynshortIOV :: FunPtr (GLshort -> IO ()) -> GLshort -> IO ()

ffishortIOV :: MonadIO m => FunPtr (GLshort -> IO ()) -> GLshort -> m ()
ffishortIOV fp v0 = liftIO (dynshortIOV fp v0)

foreign import CALLCONV "dynamic" dynubyteIOV :: FunPtr (GLubyte -> IO ()) -> GLubyte -> IO ()

ffiubyteIOV :: MonadIO m => FunPtr (GLubyte -> IO ()) -> GLubyte -> m ()
ffiubyteIOV fp v0 = liftIO (dynubyteIOV fp v0)

foreign import CALLCONV "dynamic" dynushortIOV :: FunPtr (GLushort -> IO ()) -> GLushort -> IO ()

ffiushortIOV :: MonadIO m => FunPtr (GLushort -> IO ()) -> GLushort -> m ()
ffiushortIOV fp v0 = liftIO (dynushortIOV fp v0)

foreign import CALLCONV "dynamic" dynenumsizeienumbooleanIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLboolean -> IO ()

ffienumsizeienumbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLboolean -> m ()
ffienumsizeienumbooleanIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeienumbooleanIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintptrbitfieldIOsync :: FunPtr (GLenum -> GLintptr -> GLbitfield -> IO GLsync) -> GLenum -> GLintptr -> GLbitfield -> IO GLsync

ffienumintptrbitfieldIOsync :: MonadIO m => FunPtr (GLenum -> GLintptr -> GLbitfield -> IO GLsync) -> GLenum -> GLintptr -> GLbitfield -> m GLsync
ffienumintptrbitfieldIOsync fp v0 v1 v2 = liftIO (dynenumintptrbitfieldIOsync fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumsizeisizeiPtrVIOV :: FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ()

ffienumsizeisizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLsizei -> GLsizei -> Ptr () -> m ()
ffienumsizeisizeiPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeisizeiPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynsizeiPtrcharIOV :: FunPtr (GLsizei -> Ptr GLchar -> IO ()) -> GLsizei -> Ptr GLchar -> IO ()

ffisizeiPtrcharIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLchar -> IO ()) -> GLsizei -> Ptr GLchar -> m ()
ffisizeiPtrcharIOV fp v0 v1 = liftIO (dynsizeiPtrcharIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynsizeiPtrintIOV :: FunPtr (GLsizei -> Ptr GLint -> IO ()) -> GLsizei -> Ptr GLint -> IO ()

ffisizeiPtrintIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLint -> IO ()) -> GLsizei -> Ptr GLint -> m ()
ffisizeiPtrintIOV fp v0 v1 = liftIO (dynsizeiPtrintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintuintuintfloatIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLfloat -> IO ()) -> GLuint -> GLuint -> GLuint -> GLfloat -> IO ()

ffiuintuintuintfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLfloat -> IO ()) -> GLuint -> GLuint -> GLuint -> GLfloat -> m ()
ffiuintuintuintfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintuintuintfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtrenumintintsizeisizeiIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiuintsizeiPtrenumintintsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiuintsizeiPtrenumintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintsizeiPtrenumintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumsizeiPtrenumintintsizeisizeiIOV :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffienumsizeiPtrenumintintsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffienumsizeiPtrenumintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumsizeiPtrenumintintsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintintIOV :: FunPtr (GLuint -> GLint -> IO ()) -> GLuint -> GLint -> IO ()

ffiuintintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> IO ()) -> GLuint -> GLint -> m ()
ffiuintintIOV fp v0 v1 = liftIO (dynuintintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintintintintintsizeisizeisizeiIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ()

ffiuintintintintintsizeisizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> m ()
ffiuintintintintintsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintintintintintsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumIOboolean :: FunPtr (GLenum -> IO GLboolean) -> GLenum -> IO GLboolean

ffienumIOboolean :: MonadIO m => FunPtr (GLenum -> IO GLboolean) -> GLenum -> m GLboolean
ffienumIOboolean fp v0 = liftIO (dynenumIOboolean fp v0)

foreign import CALLCONV "dynamic" dynenumuintIOboolean :: FunPtr (GLenum -> GLuint -> IO GLboolean) -> GLenum -> GLuint -> IO GLboolean

ffienumuintIOboolean :: MonadIO m => FunPtr (GLenum -> GLuint -> IO GLboolean) -> GLenum -> GLuint -> m GLboolean
ffienumuintIOboolean fp v0 v1 = liftIO (dynenumuintIOboolean fp v0 v1)

foreign import CALLCONV "dynamic" dynuint64IOboolean :: FunPtr (GLuint64 -> IO GLboolean) -> GLuint64 -> IO GLboolean

ffiuint64IOboolean :: MonadIO m => FunPtr (GLuint64 -> IO GLboolean) -> GLuint64 -> m GLboolean
ffiuint64IOboolean fp v0 = liftIO (dynuint64IOboolean fp v0)

foreign import CALLCONV "dynamic" dynintPtrcharIOboolean :: FunPtr (GLint -> Ptr GLchar -> IO GLboolean) -> GLint -> Ptr GLchar -> IO GLboolean

ffiintPtrcharIOboolean :: MonadIO m => FunPtr (GLint -> Ptr GLchar -> IO GLboolean) -> GLint -> Ptr GLchar -> m GLboolean
ffiintPtrcharIOboolean fp v0 v1 = liftIO (dynintPtrcharIOboolean fp v0 v1)

foreign import CALLCONV "dynamic" dynuintuintfloatfloatIOboolean :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> IO GLboolean) -> GLuint -> GLuint -> GLfloat -> GLfloat -> IO GLboolean

ffiuintuintfloatfloatIOboolean :: MonadIO m => FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> IO GLboolean) -> GLuint -> GLuint -> GLfloat -> GLfloat -> m GLboolean
ffiuintuintfloatfloatIOboolean fp v0 v1 v2 v3 = liftIO (dynuintuintfloatfloatIOboolean fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintfloatfloatIOboolean :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO GLboolean) -> GLuint -> GLfloat -> GLfloat -> IO GLboolean

ffiuintfloatfloatIOboolean :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> IO GLboolean) -> GLuint -> GLfloat -> GLfloat -> m GLboolean
ffiuintfloatfloatIOboolean fp v0 v1 v2 = liftIO (dynuintfloatfloatIOboolean fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsyncIOboolean :: FunPtr (GLsync -> IO GLboolean) -> GLsync -> IO GLboolean

ffisyncIOboolean :: MonadIO m => FunPtr (GLsync -> IO GLboolean) -> GLsync -> m GLboolean
ffisyncIOboolean fp v0 = liftIO (dynsyncIOboolean fp v0)

foreign import CALLCONV "dynamic" dynuintenumIOboolean :: FunPtr (GLuint -> GLenum -> IO GLboolean) -> GLuint -> GLenum -> IO GLboolean

ffiuintenumIOboolean :: MonadIO m => FunPtr (GLuint -> GLenum -> IO GLboolean) -> GLuint -> GLenum -> m GLboolean
ffiuintenumIOboolean fp v0 v1 = liftIO (dynuintenumIOboolean fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtrcharIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ()

ffienumuintsizeiPtrcharIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> m ()
ffienumuintsizeiPtrcharIOV fp v0 v1 v2 v3 = liftIO (dynenumuintsizeiPtrcharIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintushortIOV :: FunPtr (GLint -> GLushort -> IO ()) -> GLint -> GLushort -> IO ()

ffiintushortIOV :: MonadIO m => FunPtr (GLint -> GLushort -> IO ()) -> GLint -> GLushort -> m ()
ffiintushortIOV fp v0 v1 = liftIO (dynintushortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintuintPtrPtrVPtrsizeiPtruintPtruintuintIOV :: FunPtr (GLuint -> GLuint -> Ptr (Ptr ()) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> Ptr (Ptr ()) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()

ffiuintuintPtrPtrVPtrsizeiPtruintPtruintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> Ptr (Ptr ()) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> Ptr (Ptr ()) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> m ()
ffiuintuintPtrPtrVPtrsizeiPtruintPtruintuintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintPtrPtrVPtrsizeiPtruintPtruintuintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumfloatIOV :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ()) -> GLuint -> GLenum -> GLfloat -> IO ()

ffiuintenumfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLfloat -> IO ()) -> GLuint -> GLenum -> GLfloat -> m ()
ffiuintenumfloatIOV fp v0 v1 v2 = liftIO (dynuintenumfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumintIOV :: FunPtr (GLuint -> GLenum -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> IO ()

ffiuintenumintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> IO ()) -> GLuint -> GLenum -> GLint -> m ()
ffiuintenumintIOV fp v0 v1 v2 = liftIO (dynuintenumintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtrubyteIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> IO ()

ffienumuintsizeiPtrubyteIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> m ()
ffienumuintsizeiPtrubyteIOV fp v0 v1 v2 v3 = liftIO (dynenumuintsizeiPtrubyteIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintsizeiIOV :: FunPtr (GLint -> GLsizei -> IO ()) -> GLint -> GLsizei -> IO ()

ffiintsizeiIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> IO ()) -> GLint -> GLsizei -> m ()
ffiintsizeiIOV fp v0 v1 = liftIO (dynintsizeiIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuint64IOV :: FunPtr (GLuint64 -> IO ()) -> GLuint64 -> IO ()

ffiuint64IOV :: MonadIO m => FunPtr (GLuint64 -> IO ()) -> GLuint64 -> m ()
ffiuint64IOV fp v0 = liftIO (dynuint64IOV fp v0)

foreign import CALLCONV "dynamic" dynuint64enumIOV :: FunPtr (GLuint64 -> GLenum -> IO ()) -> GLuint64 -> GLenum -> IO ()

ffiuint64enumIOV :: MonadIO m => FunPtr (GLuint64 -> GLenum -> IO ()) -> GLuint64 -> GLenum -> m ()
ffiuint64enumIOV fp v0 v1 = liftIO (dynuint64enumIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumdoubledoubleintintPtrdoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()

ffienumdoubledoubleintintPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
ffienumdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumfloatfloatintintPtrfloatIOV :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()

ffienumfloatfloatintintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
ffienumfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumfixedfixedintintfixedIOV :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ()

ffienumfixedfixedintintfixedIOV :: MonadIO m => FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> m ()
ffienumfixedfixedintintfixedIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumfixedfixedintintfixedIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumdoubledoubleintintdoubledoubleintintPtrdoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()

ffienumdoubledoubleintintdoubledoubleintintPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
ffienumdoubledoubleintintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumdoubledoubleintintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumfloatfloatintintfloatfloatintintPtrfloatIOV :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()

ffienumfloatfloatintintfloatfloatintintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
ffienumfloatfloatintintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumfloatfloatintintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumfixedfixedintintfixedfixedintintfixedIOV :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ()

ffienumfixedfixedintintfixedfixedintintfixedIOV :: MonadIO m => FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> m ()
ffienumfixedfixedintintfixedfixedintintfixedIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumfixedfixedintintfixedfixedintintfixedIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumenumIOPtrV :: FunPtr (GLenum -> GLenum -> IO (Ptr ())) -> GLenum -> GLenum -> IO (Ptr ())

ffienumenumIOPtrV :: MonadIO m => FunPtr (GLenum -> GLenum -> IO (Ptr ())) -> GLenum -> GLenum -> m (Ptr ())
ffienumenumIOPtrV fp v0 v1 = liftIO (dynenumenumIOPtrV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumintptrsizeiptrbitfieldIOPtrV :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ())) -> GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ())

ffienumintptrsizeiptrbitfieldIOPtrV :: MonadIO m => FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ())) -> GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> m (Ptr ())
ffienumintptrsizeiptrbitfieldIOPtrV fp v0 v1 v2 v3 = liftIO (dynenumintptrsizeiptrbitfieldIOPtrV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintenumsizeisizeiintintbooleanPtrVIOV :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr () -> IO ()) -> GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr () -> IO ()

ffienumuintenumsizeisizeiintintbooleanPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr () -> IO ()) -> GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr () -> m ()
ffienumuintenumsizeisizeiintintbooleanPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumuintenumsizeisizeiintintbooleanPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynintdoubledoubleIOV :: FunPtr (GLint -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> IO ()

ffiintdoubledoubleIOV :: MonadIO m => FunPtr (GLint -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> m ()
ffiintdoubledoubleIOV fp v0 v1 v2 = liftIO (dynintdoubledoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintfloatfloatIOV :: FunPtr (GLint -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> IO ()

ffiintfloatfloatIOV :: MonadIO m => FunPtr (GLint -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> m ()
ffiintfloatfloatIOV fp v0 v1 v2 = liftIO (dynintfloatfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintfixedfixedIOV :: FunPtr (GLint -> GLfixed -> GLfixed -> IO ()) -> GLint -> GLfixed -> GLfixed -> IO ()

ffiintfixedfixedIOV :: MonadIO m => FunPtr (GLint -> GLfixed -> GLfixed -> IO ()) -> GLint -> GLfixed -> GLfixed -> m ()
ffiintfixedfixedIOV fp v0 v1 v2 = liftIO (dynintfixedfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintdoubledoubleintdoubledoubleIOV :: FunPtr (GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()

ffiintdoubledoubleintdoubledoubleIOV :: MonadIO m => FunPtr (GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> m ()
ffiintdoubledoubleintdoubledoubleIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynintdoubledoubleintdoubledoubleIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynintfloatfloatintfloatfloatIOV :: FunPtr (GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()

ffiintfloatfloatintfloatfloatIOV :: MonadIO m => FunPtr (GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> m ()
ffiintfloatfloatintfloatfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynintfloatfloatintfloatfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynintfixedfixedfixedfixedIOV :: FunPtr (GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffiintfixedfixedfixedfixedIOV :: MonadIO m => FunPtr (GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
ffiintfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4 = liftIO (dynintfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumIOPtrV :: FunPtr (GLuint -> GLenum -> IO (Ptr ())) -> GLuint -> GLenum -> IO (Ptr ())

ffiuintenumIOPtrV :: MonadIO m => FunPtr (GLuint -> GLenum -> IO (Ptr ())) -> GLuint -> GLenum -> m (Ptr ())
ffiuintenumIOPtrV fp v0 v1 = liftIO (dynuintenumIOPtrV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintintptrsizeiptrbitfieldIOPtrV :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ())) -> GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ())

ffiuintintptrsizeiptrbitfieldIOPtrV :: MonadIO m => FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ())) -> GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> m (Ptr ())
ffiuintintptrsizeiptrbitfieldIOPtrV fp v0 v1 v2 v3 = liftIO (dynuintintptrsizeiptrbitfieldIOPtrV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintIOPtrV :: FunPtr (GLuint -> IO (Ptr ())) -> GLuint -> IO (Ptr ())

ffiuintIOPtrV :: MonadIO m => FunPtr (GLuint -> IO (Ptr ())) -> GLuint -> m (Ptr ())
ffiuintIOPtrV fp v0 = liftIO (dynuintIOPtrV fp v0)

foreign import CALLCONV "dynamic" dynuintintbitfieldPtrintPtrenumIOPtrV :: FunPtr (GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> IO (Ptr ())) -> GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> IO (Ptr ())

ffiuintintbitfieldPtrintPtrenumIOPtrV :: MonadIO m => FunPtr (GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> IO (Ptr ())) -> GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> m (Ptr ())
ffiuintintbitfieldPtrintPtrenumIOPtrV fp v0 v1 v2 v3 v4 = liftIO (dynuintintbitfieldPtrintPtrenumIOPtrV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintdoubledoubleintintPtrdoubleIOV :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()

ffiuintuintdoubledoubleintintPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
ffiuintuintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintuintfloatfloatintintPtrfloatIOV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()

ffiuintuintfloatfloatintintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
ffiuintuintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintuintdoubledoubleintintdoubledoubleintintPtrdoubleIOV :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()

ffiuintuintdoubledoubleintintdoubledoubleintintPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ()) -> GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> m ()
ffiuintuintdoubledoubleintintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintuintdoubledoubleintintdoubledoubleintintPtrdoubleIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynuintuintfloatfloatintintfloatfloatintintPtrfloatIOV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()

ffiuintuintfloatfloatintintfloatfloatintintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> m ()
ffiuintuintfloatfloatintintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintuintfloatfloatintintfloatfloatintintPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynenumdoubledoubledoubledoubledoubledoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffienumdoubledoubledoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffienumdoubledoubledoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumdoubledoubledoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynintPtrubyteIOV :: FunPtr (GLint -> Ptr GLubyte -> IO ()) -> GLint -> Ptr GLubyte -> IO ()

ffiintPtrubyteIOV :: MonadIO m => FunPtr (GLint -> Ptr GLubyte -> IO ()) -> GLint -> Ptr GLubyte -> m ()
ffiintPtrubyteIOV fp v0 v1 = liftIO (dynintPtrubyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtruintIOV :: FunPtr (GLint -> Ptr GLuint -> IO ()) -> GLint -> Ptr GLuint -> IO ()

ffiintPtruintIOV :: MonadIO m => FunPtr (GLint -> Ptr GLuint -> IO ()) -> GLint -> Ptr GLuint -> m ()
ffiintPtruintIOV fp v0 v1 = liftIO (dynintPtruintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrushortIOV :: FunPtr (GLint -> Ptr GLushort -> IO ()) -> GLint -> Ptr GLushort -> IO ()

ffiintPtrushortIOV :: MonadIO m => FunPtr (GLint -> Ptr GLushort -> IO ()) -> GLint -> Ptr GLushort -> m ()
ffiintPtrushortIOV fp v0 v1 = liftIO (dynintPtrushortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumdoubledoubledoubledoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffienumdoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffienumdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumfloatfloatfloatfloatIOV :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffienumfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffienumfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumdoubledoubledoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffienumdoubledoubledoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> GLdouble -> m ()
ffienumdoubledoubledoubleIOV fp v0 v1 v2 v3 = liftIO (dynenumdoubledoubledoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumfloatfloatfloatIOV :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffienumfloatfloatfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> GLfloat -> m ()
ffienumfloatfloatfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumfloatfloatfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumbooleanIOV :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLboolean -> IO ()

ffienumenumbooleanIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLboolean -> IO ()) -> GLenum -> GLenum -> GLboolean -> m ()
ffienumenumbooleanIOV fp v0 v1 v2 = liftIO (dynenumenumbooleanIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumPtrintPtrsizeisizeiIOV :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ()) -> GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ()

ffienumPtrintPtrsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ()) -> GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> m ()
ffienumPtrintPtrsizeisizeiIOV fp v0 v1 v2 v3 = liftIO (dynenumPtrintPtrsizeisizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumPtrVsizeisizeiIOV :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ()) -> GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ()

ffienumPtrVsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ()) -> GLenum -> Ptr () -> GLsizei -> GLsizei -> m ()
ffienumPtrVsizeisizeiIOV fp v0 v1 v2 v3 = liftIO (dynenumPtrVsizeisizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumPtrVsizeisizeisizeiintIOV :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ()

ffienumPtrVsizeisizeisizeiintIOV :: MonadIO m => FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> m ()
ffienumPtrVsizeisizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumPtrVsizeisizeisizeiintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumPtrVsizeisizeiintIOV :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ()

ffienumPtrVsizeisizeiintIOV :: MonadIO m => FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> m ()
ffienumPtrVsizeisizeiintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumPtrVsizeisizeiintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintptrintptrsizeisizeiIOV :: FunPtr (GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ()

ffienumintptrintptrsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> m ()
ffienumintptrintptrsizeisizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumintptrintptrsizeisizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumPtrsizeienumPtrPtrVsizeiIOV :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> IO ()) -> GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> IO ()

ffienumPtrsizeienumPtrPtrVsizeiIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> IO ()) -> GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> m ()
ffienumPtrsizeienumPtrPtrVsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumPtrsizeienumPtrPtrVsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumPtrsizeienumPtrPtrVsizeiPtrintIOV :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> IO ()

ffienumPtrsizeienumPtrPtrVsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> m ()
ffienumPtrsizeienumPtrPtrVsizeiPtrintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumPtrsizeienumPtrPtrVsizeiPtrintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumPtrVsizeisizeiIOV :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ()

ffienumenumPtrVsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> m ()
ffienumenumPtrVsizeisizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumPtrVsizeisizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumPtrVsizeisizeisizeiintIOV :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ()

ffienumenumPtrVsizeisizeisizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> m ()
ffienumenumPtrVsizeisizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumPtrVsizeisizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumenumPtrVsizeisizeiintIOV :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ()

ffienumenumPtrVsizeisizeiintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ()) -> GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> m ()
ffienumenumPtrVsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumPtrVsizeisizeiintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumintptrintptrsizeisizeiIOV :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ()

ffienumenumintptrintptrsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> m ()
ffienumenumintptrintptrsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumenumintptrintptrsizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumuintuintPtrintPtrsizeisizeiIOV :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ()

ffienumuintuintPtrintPtrsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ()) -> GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> m ()
ffienumuintuintPtrintPtrsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintuintPtrintPtrsizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynPtrenumPtrintPtrsizeisizeiintIOV :: FunPtr (Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> IO ()) -> Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> IO ()

ffiPtrenumPtrintPtrsizeisizeiintIOV :: MonadIO m => FunPtr (Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> IO ()) -> Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> m ()
ffiPtrenumPtrintPtrsizeisizeiintIOV fp v0 v1 v2 v3 v4 = liftIO (dynPtrenumPtrintPtrsizeisizeiintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynPtrenumPtrsizeienumPtrPtrVsizeiintIOV :: FunPtr (Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GLint -> IO ()) -> Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GLint -> IO ()

ffiPtrenumPtrsizeienumPtrPtrVsizeiintIOV :: MonadIO m => FunPtr (Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GLint -> IO ()) -> Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GLint -> m ()
ffiPtrenumPtrsizeienumPtrPtrVsizeiintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynPtrenumPtrsizeienumPtrPtrVsizeiintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumbyteIOV :: FunPtr (GLenum -> GLbyte -> IO ()) -> GLenum -> GLbyte -> IO ()

ffienumbyteIOV :: MonadIO m => FunPtr (GLenum -> GLbyte -> IO ()) -> GLenum -> GLbyte -> m ()
ffienumbyteIOV fp v0 v1 = liftIO (dynenumbyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumPtrbyteIOV :: FunPtr (GLenum -> Ptr GLbyte -> IO ()) -> GLenum -> Ptr GLbyte -> IO ()

ffienumPtrbyteIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLbyte -> IO ()) -> GLenum -> Ptr GLbyte -> m ()
ffienumPtrbyteIOV fp v0 v1 = liftIO (dynenumPtrbyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumdoubleIOV :: FunPtr (GLenum -> GLdouble -> IO ()) -> GLenum -> GLdouble -> IO ()

ffienumdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> IO ()) -> GLenum -> GLdouble -> m ()
ffienumdoubleIOV fp v0 v1 = liftIO (dynenumdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumhalfNVIOV :: FunPtr (GLenum -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> IO ()

ffienumhalfNVIOV :: MonadIO m => FunPtr (GLenum -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> m ()
ffienumhalfNVIOV fp v0 v1 = liftIO (dynenumhalfNVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumPtrhalfNVIOV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ()) -> GLenum -> Ptr GLhalfNV -> IO ()

ffienumPtrhalfNVIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLhalfNV -> IO ()) -> GLenum -> Ptr GLhalfNV -> m ()
ffienumPtrhalfNVIOV fp v0 v1 = liftIO (dynenumPtrhalfNVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumshortIOV :: FunPtr (GLenum -> GLshort -> IO ()) -> GLenum -> GLshort -> IO ()

ffienumshortIOV :: MonadIO m => FunPtr (GLenum -> GLshort -> IO ()) -> GLenum -> GLshort -> m ()
ffienumshortIOV fp v0 v1 = liftIO (dynenumshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumPtrshortIOV :: FunPtr (GLenum -> Ptr GLshort -> IO ()) -> GLenum -> Ptr GLshort -> IO ()

ffienumPtrshortIOV :: MonadIO m => FunPtr (GLenum -> Ptr GLshort -> IO ()) -> GLenum -> Ptr GLshort -> m ()
ffienumPtrshortIOV fp v0 v1 = liftIO (dynenumPtrshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumbytebyteIOV :: FunPtr (GLenum -> GLbyte -> GLbyte -> IO ()) -> GLenum -> GLbyte -> GLbyte -> IO ()

ffienumbytebyteIOV :: MonadIO m => FunPtr (GLenum -> GLbyte -> GLbyte -> IO ()) -> GLenum -> GLbyte -> GLbyte -> m ()
ffienumbytebyteIOV fp v0 v1 v2 = liftIO (dynenumbytebyteIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumdoubledoubleIOV :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> IO ()

ffienumdoubledoubleIOV :: MonadIO m => FunPtr (GLenum -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLdouble -> GLdouble -> m ()
ffienumdoubledoubleIOV fp v0 v1 v2 = liftIO (dynenumdoubledoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumfloatfloatIOV :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> IO ()

ffienumfloatfloatIOV :: MonadIO m => FunPtr (GLenum -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLfloat -> GLfloat -> m ()
ffienumfloatfloatIOV fp v0 v1 v2 = liftIO (dynenumfloatfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumhalfNVhalfNVIOV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> GLhalfNV -> IO ()

ffienumhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> GLhalfNV -> m ()
ffienumhalfNVhalfNVIOV fp v0 v1 v2 = liftIO (dynenumhalfNVhalfNVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumshortshortIOV :: FunPtr (GLenum -> GLshort -> GLshort -> IO ()) -> GLenum -> GLshort -> GLshort -> IO ()

ffienumshortshortIOV :: MonadIO m => FunPtr (GLenum -> GLshort -> GLshort -> IO ()) -> GLenum -> GLshort -> GLshort -> m ()
ffienumshortshortIOV fp v0 v1 v2 = liftIO (dynenumshortshortIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumfixedfixedIOV :: FunPtr (GLenum -> GLfixed -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> IO ()

ffienumfixedfixedIOV :: MonadIO m => FunPtr (GLenum -> GLfixed -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> m ()
ffienumfixedfixedIOV fp v0 v1 v2 = liftIO (dynenumfixedfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumbytebytebyteIOV :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ()

ffienumbytebytebyteIOV :: MonadIO m => FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLenum -> GLbyte -> GLbyte -> GLbyte -> m ()
ffienumbytebytebyteIOV fp v0 v1 v2 v3 = liftIO (dynenumbytebytebyteIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumhalfNVhalfNVhalfNVIOV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()

ffienumhalfNVhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
ffienumhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 = liftIO (dynenumhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumintintintIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> IO ()

ffienumintintintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> m ()
ffienumintintintIOV fp v0 v1 v2 v3 = liftIO (dynenumintintintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumshortshortshortIOV :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ()) -> GLenum -> GLshort -> GLshort -> GLshort -> IO ()

ffienumshortshortshortIOV :: MonadIO m => FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ()) -> GLenum -> GLshort -> GLshort -> GLshort -> m ()
ffienumshortshortshortIOV fp v0 v1 v2 v3 = liftIO (dynenumshortshortshortIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumfixedfixedfixedIOV :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffienumfixedfixedfixedIOV :: MonadIO m => FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLfixed -> m ()
ffienumfixedfixedfixedIOV fp v0 v1 v2 v3 = liftIO (dynenumfixedfixedfixedIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumbytebytebytebyteIOV :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()

ffienumbytebytebytebyteIOV :: MonadIO m => FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ()) -> GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> m ()
ffienumbytebytebytebyteIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumbytebytebytebyteIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumhalfNVhalfNVhalfNVhalfNVIOV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()

ffienumhalfNVhalfNVhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
ffienumhalfNVhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumhalfNVhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumshortshortshortshortIOV :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()

ffienumshortshortshortshortIOV :: MonadIO m => FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
ffienumshortshortshortshortIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumshortshortshortshortIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumfixedfixedfixedfixedIOV :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()

ffienumfixedfixedfixedfixedIOV :: MonadIO m => FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ()) -> GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> m ()
ffienumfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumfixedfixedfixedfixedIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumintenumsizeiPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumintenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumintenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumintenumsizeiPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumenumfloatIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ()) -> GLenum -> GLenum -> GLenum -> GLfloat -> IO ()

ffienumenumenumfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ()) -> GLenum -> GLenum -> GLenum -> GLfloat -> m ()
ffienumenumenumfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumintIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLint -> IO ()

ffienumenumenumintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ()) -> GLenum -> GLenum -> GLenum -> GLint -> m ()
ffienumenumenumintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumdoubleIOV :: FunPtr (GLenum -> GLenum -> GLenum -> GLdouble -> IO ()) -> GLenum -> GLenum -> GLenum -> GLdouble -> IO ()

ffienumenumenumdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> GLdouble -> IO ()) -> GLenum -> GLenum -> GLenum -> GLdouble -> m ()
ffienumenumenumdoubleIOV fp v0 v1 v2 v3 = liftIO (dynenumenumenumdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumintintsizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintintsizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintintsizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumenumintintsizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumenumintintsizeisizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintintsizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintintsizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumenumintintsizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumenumintintsizeisizeisizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintintsizeisizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintintsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynenumenumintintsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynenumenumintintsizeienumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintintsizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumenumintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumenumintintintsizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintintintsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumenumintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumenumintintintintsizeisizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumenumintintintintsizeisizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumenumintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynenumenumintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynuintsizeiptrPtrVenumIOV :: FunPtr (GLuint -> GLsizeiptr -> Ptr () -> GLenum -> IO ()) -> GLuint -> GLsizeiptr -> Ptr () -> GLenum -> IO ()

ffiuintsizeiptrPtrVenumIOV :: MonadIO m => FunPtr (GLuint -> GLsizeiptr -> Ptr () -> GLenum -> IO ()) -> GLuint -> GLsizeiptr -> Ptr () -> GLenum -> m ()
ffiuintsizeiptrPtrVenumIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiptrPtrVenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintptrsizeiptrbooleanIOV :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ()) -> GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ()

ffiuintintptrsizeiptrbooleanIOV :: MonadIO m => FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ()) -> GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> m ()
ffiuintintptrsizeiptrbooleanIOV fp v0 v1 v2 v3 = liftIO (dynuintintptrsizeiptrbooleanIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiptrPtrVbitfieldIOV :: FunPtr (GLuint -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ()) -> GLuint -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ()

ffiuintsizeiptrPtrVbitfieldIOV :: MonadIO m => FunPtr (GLuint -> GLsizeiptr -> Ptr () -> GLbitfield -> IO ()) -> GLuint -> GLsizeiptr -> Ptr () -> GLbitfield -> m ()
ffiuintsizeiptrPtrVbitfieldIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiptrPtrVbitfieldIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumuintIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> IO ()

ffiuintenumenumuintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> m ()
ffiuintenumenumuintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtrfloatIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffiuintuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffiuintuintsizeiPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintuintsizeiPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintuintPtrfloatIOV :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLuint -> Ptr GLfloat -> IO ()

ffiuintuintuintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> GLuint -> GLuint -> Ptr GLfloat -> m ()
ffiuintuintuintPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintuintuintPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumuintintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> IO ()

ffiuintenumuintintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> m ()
ffiuintenumuintintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumuintintIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ()

ffiuintenumenumuintintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> GLint -> m ()
ffiuintenumenumuintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumenumuintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumenumuintintintIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()

ffiuintenumenumuintintintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> m ()
ffiuintenumenumuintintintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintenumenumuintintintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumuintintenumIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLenum -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> GLenum -> IO ()

ffiuintenumuintintenumIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLenum -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> GLenum -> m ()
ffiuintenumuintintenumIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintintenumIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumuintintintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ()

ffiuintenumuintintintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> GLint -> m ()
ffiuintenumuintintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumuintdoubledoubledoubledoubleIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiuintenumuintdoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiuintenumuintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumuintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumuintfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintenumuintfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintenumuintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumuintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumuintintintintintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

ffiuintenumuintintintintintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
ffiuintenumuintintintintintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumuintintintintintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumuintuintuintuintuintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintenumuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintenumuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumuintsizeiPtrfloatIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffiuintenumuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffiuintenumuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumuintsizeiPtrintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ()

ffiuintenumuintsizeiPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> m ()
ffiuintenumuintsizeiPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintsizeiPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumuintsizeiPtruintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ()

ffiuintenumuintsizeiPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> m ()
ffiuintenumuintsizeiPtruintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintsizeiPtruintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumenumsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintenumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumsizeisizeiIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLsizei -> IO ()

ffiuintenumsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLsizei -> m ()
ffiuintenumsizeisizeiIOV fp v0 v1 v2 v3 = liftIO (dynuintenumsizeisizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeienumsizeisizeiIOV :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()

ffiuintsizeienumsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
ffiuintsizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintsizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintsizeisizeienumsizeisizeiIOV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()

ffiuintsizeisizeienumsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
ffiuintsizeisizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintsizeisizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumintPtrcharintPtrcharIOV :: FunPtr (GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> IO ()) -> GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> IO ()

ffienumintPtrcharintPtrcharIOV :: MonadIO m => FunPtr (GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> IO ()) -> GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> m ()
ffienumintPtrcharintPtrcharIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumintPtrcharintPtrcharIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynsizeiPtrVenumIOuint :: FunPtr (GLsizei -> Ptr () -> GLenum -> IO GLuint) -> GLsizei -> Ptr () -> GLenum -> IO GLuint

ffisizeiPtrVenumIOuint :: MonadIO m => FunPtr (GLsizei -> Ptr () -> GLenum -> IO GLuint) -> GLsizei -> Ptr () -> GLenum -> m GLuint
ffisizeiPtrVenumIOuint fp v0 v1 v2 = liftIO (dynsizeiPtrVenumIOuint fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynPtrVsizeiPtrcharIOV :: FunPtr (Ptr () -> GLsizei -> Ptr GLchar -> IO ()) -> Ptr () -> GLsizei -> Ptr GLchar -> IO ()

ffiPtrVsizeiPtrcharIOV :: MonadIO m => FunPtr (Ptr () -> GLsizei -> Ptr GLchar -> IO ()) -> Ptr () -> GLsizei -> Ptr GLchar -> m ()
ffiPtrVsizeiPtrcharIOV fp v0 v1 v2 = liftIO (dynPtrVsizeiPtrcharIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumuintenumIOenum :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum) -> GLenum -> GLuint -> GLenum -> IO GLenum

ffienumuintenumIOenum :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum) -> GLenum -> GLuint -> GLenum -> m GLenum
ffienumuintenumIOenum fp v0 v1 v2 = liftIO (dynenumuintenumIOenum fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintenumIOV :: FunPtr (GLuint -> GLuint -> GLenum -> IO ()) -> GLuint -> GLuint -> GLenum -> IO ()

ffiuintuintenumIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> IO ()) -> GLuint -> GLuint -> GLenum -> m ()
ffiuintuintenumIOV fp v0 v1 v2 = liftIO (dynuintuintenumIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtrubytesizeienumPtrVIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ()

ffiuintsizeiPtrubytesizeienumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> m ()
ffiuintsizeiPtrubytesizeienumPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintsizeiPtrubytesizeienumPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintsizeienumPtrVIOV :: FunPtr (GLuint -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> GLenum -> Ptr () -> IO ()

ffiuintsizeienumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> GLenum -> Ptr () -> m ()
ffiuintsizeienumPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeienumPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtrVbitfielduintsizeiuintfloatIOenum :: FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum) -> GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum

ffiuintenumPtrVbitfielduintsizeiuintfloatIOenum :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum) -> GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> m GLenum
ffiuintenumPtrVbitfielduintsizeiuintfloatIOenum fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumPtrVbitfielduintsizeiuintfloatIOenum fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumPtrVbitfielduintfloatPtruintIOenum :: FunPtr (GLenum -> Ptr () -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> IO GLenum) -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> IO GLenum

ffienumPtrVbitfielduintfloatPtruintIOenum :: MonadIO m => FunPtr (GLenum -> Ptr () -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> IO GLenum) -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> m GLenum
ffienumPtrVbitfielduintfloatPtruintIOenum fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumPtrVbitfielduintfloatPtruintIOenum fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumPtrVbitfielduintsizeienumuintfloatIOV :: FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> IO ()

ffiuintenumPtrVbitfielduintsizeienumuintfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> m ()
ffiuintenumPtrVbitfielduintsizeienumuintfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumPtrVbitfielduintsizeienumuintfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumPtrVbitfieldsizeienumPtrVenumuintfloatIOV :: FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLenum -> GLuint -> GLfloat -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLenum -> GLuint -> GLfloat -> IO ()

ffiuintenumPtrVbitfieldsizeienumPtrVenumuintfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr () -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLenum -> GLuint -> GLfloat -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLbitfield -> GLsizei -> GLenum -> Ptr () -> GLenum -> GLuint -> GLfloat -> m ()
ffiuintenumPtrVbitfieldsizeienumPtrVenumuintfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintenumPtrVbitfieldsizeienumPtrVenumuintfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintenumsizeiptrPtrVsizeiuintsizeiuintfloatIOenum :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> Ptr () -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum) -> GLuint -> GLenum -> GLsizeiptr -> Ptr () -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum

ffiuintenumsizeiptrPtrVsizeiuintsizeiuintfloatIOenum :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizeiptr -> Ptr () -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum) -> GLuint -> GLenum -> GLsizeiptr -> Ptr () -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> m GLenum
ffiuintenumsizeiptrPtrVsizeiuintsizeiuintfloatIOenum fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumsizeiptrPtrVsizeiuintsizeiuintfloatIOenum fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumintuintIOV :: FunPtr (GLenum -> GLint -> GLuint -> IO ()) -> GLenum -> GLint -> GLuint -> IO ()

ffienumintuintIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLuint -> IO ()) -> GLenum -> GLint -> GLuint -> m ()
ffienumintuintIOV fp v0 v1 v2 = liftIO (dynenumintuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumsizeiPtrVIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintenumsizeiPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintenumsizeiPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeisizeisizeiPtrubytesizeienumPtrVIOV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ()

ffiuintsizeisizeisizeiPtrubytesizeienumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr () -> m ()
ffiuintsizeisizeisizeiPtrubytesizeienumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintsizeisizeisizeiPtrubytesizeienumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintsizeisizeienumPtrVIOV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr () -> IO ()

ffiuintsizeisizeienumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr () -> m ()
ffiuintsizeisizeienumPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintsizeisizeienumPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumintPtrfloatIOV :: FunPtr (GLenum -> GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLint -> Ptr GLfloat -> IO ()

ffienumenumintPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLenum -> GLenum -> GLint -> Ptr GLfloat -> m ()
ffienumenumintPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynenumenumintPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeisizeifloatPtrfloatPtrfloatPtrfloatPtrfloatIOboolean :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO GLboolean) -> GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO GLboolean

ffiuintsizeisizeifloatPtrfloatPtrfloatPtrfloatPtrfloatIOboolean :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO GLboolean) -> GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m GLboolean
ffiuintsizeisizeifloatPtrfloatPtrfloatPtrfloatPtrfloatIOboolean fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintsizeisizeifloatPtrfloatPtrfloatPtrfloatPtrfloatIOboolean fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynPtrintIOint :: FunPtr (Ptr GLint -> IO GLint) -> Ptr GLint -> IO GLint

ffiPtrintIOint :: MonadIO m => FunPtr (Ptr GLint -> IO GLint) -> Ptr GLint -> m GLint
ffiPtrintIOint fp v0 = liftIO (dynPtrintIOint fp v0)

foreign import CALLCONV "dynamic" dynuintuint64EXTuintuintenumenumuintenumuintenumuintenumuintIOV :: FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> IO ()

ffiuintuint64EXTuintuintenumenumuintenumuintenumuintenumuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> m ()
ffiuintuint64EXTuintuintenumenumuintenumuintenumuintenumuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO (dynuintuint64EXTuintuintenumenumuintenumuintenumuintenumuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)

foreign import CALLCONV "dynamic" dynuintuint64EXTuintuintenumenumuintuintenumuintuintIOV :: FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> IO ()

ffiuintuint64EXTuintuintenumenumuintuintenumuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> m ()
ffiuintuint64EXTuintuintenumenumuintuintenumuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintuint64EXTuintuintenumenumuintuintenumuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynsizeiPtruintPtrfloatIOV :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ()) -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ()

ffisizeiPtruintPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ()) -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> m ()
ffisizeiPtruintPtrfloatIOV fp v0 v1 v2 = liftIO (dynsizeiPtruintPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeiPtruintPtrclampfIOV :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ()) -> GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ()

ffisizeiPtruintPtrclampfIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ()) -> GLsizei -> Ptr GLuint -> Ptr GLclampf -> m ()
ffisizeiPtruintPtrclampfIOV fp v0 v1 v2 = liftIO (dynsizeiPtruintPtrclampfIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeiPtruintPtrfixedIOV :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfixed -> IO ()) -> GLsizei -> Ptr GLuint -> Ptr GLfixed -> IO ()

ffisizeiPtruintPtrfixedIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfixed -> IO ()) -> GLsizei -> Ptr GLuint -> Ptr GLfixed -> m ()
ffisizeiPtruintPtrfixedIOV fp v0 v1 v2 = liftIO (dynsizeiPtruintPtrfixedIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintenumPtrVsizeiIOV :: FunPtr (GLuint -> GLenum -> Ptr () -> GLsizei -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLsizei -> IO ()

ffiuintenumPtrVsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr () -> GLsizei -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLsizei -> m ()
ffiuintenumPtrVsizeiIOV fp v0 v1 v2 v3 = liftIO (dynuintenumPtrVsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumPtrVintIOV :: FunPtr (GLuint -> GLenum -> Ptr () -> GLint -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLint -> IO ()

ffiuintenumPtrVintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr () -> GLint -> IO ()) -> GLuint -> GLenum -> Ptr () -> GLint -> m ()
ffiuintenumPtrVintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumPtrVintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintuintsizeiPtrintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> IO ()

ffienumuintuintsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> m ()
ffienumuintuintsizeiPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintuintsizeiPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintuintsizeiPtruintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> IO ()

ffienumuintuintsizeiPtruintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> m ()
ffienumuintuintsizeiPtruintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintuintsizeiPtruintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintuintsizeiPtrfloatIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()

ffienumuintuintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> m ()
ffienumuintuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintuintsizeiPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintdoubledoubledoubledoubleIOV :: FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffienumuintdoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffienumuintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumuintfloatfloatfloatfloatIOV :: FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffienumuintfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffienumuintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumuintintintintintIOV :: FunPtr (GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

ffienumuintintintintintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
ffienumuintintintintintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumuintintintintintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtrintIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ()

ffienumuintsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> m ()
ffienumuintsizeiPtrintIOV fp v0 v1 v2 v3 = liftIO (dynenumuintsizeiPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtrubytedoubledoubledoubledoubleIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiuintsizeiPtrubytedoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiuintsizeiPtrubytedoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintsizeiPtrubytedoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintsizeiPtrubytefloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintsizeiPtrubytefloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintsizeiPtrubytefloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintsizeiPtrubytefloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumuintsizeiPtrdoubleIOV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> IO ()

ffienumuintsizeiPtrdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> m ()
ffienumuintsizeiPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynenumuintsizeiPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintenumintPtrfloatIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> IO ()

ffiuintintenumintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> m ()
ffiuintintenumintPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintenumintPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumsizeiPtrVIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffienumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffienumenumsizeiPtrVIOV fp v0 v1 v2 v3 = liftIO (dynenumenumsizeiPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintdoubleIOV :: FunPtr (GLuint -> GLint -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> IO ()

ffiuintintdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> m ()
ffiuintintdoubleIOV fp v0 v1 v2 = liftIO (dynuintintdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintfloatIOV :: FunPtr (GLuint -> GLint -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> IO ()

ffiuintintfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> m ()
ffiuintintfloatIOV fp v0 v1 v2 = liftIO (dynuintintfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintintIOV :: FunPtr (GLuint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> IO ()

ffiuintintintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> m ()
ffiuintintintIOV fp v0 v1 v2 = liftIO (dynuintintintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintint64IOV :: FunPtr (GLuint -> GLint -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> IO ()

ffiuintintint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> m ()
ffiuintintint64IOV fp v0 v1 v2 = liftIO (dynuintintint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintint64EXTIOV :: FunPtr (GLuint -> GLint -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> IO ()

ffiuintintint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> m ()
ffiuintintint64EXTIOV fp v0 v1 v2 = liftIO (dynuintintint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintsizeiPtrint64EXTIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ()

ffiuintintsizeiPtrint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> m ()
ffiuintintsizeiPtrint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtrint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintuintIOV :: FunPtr (GLuint -> GLint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> IO ()

ffiuintintuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> m ()
ffiuintintuintIOV fp v0 v1 v2 = liftIO (dynuintintuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintuint64IOV :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> IO ()

ffiuintintuint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> m ()
ffiuintintuint64IOV fp v0 v1 v2 = liftIO (dynuintintuint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintuint64EXTIOV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> IO ()

ffiuintintuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> m ()
ffiuintintuint64EXTIOV fp v0 v1 v2 = liftIO (dynuintintuint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintsizeiPtruint64EXTIOV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ()

ffiuintintsizeiPtruint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> m ()
ffiuintintsizeiPtruint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynuintintsizeiPtruint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintdoubledoubleIOV :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> GLdouble -> IO ()

ffiuintintdoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> GLdouble -> m ()
ffiuintintdoubledoubleIOV fp v0 v1 v2 v3 = liftIO (dynuintintdoubledoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintfloatfloatIOV :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> GLfloat -> IO ()

ffiuintintfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> GLfloat -> m ()
ffiuintintfloatfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintintfloatfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintintintIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> IO ()

ffiuintintintintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> m ()
ffiuintintintintIOV fp v0 v1 v2 v3 = liftIO (dynuintintintintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintint64int64IOV :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> GLint64 -> IO ()

ffiuintintint64int64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> GLint64 -> m ()
ffiuintintint64int64IOV fp v0 v1 v2 v3 = liftIO (dynuintintint64int64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintint64EXTint64EXTIOV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> GLint64EXT -> IO ()

ffiuintintint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> GLint64EXT -> m ()
ffiuintintint64EXTint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynuintintint64EXTint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintuintuintIOV :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> GLuint -> IO ()

ffiuintintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> GLuint -> m ()
ffiuintintuintuintIOV fp v0 v1 v2 v3 = liftIO (dynuintintuintuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintuint64uint64IOV :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> GLuint64 -> IO ()

ffiuintintuint64uint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> GLuint64 -> m ()
ffiuintintuint64uint64IOV fp v0 v1 v2 v3 = liftIO (dynuintintuint64uint64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintuint64EXTuint64EXTIOV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiuintintuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> m ()
ffiuintintuint64EXTuint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynuintintuint64EXTuint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintdoubledoubledoubleIOV :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiuintintdoubledoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiuintintdoubledoubledoubleIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintdoubledoubledoubleIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintfloatfloatfloatIOV :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintintfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintintfloatfloatfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintfloatfloatfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintintintintIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

ffiuintintintintintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
ffiuintintintintintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintintintintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintint64int64int64IOV :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ()

ffiuintintint64int64int64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> m ()
ffiuintintint64int64int64IOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintint64int64int64IOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintint64EXTint64EXTint64EXTIOV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()

ffiuintintint64EXTint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
ffiuintintint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintuintuintuintIOV :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintintuintuintuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintuintuintuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintuint64uint64uint64IOV :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()

ffiuintintuint64uint64uint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
ffiuintintuint64uint64uint64IOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintuint64uint64uint64IOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintuint64EXTuint64EXTuint64EXTIOV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiuintintuint64EXTuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
ffiuintintuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintdoubledoubledoubledoubleIOV :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiuintintdoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiuintintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintintfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintintintintintIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

ffiuintintintintintintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> m ()
ffiuintintintintintintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintintintintintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintint64int64int64int64IOV :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ()

ffiuintintint64int64int64int64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> m ()
ffiuintintint64int64int64int64IOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintint64int64int64int64IOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintint64EXTint64EXTint64EXTint64EXTIOV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()

ffiuintintint64EXTint64EXTint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
ffiuintintint64EXTint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintint64EXTint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintuintuintuintuintIOV :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintintuintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintuint64uint64uint64uint64IOV :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()

ffiuintintuint64uint64uint64uint64IOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
ffiuintintuint64uint64uint64uint64IOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintuint64uint64uint64uint64IOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintuint64EXTuint64EXTuint64EXTuint64EXTIOV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiuintintuint64EXTuint64EXTuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
ffiuintintuint64EXTuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintuint64EXTuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintintsizeibooleanPtrdoubleIOV :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ()) -> GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ()

ffiuintintsizeibooleanPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ()) -> GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
ffiuintintsizeibooleanPtrdoubleIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintsizeibooleanPtrdoubleIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintsizeibooleanPtrfloatIOV :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()

ffiuintintsizeibooleanPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
ffiuintintsizeibooleanPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintsizeibooleanPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynPtrfixedPtrintIObitfield :: FunPtr (Ptr GLfixed -> Ptr GLint -> IO GLbitfield) -> Ptr GLfixed -> Ptr GLint -> IO GLbitfield

ffiPtrfixedPtrintIObitfield :: MonadIO m => FunPtr (Ptr GLfixed -> Ptr GLint -> IO GLbitfield) -> Ptr GLfixed -> Ptr GLint -> m GLbitfield
ffiPtrfixedPtrintIObitfield fp v0 v1 = liftIO (dynPtrfixedPtrintIObitfield fp v0 v1)

foreign import CALLCONV "dynamic" dynenumuintenumuintIOV :: FunPtr (GLenum -> GLuint -> GLenum -> GLuint -> IO ()) -> GLenum -> GLuint -> GLenum -> GLuint -> IO ()

ffienumuintenumuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> GLuint -> IO ()) -> GLenum -> GLuint -> GLenum -> GLuint -> m ()
ffienumuintenumuintIOV fp v0 v1 v2 v3 = liftIO (dynenumuintenumuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynshortshortIOV :: FunPtr (GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> IO ()

ffishortshortIOV :: MonadIO m => FunPtr (GLshort -> GLshort -> IO ()) -> GLshort -> GLshort -> m ()
ffishortshortIOV fp v0 v1 = liftIO (dynshortshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintbooleanIOV :: FunPtr (GLuint -> GLboolean -> IO ()) -> GLuint -> GLboolean -> IO ()

ffiuintbooleanIOV :: MonadIO m => FunPtr (GLuint -> GLboolean -> IO ()) -> GLuint -> GLboolean -> m ()
ffiuintbooleanIOV fp v0 v1 = liftIO (dynuintbooleanIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintintsizeisizeienumenumPtrVIOV :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiintintsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynintintsizeisizeienumenumsizeiPtrVIOV :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiintintsizeisizeienumenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
ffiintintsizeisizeienumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynintintsizeisizeienumenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynPtrdoublePtrdoubleIOV :: FunPtr (Ptr GLdouble -> Ptr GLdouble -> IO ()) -> Ptr GLdouble -> Ptr GLdouble -> IO ()

ffiPtrdoublePtrdoubleIOV :: MonadIO m => FunPtr (Ptr GLdouble -> Ptr GLdouble -> IO ()) -> Ptr GLdouble -> Ptr GLdouble -> m ()
ffiPtrdoublePtrdoubleIOV fp v0 v1 = liftIO (dynPtrdoublePtrdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynPtrintPtrintIOV :: FunPtr (Ptr GLint -> Ptr GLint -> IO ()) -> Ptr GLint -> Ptr GLint -> IO ()

ffiPtrintPtrintIOV :: MonadIO m => FunPtr (Ptr GLint -> Ptr GLint -> IO ()) -> Ptr GLint -> Ptr GLint -> m ()
ffiPtrintPtrintIOV fp v0 v1 = liftIO (dynPtrintPtrintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynPtrshortPtrshortIOV :: FunPtr (Ptr GLshort -> Ptr GLshort -> IO ()) -> Ptr GLshort -> Ptr GLshort -> IO ()

ffiPtrshortPtrshortIOV :: MonadIO m => FunPtr (Ptr GLshort -> Ptr GLshort -> IO ()) -> Ptr GLshort -> Ptr GLshort -> m ()
ffiPtrshortPtrshortIOV fp v0 v1 = liftIO (dynPtrshortPtrshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynPtrfixedPtrfixedIOV :: FunPtr (Ptr GLfixed -> Ptr GLfixed -> IO ()) -> Ptr GLfixed -> Ptr GLfixed -> IO ()

ffiPtrfixedPtrfixedIOV :: MonadIO m => FunPtr (Ptr GLfixed -> Ptr GLfixed -> IO ()) -> Ptr GLfixed -> Ptr GLfixed -> m ()
ffiPtrfixedPtrfixedIOV fp v0 v1 = liftIO (dynPtrfixedPtrfixedIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumIOint :: FunPtr (GLenum -> IO GLint) -> GLenum -> IO GLint

ffienumIOint :: MonadIO m => FunPtr (GLenum -> IO GLint) -> GLenum -> m GLint
ffienumIOint fp v0 = liftIO (dynenumIOint fp v0)

foreign import CALLCONV "dynamic" dynenumenumsizeisizeiIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> IO ()

ffienumenumsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> m ()
ffienumenumsizeisizeiIOV fp v0 v1 v2 v3 = liftIO (dynenumenumsizeisizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumsizeienumsizeisizeiIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()

ffienumsizeienumsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
ffienumsizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumsizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumsizeisizeienumsizeisizeiIOV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()

ffienumsizeisizeienumsizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
ffienumsizeisizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumsizeisizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumsizeiPtrPtrVIOV :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr ()) -> IO ()) -> GLenum -> GLsizei -> Ptr (Ptr ()) -> IO ()

ffienumsizeiPtrPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr (Ptr ()) -> IO ()) -> GLenum -> GLsizei -> Ptr (Ptr ()) -> m ()
ffienumsizeiPtrPtrVIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtrPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynPtruintPtrfloatPtrfloatIOV :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ()

ffiPtruintPtrfloatPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> m ()
ffiPtruintPtrfloatPtrfloatIOV fp v0 v1 v2 = liftIO (dynPtruintPtrfloatPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynPtruintPtrfloatPtrfloatPtrfloatIOV :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()

ffiPtruintPtrfloatPtrfloatPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
ffiPtruintPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynPtruintPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintubyteubyteubyteubytefloatfloatfloatIOV :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintubyteubyteubyteubytefloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintubyteubyteubyteubytefloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintubyteubyteubyteubytefloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynPtruintPtrubytePtrfloatIOV :: FunPtr (Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> IO ()

ffiPtruintPtrubytePtrfloatIOV :: MonadIO m => FunPtr (Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> m ()
ffiPtruintPtrubytePtrfloatIOV fp v0 v1 v2 = liftIO (dynPtruintPtrubytePtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO (dynuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)

foreign import CALLCONV "dynamic" dynPtruintPtrfloatPtrfloatPtrfloatPtrfloatIOV :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()

ffiPtruintPtrfloatPtrfloatPtrfloatPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
ffiPtruintPtrfloatPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynPtruintPtrfloatPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintfloatfloatfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynPtruintPtrfloatIOV :: FunPtr (Ptr GLuint -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> IO ()

ffiPtruintPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLuint -> Ptr GLfloat -> IO ()) -> Ptr GLuint -> Ptr GLfloat -> m ()
ffiPtruintPtrfloatIOV fp v0 v1 = liftIO (dynPtruintPtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynfloatbooleanIOV :: FunPtr (GLfloat -> GLboolean -> IO ()) -> GLfloat -> GLboolean -> IO ()

ffifloatbooleanIOV :: MonadIO m => FunPtr (GLfloat -> GLboolean -> IO ()) -> GLfloat -> GLboolean -> m ()
ffifloatbooleanIOV fp v0 v1 = liftIO (dynfloatbooleanIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynclampxbooleanIOV :: FunPtr (GLclampx -> GLboolean -> IO ()) -> GLclampx -> GLboolean -> IO ()

fficlampxbooleanIOV :: MonadIO m => FunPtr (GLclampx -> GLboolean -> IO ()) -> GLclampx -> GLboolean -> m ()
fficlampxbooleanIOV fp v0 v1 = liftIO (dynclampxbooleanIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynclampfbooleanIOV :: FunPtr (GLclampf -> GLboolean -> IO ()) -> GLclampf -> GLboolean -> IO ()

fficlampfbooleanIOV :: MonadIO m => FunPtr (GLclampf -> GLboolean -> IO ()) -> GLclampf -> GLboolean -> m ()
fficlampfbooleanIOV fp v0 v1 = liftIO (dynclampfbooleanIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintbitfieldIOV :: FunPtr (GLuint -> GLbitfield -> IO ()) -> GLuint -> GLbitfield -> IO ()

ffiuintbitfieldIOV :: MonadIO m => FunPtr (GLuint -> GLbitfield -> IO ()) -> GLuint -> GLbitfield -> m ()
ffiuintbitfieldIOV fp v0 v1 = liftIO (dynuintbitfieldIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintintsizeisizeiIOV :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiintintsizeisizeiIOV :: MonadIO m => FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiintintsizeisizeiIOV fp v0 v1 v2 v3 = liftIO (dynintintsizeisizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtrintIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLsizei -> Ptr GLint -> IO ()

ffiuintsizeiPtrintIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLint -> IO ()) -> GLuint -> GLsizei -> Ptr GLint -> m ()
ffiuintsizeiPtrintIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintintintsizeisizeiIOV :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

ffiuintintintsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
ffiuintintintsizeisizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintintsizeisizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintPtrintIOV :: FunPtr (GLuint -> Ptr GLint -> IO ()) -> GLuint -> Ptr GLint -> IO ()

ffiuintPtrintIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLint -> IO ()) -> GLuint -> Ptr GLint -> m ()
ffiuintPtrintIOV fp v0 v1 = liftIO (dynuintPtrintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintbooleanuintintPtruintIOV :: FunPtr (GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> IO ()) -> GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> IO ()

ffiuintbooleanuintintPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> IO ()) -> GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> m ()
ffiuintbooleanuintintPtruintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintbooleanuintintPtruintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumenumsizeisizeienumenumPtrVPtrVIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> IO ()

ffienumenumsizeisizeienumenumPtrVPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> Ptr () -> m ()
ffienumenumsizeisizeienumenumPtrVPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumenumsizeisizeienumenumPtrVPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumPtrVIOV :: FunPtr (GLuint -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> Ptr () -> IO ()

ffiuintenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> Ptr () -> m ()
ffiuintenumPtrVIOV fp v0 v1 v2 = liftIO (dynuintenumPtrVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeiPtruintenumPtrVsizeiIOV :: FunPtr (GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> IO ()) -> GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> IO ()

ffisizeiPtruintenumPtrVsizeiIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> IO ()) -> GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> m ()
ffisizeiPtruintenumPtrVsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynsizeiPtruintenumPtrVsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynenumuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintIOV fp v0 v1 v2 v3 = liftIO (dynenumuintuintuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumuintuintuintuintIOV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffienumuintuintuintuintIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffienumuintuintuintuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumuintuintuintuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynhandleARBsizeiPtrPtrcharARBPtrintIOV :: FunPtr (GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> IO ()) -> GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> IO ()

ffihandleARBsizeiPtrPtrcharARBPtrintIOV :: MonadIO m => FunPtr (GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> IO ()) -> GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> m ()
ffihandleARBsizeiPtrPtrcharARBPtrintIOV fp v0 v1 v2 v3 = liftIO (dynhandleARBsizeiPtrPtrcharARBPtrintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintuintuintbitfieldIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> IO ()

ffiuintuintuintuintbitfieldIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> m ()
ffiuintuintuintuintbitfieldIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintuintuintbitfieldIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynsizeiuintIOV :: FunPtr (GLsizei -> GLuint -> IO ()) -> GLsizei -> GLuint -> IO ()

ffisizeiuintIOV :: MonadIO m => FunPtr (GLsizei -> GLuint -> IO ()) -> GLsizei -> GLuint -> m ()
ffisizeiuintIOV fp v0 v1 = liftIO (dynsizeiuintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynsizeienumPtrVuintenumuintenumPtrfloatIOV :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ()

ffisizeienumPtrVuintenumuintenumPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> m ()
ffisizeienumPtrVuintenumuintenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynsizeienumPtrVuintenumuintenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumuintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> IO ()

ffiuintenumuintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> m ()
ffiuintenumuintIOV fp v0 v1 v2 = liftIO (dynuintenumuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumenumintuintIOV :: FunPtr (GLenum -> GLenum -> GLint -> GLuint -> IO ()) -> GLenum -> GLenum -> GLint -> GLuint -> IO ()

ffienumenumintuintIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLint -> GLuint -> IO ()) -> GLenum -> GLenum -> GLint -> GLuint -> m ()
ffienumenumintuintIOV fp v0 v1 v2 v3 = liftIO (dynenumenumintuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumenumenumIOV :: FunPtr (GLenum -> GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> GLenum -> IO ()

ffienumenumenumIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLenum -> IO ()) -> GLenum -> GLenum -> GLenum -> m ()
ffienumenumenumIOV fp v0 v1 v2 = liftIO (dynenumenumenumIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsizeienumPtrVuintintuintenumPtrfloatIOV :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> IO ()

ffisizeienumPtrVuintintuintenumPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> m ()
ffisizeienumPtrVuintintuintenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynsizeienumPtrVuintintuintenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynsizeienumPtrVuintenumuintenumenumPtrfloatIOV :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffisizeienumPtrVuintenumuintenumenumPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffisizeienumPtrVuintenumuintenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynsizeienumPtrVuintenumuintenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumuintenumIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> IO ()) -> GLuint -> GLenum -> GLuint -> GLenum -> IO ()

ffiuintenumuintenumIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> IO ()) -> GLuint -> GLenum -> GLuint -> GLenum -> m ()
ffiuintenumuintenumIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynsizeienumPtrVuintintuintenumenumPtrfloatIOV :: FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()

ffisizeienumPtrVuintintuintenumenumPtrfloatIOV :: MonadIO m => FunPtr (GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ()) -> GLsizei -> GLenum -> Ptr () -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> m ()
ffisizeienumPtrVuintintuintenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynsizeienumPtrVuintintuintenumenumPtrfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintintuintenumIOV :: FunPtr (GLuint -> GLint -> GLuint -> GLenum -> IO ()) -> GLuint -> GLint -> GLuint -> GLenum -> IO ()

ffiuintintuintenumIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLuint -> GLenum -> IO ()) -> GLuint -> GLint -> GLuint -> GLenum -> m ()
ffiuintintuintenumIOV fp v0 v1 v2 v3 = liftIO (dynuintintuintenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintenumenumenumenumIOV :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()

ffiuintuintenumenumenumenumIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()) -> GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
ffiuintuintenumenumenumenumIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintuintenumenumenumenumIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumuintintptrsizeiptrIOV :: FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()

ffienumenumuintintptrsizeiptrIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> m ()
ffienumenumuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynenumenumuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynbytebyteIOV :: FunPtr (GLbyte -> GLbyte -> IO ()) -> GLbyte -> GLbyte -> IO ()

ffibytebyteIOV :: MonadIO m => FunPtr (GLbyte -> GLbyte -> IO ()) -> GLbyte -> GLbyte -> m ()
ffibytebyteIOV fp v0 v1 = liftIO (dynbytebyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynPtrfloatPtrfloatPtrfloatPtrfloatIOV :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()

ffiPtrfloatPtrfloatPtrfloatPtrfloatIOV :: MonadIO m => FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
ffiPtrfloatPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynPtrfloatPtrfloatPtrfloatPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynfloatfloatubyteubyteubyteubytefloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatubyteubyteubyteubytefloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatubyteubyteubyteubytefloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynfloatfloatubyteubyteubyteubytefloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynPtrfloatPtrubytePtrfloatIOV :: FunPtr (Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> IO ()

ffiPtrfloatPtrubytePtrfloatIOV :: MonadIO m => FunPtr (Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> IO ()) -> Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> m ()
ffiPtrfloatPtrubytePtrfloatIOV fp v0 v1 v2 = liftIO (dynPtrfloatPtrubytePtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynhalfNVhalfNVIOV :: FunPtr (GLhalfNV -> GLhalfNV -> IO ()) -> GLhalfNV -> GLhalfNV -> IO ()

ffihalfNVhalfNVIOV :: MonadIO m => FunPtr (GLhalfNV -> GLhalfNV -> IO ()) -> GLhalfNV -> GLhalfNV -> m ()
ffihalfNVhalfNVIOV fp v0 v1 = liftIO (dynhalfNVhalfNVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 = liftIO (dynfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14)

foreign import CALLCONV "dynamic" dynenumenumdoubleIOV :: FunPtr (GLenum -> GLenum -> GLdouble -> IO ()) -> GLenum -> GLenum -> GLdouble -> IO ()

ffienumenumdoubleIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLdouble -> IO ()) -> GLenum -> GLenum -> GLdouble -> m ()
ffienumenumdoubleIOV fp v0 v1 v2 = liftIO (dynenumenumdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynenumintintsizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintsizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintsizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumintintsizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumintintsizeisizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintsizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintsizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumintintsizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumsizeienumsizeisizeibooleanIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffienumsizeienumsizeisizeibooleanIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> m ()
ffienumsizeienumsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumsizeienumsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumsizeisizeiintsizeisizeibooleanIOV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffienumsizeisizeiintsizeisizeibooleanIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> m ()
ffienumsizeisizeiintsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumsizeisizeiintsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintintsizeisizeisizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintsizeisizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumintintsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumintenumsizeisizeisizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintenumsizeisizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintenumsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynenumintenumsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynenumsizeienumsizeisizeisizeibooleanIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffienumsizeienumsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffienumsizeienumsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumsizeienumsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumsizeisizeiintsizeisizeisizeibooleanIOV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffienumsizeisizeiintsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffienumsizeisizeiintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynenumsizeisizeiintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumintenumsizeisizeisizeisizeiintenumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintenumsizeisizeisizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintenumsizeisizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynenumintenumsizeisizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynenumintintintintsizeisizeisizeibooleanIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffienumintintintintsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffienumintintintintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumintintintintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumsizeienumsizeiIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()

ffienumsizeienumsizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> m ()
ffienumsizeienumsizeiIOV fp v0 v1 v2 v3 = liftIO (dynenumsizeienumsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynenumsizeienumsizeisizeisizeiIOV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()

ffienumsizeienumsizeisizeisizeiIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> m ()
ffienumsizeienumsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynenumsizeienumsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynenumenumsizeisizeisizeisizeibitfieldIOV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ()

ffienumenumsizeisizeisizeisizeibitfieldIOV :: MonadIO m => FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ()) -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> m ()
ffienumenumsizeisizeisizeisizeibitfieldIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumenumsizeisizeisizeisizeibitfieldIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintintsizeienumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintsizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynenumintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynenumintintintsizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintintsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynenumintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynenumintintintintintsizeisizeisizeisizeienumenumPtrVIOV :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffienumintintintintintsizeisizeisizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffienumintintintintintsizeisizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO (dynenumintintintintintsizeisizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)

foreign import CALLCONV "dynamic" dynuintenumuintintptrsizeiptrIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()

ffiuintenumuintintptrsizeiptrIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> m ()
ffiuintenumuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumenumuintintptrsizeiptrIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()

ffiuintenumenumuintintptrsizeiptrIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> m ()
ffiuintenumenumuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintenumenumuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumintintsizeiintenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintintsizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintintsizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumintintsizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumintintsizeisizeiintenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintintsizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintintsizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintenumintintsizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintenumsizeisizeiintsizeisizeibooleanIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintenumsizeisizeiintsizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintenumsizeisizeiintsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumsizeisizeiintsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumsizeiintsizeisizeibooleanIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintenumsizeiintsizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintenumsizeiintsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumsizeiintsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumintintsizeisizeisizeiintenumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintintsizeisizeisizeiintenumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintintsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO (dynuintenumintintsizeisizeisizeiintenumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

foreign import CALLCONV "dynamic" dynuintenumsizeisizeiintsizeisizeisizeibooleanIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintenumsizeisizeiintsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintenumsizeisizeiintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintenumsizeisizeiintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumsizeiintsizeisizeisizeibooleanIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintenumsizeiintsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintenumsizeiintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumsizeiintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintintintintintsizeisizeisizeibooleanIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintintintintintsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintintintintintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintintintintintsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumenumfloatIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLfloat -> IO ()) -> GLuint -> GLenum -> GLenum -> GLfloat -> IO ()

ffiuintenumenumfloatIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLfloat -> IO ()) -> GLuint -> GLenum -> GLenum -> GLfloat -> m ()
ffiuintenumenumfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumenumintIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLint -> IO ()

ffiuintenumenumintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLint -> IO ()) -> GLuint -> GLenum -> GLenum -> GLint -> m ()
ffiuintenumenumintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumenumintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeienumsizeiIOV :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> IO ()

ffiuintsizeienumsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> m ()
ffiuintsizeienumsizeiIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeienumsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumsizeienumsizeiIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()

ffiuintenumsizeienumsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> m ()
ffiuintenumsizeienumsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumsizeienumsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumsizeienumsizeisizeiIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()

ffiuintenumsizeienumsizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> m ()
ffiuintenumsizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintenumsizeienumsizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintsizeienumsizeisizeibooleanIOV :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintsizeienumsizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintsizeienumsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintsizeienumsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumsizeienumsizeisizeibooleanIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintenumsizeienumsizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintenumsizeienumsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumsizeienumsizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintsizeienumsizeisizeisizeiIOV :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()

ffiuintsizeienumsizeisizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> m ()
ffiuintsizeienumsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintsizeienumsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintenumsizeienumsizeisizeisizeiIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()

ffiuintenumsizeienumsizeisizeisizeiIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> m ()
ffiuintenumsizeienumsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintenumsizeienumsizeisizeisizeiIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintsizeienumsizeisizeisizeibooleanIOV :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintsizeienumsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintsizeienumsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintsizeienumsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumsizeienumsizeisizeisizeibooleanIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()

ffiuintenumsizeienumsizeisizeisizeibooleanIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> m ()
ffiuintenumsizeienumsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumsizeienumsizeisizeisizeibooleanIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintenumenumsizeisizeisizeisizeibitfieldIOV :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ()) -> GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ()

ffiuintenumenumsizeisizeisizeisizeibitfieldIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ()) -> GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> m ()
ffiuintenumenumsizeisizeisizeisizeibitfieldIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumenumsizeisizeisizeisizeibitfieldIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintintintsizeienumenumPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintintintsizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintenumintintsizeienumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintintsizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumintintsizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintintintintsizeisizeienumenumPtrVIOV :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintintintintsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 = liftIO (dynuintintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8)

foreign import CALLCONV "dynamic" dynuintenumintintintsizeisizeienumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintintintsizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO (dynuintenumintintintsizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)

foreign import CALLCONV "dynamic" dynuintenumintintintintsizeisizeisizeienumenumPtrVIOV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()

ffiuintenumintintintintsizeisizeisizeienumenumPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
ffiuintenumintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO (dynuintenumintintintintsizeisizeisizeienumenumPtrVIOV fp v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)

foreign import CALLCONV "dynamic" dynuintenumuintenumuintuintuintuintIOV :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintenumuintenumuintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintenumuintenumuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintenumuintenumuintuintuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynenumuintenumenumIOV :: FunPtr (GLenum -> GLuint -> GLenum -> GLenum -> IO ()) -> GLenum -> GLuint -> GLenum -> GLenum -> IO ()

ffienumuintenumenumIOV :: MonadIO m => FunPtr (GLenum -> GLuint -> GLenum -> GLenum -> IO ()) -> GLenum -> GLuint -> GLenum -> GLenum -> m ()
ffienumuintenumenumIOV fp v0 v1 v2 v3 = liftIO (dynenumuintenumenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynsizeiPtrintenumIOV :: FunPtr (GLsizei -> Ptr GLint -> GLenum -> IO ()) -> GLsizei -> Ptr GLint -> GLenum -> IO ()

ffisizeiPtrintenumIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLint -> GLenum -> IO ()) -> GLsizei -> Ptr GLint -> GLenum -> m ()
ffisizeiPtrintenumIOV fp v0 v1 v2 = liftIO (dynsizeiPtrintenumIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuintuintintptrsizeiptrIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ()

ffiuintuintuintintptrsizeiptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ()) -> GLuint -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> m ()
ffiuintuintuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintuintintptrsizeiptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynsizeiPtrintsizeiPtrintenumIOV :: FunPtr (GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> IO ()) -> GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> IO ()

ffisizeiPtrintsizeiPtrintenumIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> IO ()) -> GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> m ()
ffisizeiPtrintsizeiPtrintenumIOV fp v0 v1 v2 v3 v4 = liftIO (dynsizeiPtrintsizeiPtrintenumIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintsizeiPtrPtrcharenumIOV :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> IO ()) -> GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> IO ()

ffiuintsizeiPtrPtrcharenumIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> IO ()) -> GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> m ()
ffiuintsizeiPtrPtrcharenumIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrPtrcharenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintsizeiPtrintenumIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> GLenum -> IO ()) -> GLuint -> GLsizei -> Ptr GLint -> GLenum -> IO ()

ffiuintsizeiPtrintenumIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLint -> GLenum -> IO ()) -> GLuint -> GLsizei -> Ptr GLint -> GLenum -> m ()
ffiuintsizeiPtrintenumIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtrintenumIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintdoubleIOV :: FunPtr (GLint -> GLdouble -> IO ()) -> GLint -> GLdouble -> IO ()

ffiintdoubleIOV :: MonadIO m => FunPtr (GLint -> GLdouble -> IO ()) -> GLint -> GLdouble -> m ()
ffiintdoubleIOV fp v0 v1 = liftIO (dynintdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintsizeiPtrdoubleIOV :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLint -> GLsizei -> Ptr GLdouble -> IO ()

ffiintsizeiPtrdoubleIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ()) -> GLint -> GLsizei -> Ptr GLdouble -> m ()
ffiintsizeiPtrdoubleIOV fp v0 v1 v2 = liftIO (dynintsizeiPtrdoubleIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintfloatIOV :: FunPtr (GLint -> GLfloat -> IO ()) -> GLint -> GLfloat -> IO ()

ffiintfloatIOV :: MonadIO m => FunPtr (GLint -> GLfloat -> IO ()) -> GLint -> GLfloat -> m ()
ffiintfloatIOV fp v0 v1 = liftIO (dynintfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintsizeiPtrfloatIOV :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLint -> GLsizei -> Ptr GLfloat -> IO ()

ffiintsizeiPtrfloatIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ()) -> GLint -> GLsizei -> Ptr GLfloat -> m ()
ffiintsizeiPtrfloatIOV fp v0 v1 v2 = liftIO (dynintsizeiPtrfloatIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintint64IOV :: FunPtr (GLint -> GLint64 -> IO ()) -> GLint -> GLint64 -> IO ()

ffiintint64IOV :: MonadIO m => FunPtr (GLint -> GLint64 -> IO ()) -> GLint -> GLint64 -> m ()
ffiintint64IOV fp v0 v1 = liftIO (dynintint64IOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintint64EXTIOV :: FunPtr (GLint -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> IO ()

ffiintint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> m ()
ffiintint64EXTIOV fp v0 v1 = liftIO (dynintint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintsizeiPtrint64IOV :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ()) -> GLint -> GLsizei -> Ptr GLint64 -> IO ()

ffiintsizeiPtrint64IOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ()) -> GLint -> GLsizei -> Ptr GLint64 -> m ()
ffiintsizeiPtrint64IOV fp v0 v1 v2 = liftIO (dynintsizeiPtrint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintsizeiPtrint64EXTIOV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ()) -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ()

ffiintsizeiPtrint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ()) -> GLint -> GLsizei -> Ptr GLint64EXT -> m ()
ffiintsizeiPtrint64EXTIOV fp v0 v1 v2 = liftIO (dynintsizeiPtrint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintsizeiPtrintIOV :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ()) -> GLint -> GLsizei -> Ptr GLint -> IO ()

ffiintsizeiPtrintIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ()) -> GLint -> GLsizei -> Ptr GLint -> m ()
ffiintsizeiPtrintIOV fp v0 v1 v2 = liftIO (dynintsizeiPtrintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintuintIOV :: FunPtr (GLint -> GLuint -> IO ()) -> GLint -> GLuint -> IO ()

ffiintuintIOV :: MonadIO m => FunPtr (GLint -> GLuint -> IO ()) -> GLint -> GLuint -> m ()
ffiintuintIOV fp v0 v1 = liftIO (dynintuintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintuint64IOV :: FunPtr (GLint -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> IO ()

ffiintuint64IOV :: MonadIO m => FunPtr (GLint -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> m ()
ffiintuint64IOV fp v0 v1 = liftIO (dynintuint64IOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintuint64EXTIOV :: FunPtr (GLint -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> IO ()

ffiintuint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> m ()
ffiintuint64EXTIOV fp v0 v1 = liftIO (dynintuint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintsizeiPtruint64IOV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ()) -> GLint -> GLsizei -> Ptr GLuint64 -> IO ()

ffiintsizeiPtruint64IOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ()) -> GLint -> GLsizei -> Ptr GLuint64 -> m ()
ffiintsizeiPtruint64IOV fp v0 v1 v2 = liftIO (dynintsizeiPtruint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintsizeiPtruint64EXTIOV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ()) -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ()

ffiintsizeiPtruint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ()) -> GLint -> GLsizei -> Ptr GLuint64EXT -> m ()
ffiintsizeiPtruint64EXTIOV fp v0 v1 v2 = liftIO (dynintsizeiPtruint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintsizeiPtruintIOV :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ()) -> GLint -> GLsizei -> Ptr GLuint -> IO ()

ffiintsizeiPtruintIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ()) -> GLint -> GLsizei -> Ptr GLuint -> m ()
ffiintsizeiPtruintIOV fp v0 v1 v2 = liftIO (dynintsizeiPtruintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintint64int64IOV :: FunPtr (GLint -> GLint64 -> GLint64 -> IO ()) -> GLint -> GLint64 -> GLint64 -> IO ()

ffiintint64int64IOV :: MonadIO m => FunPtr (GLint -> GLint64 -> GLint64 -> IO ()) -> GLint -> GLint64 -> GLint64 -> m ()
ffiintint64int64IOV fp v0 v1 v2 = liftIO (dynintint64int64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintint64EXTint64EXTIOV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> GLint64EXT -> IO ()

ffiintint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLint64EXT -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> GLint64EXT -> m ()
ffiintint64EXTint64EXTIOV fp v0 v1 v2 = liftIO (dynintint64EXTint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintuintuintIOV :: FunPtr (GLint -> GLuint -> GLuint -> IO ()) -> GLint -> GLuint -> GLuint -> IO ()

ffiintuintuintIOV :: MonadIO m => FunPtr (GLint -> GLuint -> GLuint -> IO ()) -> GLint -> GLuint -> GLuint -> m ()
ffiintuintuintIOV fp v0 v1 v2 = liftIO (dynintuintuintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintuint64uint64IOV :: FunPtr (GLint -> GLuint64 -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> GLuint64 -> IO ()

ffiintuint64uint64IOV :: MonadIO m => FunPtr (GLint -> GLuint64 -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> GLuint64 -> m ()
ffiintuint64uint64IOV fp v0 v1 v2 = liftIO (dynintuint64uint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintuint64EXTuint64EXTIOV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiintuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> GLuint64EXT -> m ()
ffiintuint64EXTuint64EXTIOV fp v0 v1 v2 = liftIO (dynintuint64EXTuint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynintdoubledoubledoubleIOV :: FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiintdoubledoubledoubleIOV :: MonadIO m => FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiintdoubledoubledoubleIOV fp v0 v1 v2 v3 = liftIO (dynintdoubledoubledoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintfloatfloatfloatIOV :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiintfloatfloatfloatIOV :: MonadIO m => FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiintfloatfloatfloatIOV fp v0 v1 v2 v3 = liftIO (dynintfloatfloatfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintint64int64int64IOV :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ()

ffiintint64int64int64IOV :: MonadIO m => FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLint -> GLint64 -> GLint64 -> GLint64 -> m ()
ffiintint64int64int64IOV fp v0 v1 v2 v3 = liftIO (dynintint64int64int64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintint64EXTint64EXTint64EXTIOV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()

ffiintint64EXTint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
ffiintint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynintint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintuintuintuintIOV :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLint -> GLuint -> GLuint -> GLuint -> IO ()

ffiintuintuintuintIOV :: MonadIO m => FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLint -> GLuint -> GLuint -> GLuint -> m ()
ffiintuintuintuintIOV fp v0 v1 v2 v3 = liftIO (dynintuintuintuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintuint64uint64uint64IOV :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()

ffiintuint64uint64uint64IOV :: MonadIO m => FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
ffiintuint64uint64uint64IOV fp v0 v1 v2 v3 = liftIO (dynintuint64uint64uint64IOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintuint64EXTuint64EXTuint64EXTIOV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiintuint64EXTuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
ffiintuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynintuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintdoubledoubledoubledoubleIOV :: FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiintdoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 = liftIO (dynintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintfloatfloatfloatfloatIOV :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiintfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintint64int64int64int64IOV :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ()

ffiintint64int64int64int64IOV :: MonadIO m => FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ()) -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> m ()
ffiintint64int64int64int64IOV fp v0 v1 v2 v3 v4 = liftIO (dynintint64int64int64int64IOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintint64EXTint64EXTint64EXTint64EXTIOV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()

ffiintint64EXTint64EXTint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
ffiintint64EXTint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4 = liftIO (dynintint64EXTint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintuintuintuintuintIOV :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiintuintuintuintuintIOV :: MonadIO m => FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiintuintuintuintuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynintuintuintuintuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintuint64uint64uint64uint64IOV :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()

ffiintuint64uint64uint64uint64IOV :: MonadIO m => FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ()) -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> m ()
ffiintuint64uint64uint64uint64IOV fp v0 v1 v2 v3 v4 = liftIO (dynintuint64uint64uint64uint64IOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintuint64EXTuint64EXTuint64EXTuint64EXTIOV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiintuint64EXTuint64EXTuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
ffiintuint64EXTuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4 = liftIO (dynintuint64EXTuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynintsizeibooleanPtrdoubleIOV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ()) -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ()

ffiintsizeibooleanPtrdoubleIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ()) -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> m ()
ffiintsizeibooleanPtrdoubleIOV fp v0 v1 v2 v3 = liftIO (dynintsizeibooleanPtrdoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintsizeibooleanPtrfloatIOV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()

ffiintsizeibooleanPtrfloatIOV :: MonadIO m => FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
ffiintsizeibooleanPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynintsizeibooleanPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtrVenumIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr () -> GLenum -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr () -> GLenum -> IO ()

ffiuintuintsizeiPtrVenumIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr () -> GLenum -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr () -> GLenum -> m ()
ffiuintuintsizeiPtrVenumIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintsizeiPtrVenumIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintbitfielduintIOV :: FunPtr (GLuint -> GLbitfield -> GLuint -> IO ()) -> GLuint -> GLbitfield -> GLuint -> IO ()

ffiuintbitfielduintIOV :: MonadIO m => FunPtr (GLuint -> GLbitfield -> GLuint -> IO ()) -> GLuint -> GLbitfield -> GLuint -> m ()
ffiuintbitfielduintIOV fp v0 v1 v2 = liftIO (dynuintbitfielduintIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynvdpauSurfaceNVenumsizeiPtrsizeiPtrintIOV :: FunPtr (GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()) -> GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()

ffivdpauSurfaceNVenumsizeiPtrsizeiPtrintIOV :: MonadIO m => FunPtr (GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ()) -> GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> m ()
ffivdpauSurfaceNVenumsizeiPtrsizeiPtrintIOV fp v0 v1 v2 v3 v4 = liftIO (dynvdpauSurfaceNVenumsizeiPtrsizeiPtrintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynPtrVPtrVIOV :: FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> Ptr () -> IO ()

ffiPtrVPtrVIOV :: MonadIO m => FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> Ptr () -> m ()
ffiPtrVPtrVIOV fp v0 v1 = liftIO (dynPtrVPtrVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynvdpauSurfaceNVIOboolean :: FunPtr (GLvdpauSurfaceNV -> IO GLboolean) -> GLvdpauSurfaceNV -> IO GLboolean

ffivdpauSurfaceNVIOboolean :: MonadIO m => FunPtr (GLvdpauSurfaceNV -> IO GLboolean) -> GLvdpauSurfaceNV -> m GLboolean
ffivdpauSurfaceNVIOboolean fp v0 = liftIO (dynvdpauSurfaceNVIOboolean fp v0)

foreign import CALLCONV "dynamic" dynsizeiPtrvdpauSurfaceNVIOV :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ()) -> GLsizei -> Ptr GLvdpauSurfaceNV -> IO ()

ffisizeiPtrvdpauSurfaceNVIOV :: MonadIO m => FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ()) -> GLsizei -> Ptr GLvdpauSurfaceNV -> m ()
ffisizeiPtrvdpauSurfaceNVIOV fp v0 v1 = liftIO (dynsizeiPtrvdpauSurfaceNVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynPtrVenumsizeiPtruintIOvdpauSurfaceNV :: FunPtr (Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV) -> Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV

ffiPtrVenumsizeiPtruintIOvdpauSurfaceNV :: MonadIO m => FunPtr (Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV) -> Ptr () -> GLenum -> GLsizei -> Ptr GLuint -> m GLvdpauSurfaceNV
ffiPtrVenumsizeiPtruintIOvdpauSurfaceNV fp v0 v1 v2 v3 = liftIO (dynPtrVenumsizeiPtruintIOvdpauSurfaceNV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynvdpauSurfaceNVenumIOV :: FunPtr (GLvdpauSurfaceNV -> GLenum -> IO ()) -> GLvdpauSurfaceNV -> GLenum -> IO ()

ffivdpauSurfaceNVenumIOV :: MonadIO m => FunPtr (GLvdpauSurfaceNV -> GLenum -> IO ()) -> GLvdpauSurfaceNV -> GLenum -> m ()
ffivdpauSurfaceNVenumIOV fp v0 v1 = liftIO (dynvdpauSurfaceNVenumIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynvdpauSurfaceNVIOV :: FunPtr (GLvdpauSurfaceNV -> IO ()) -> GLvdpauSurfaceNV -> IO ()

ffivdpauSurfaceNVIOV :: MonadIO m => FunPtr (GLvdpauSurfaceNV -> IO ()) -> GLvdpauSurfaceNV -> m ()
ffivdpauSurfaceNVIOV fp v0 = liftIO (dynvdpauSurfaceNVIOV fp v0)

foreign import CALLCONV "dynamic" dynuintenumsizeiuintuintIOV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ()

ffiuintenumsizeiuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ()) -> GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> m ()
ffiuintenumsizeiuintuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintenumsizeiuintuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumuintPtrVIOV :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr () -> IO ()

ffiuintenumuintPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLuint -> Ptr () -> IO ()) -> GLuint -> GLenum -> GLuint -> Ptr () -> m ()
ffiuintenumuintPtrVIOV fp v0 v1 v2 v3 = liftIO (dynuintenumuintPtrVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintPtrbyteIOV :: FunPtr (GLuint -> Ptr GLbyte -> IO ()) -> GLuint -> Ptr GLbyte -> IO ()

ffiuintPtrbyteIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLbyte -> IO ()) -> GLuint -> Ptr GLbyte -> m ()
ffiuintPtrbyteIOV fp v0 v1 = liftIO (dynuintPtrbyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtrdoubleIOV :: FunPtr (GLuint -> Ptr GLdouble -> IO ()) -> GLuint -> Ptr GLdouble -> IO ()

ffiuintPtrdoubleIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLdouble -> IO ()) -> GLuint -> Ptr GLdouble -> m ()
ffiuintPtrdoubleIOV fp v0 v1 = liftIO (dynuintPtrdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtrshortIOV :: FunPtr (GLuint -> Ptr GLshort -> IO ()) -> GLuint -> Ptr GLshort -> IO ()

ffiuintPtrshortIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLshort -> IO ()) -> GLuint -> Ptr GLshort -> m ()
ffiuintPtrshortIOV fp v0 v1 = liftIO (dynuintPtrshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtrushortIOV :: FunPtr (GLuint -> Ptr GLushort -> IO ()) -> GLuint -> Ptr GLushort -> IO ()

ffiuintPtrushortIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLushort -> IO ()) -> GLuint -> Ptr GLushort -> m ()
ffiuintPtrushortIOV fp v0 v1 = liftIO (dynuintPtrushortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintuintintenumbooleanuintIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()) -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()

ffiuintuintintenumbooleanuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()) -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> m ()
ffiuintuintintenumbooleanuintIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintuintintenumbooleanuintIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintuintintenumuintIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ()

ffiuintuintintenumuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLuint -> GLint -> GLenum -> GLuint -> m ()
ffiuintuintintenumuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintintenumuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintuintintptrsizeiIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ()) -> GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ()

ffiuintuintuintintptrsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ()) -> GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> m ()
ffiuintuintuintintptrsizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintuintintptrsizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintintenumsizeiintptrIOV :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()

ffiuintuintintenumsizeiintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
ffiuintuintintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintuintintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintuintsizeiintptrIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLsizei -> GLintptr -> IO ()

ffiuintuintsizeiintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLsizei -> GLintptr -> m ()
ffiuintuintsizeiintptrIOV fp v0 v1 v2 v3 = liftIO (dynuintuintsizeiintptrIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuintenumsizeiintptrIOV :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ()

ffiuintuintenumsizeiintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> m ()
ffiuintuintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintenumsizeiintptrIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintenumintenumsizeiintptrIOV :: FunPtr (GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()

ffiuintuintenumintenumsizeiintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
ffiuintuintenumintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintenumintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintuintuintintenumsizeiintptrIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()

ffiuintuintuintintenumsizeiintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> m ()
ffiuintuintuintintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintuintuintintenumsizeiintptrIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintuintuintintenumbooleansizeiintptrIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()

ffiuintuintuintintenumbooleansizeiintptrIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()) -> GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> m ()
ffiuintuintuintintenumbooleansizeiintptrIOV fp v0 v1 v2 v3 v4 v5 v6 v7 = liftIO (dynuintuintuintintenumbooleansizeiintptrIOV fp v0 v1 v2 v3 v4 v5 v6 v7)

foreign import CALLCONV "dynamic" dynuintuintsizeiPtruintPtrintptrPtrsizeiIOV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ()

ffiuintuintsizeiPtruintPtrintptrPtrsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ()) -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> m ()
ffiuintuintsizeiPtruintPtrintptrPtrsizeiIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintuintsizeiPtruintPtrintptrPtrsizeiIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintdoubleIOV :: FunPtr (GLuint -> GLdouble -> IO ()) -> GLuint -> GLdouble -> IO ()

ffiuintdoubleIOV :: MonadIO m => FunPtr (GLuint -> GLdouble -> IO ()) -> GLuint -> GLdouble -> m ()
ffiuintdoubleIOV fp v0 v1 = liftIO (dynuintdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintfloatIOV :: FunPtr (GLuint -> GLfloat -> IO ()) -> GLuint -> GLfloat -> IO ()

ffiuintfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> IO ()) -> GLuint -> GLfloat -> m ()
ffiuintfloatIOV fp v0 v1 = liftIO (dynuintfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuinthalfNVIOV :: FunPtr (GLuint -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> IO ()

ffiuinthalfNVIOV :: MonadIO m => FunPtr (GLuint -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> m ()
ffiuinthalfNVIOV fp v0 v1 = liftIO (dynuinthalfNVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtrhalfNVIOV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ()) -> GLuint -> Ptr GLhalfNV -> IO ()

ffiuintPtrhalfNVIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLhalfNV -> IO ()) -> GLuint -> Ptr GLhalfNV -> m ()
ffiuintPtrhalfNVIOV fp v0 v1 = liftIO (dynuintPtrhalfNVIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintshortIOV :: FunPtr (GLuint -> GLshort -> IO ()) -> GLuint -> GLshort -> IO ()

ffiuintshortIOV :: MonadIO m => FunPtr (GLuint -> GLshort -> IO ()) -> GLuint -> GLshort -> m ()
ffiuintshortIOV fp v0 v1 = liftIO (dynuintshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuinthalfNVhalfNVIOV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> GLhalfNV -> IO ()

ffiuinthalfNVhalfNVIOV :: MonadIO m => FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> GLhalfNV -> m ()
ffiuinthalfNVhalfNVIOV fp v0 v1 v2 = liftIO (dynuinthalfNVhalfNVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintshortshortIOV :: FunPtr (GLuint -> GLshort -> GLshort -> IO ()) -> GLuint -> GLshort -> GLshort -> IO ()

ffiuintshortshortIOV :: MonadIO m => FunPtr (GLuint -> GLshort -> GLshort -> IO ()) -> GLuint -> GLshort -> GLshort -> m ()
ffiuintshortshortIOV fp v0 v1 v2 = liftIO (dynuintshortshortIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintdoubledoubledoubleIOV :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiuintdoubledoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiuintdoubledoubledoubleIOV fp v0 v1 v2 v3 = liftIO (dynuintdoubledoubledoubleIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuinthalfNVhalfNVhalfNVIOV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()

ffiuinthalfNVhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
ffiuinthalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 = liftIO (dynuinthalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintshortshortshortIOV :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ()) -> GLuint -> GLshort -> GLshort -> GLshort -> IO ()

ffiuintshortshortshortIOV :: MonadIO m => FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ()) -> GLuint -> GLshort -> GLshort -> GLshort -> m ()
ffiuintshortshortshortIOV fp v0 v1 v2 v3 = liftIO (dynuintshortshortshortIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintubyteubyteubyteubyteIOV :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()) -> GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()

ffiuintubyteubyteubyteubyteIOV :: MonadIO m => FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ()) -> GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> m ()
ffiuintubyteubyteubyteubyteIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintubyteubyteubyteubyteIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintdoubledoubledoubledoubleIOV :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

ffiuintdoubledoubledoubledoubleIOV :: MonadIO m => FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()) -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
ffiuintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintdoubledoubledoubledoubleIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintfloatfloatfloatfloatIOV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

ffiuintfloatfloatfloatfloatIOV :: MonadIO m => FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()) -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
ffiuintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintfloatfloatfloatfloatIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuinthalfNVhalfNVhalfNVhalfNVIOV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()

ffiuinthalfNVhalfNVhalfNVhalfNVIOV :: MonadIO m => FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ()) -> GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> m ()
ffiuinthalfNVhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuinthalfNVhalfNVhalfNVhalfNVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintshortshortshortshortIOV :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()

ffiuintshortshortshortshortIOV :: MonadIO m => FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ()) -> GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> m ()
ffiuintshortshortshortshortIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintshortshortshortshortIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintenumbooleansizeiuintuintIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> IO ()

ffiuintintenumbooleansizeiuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> m ()
ffiuintintenumbooleansizeiuintuintIOV fp v0 v1 v2 v3 v4 v5 v6 = liftIO (dynuintintenumbooleansizeiuintuintIOV fp v0 v1 v2 v3 v4 v5 v6)

foreign import CALLCONV "dynamic" dynuintintenumbooleanuintIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()

ffiuintintenumbooleanuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> m ()
ffiuintintenumbooleanuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintenumbooleanuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintenumbooleansizeiIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> IO ()

ffiuintintenumbooleansizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> m ()
ffiuintintenumbooleansizeiIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintenumbooleansizeiIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuintuintuintuintIOV :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()

ffiuintuintuintuintuintIOV :: MonadIO m => FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ()) -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
ffiuintuintuintuintuintIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuintuintuintuintIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintintenumuintIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLint -> GLenum -> GLuint -> IO ()

ffiuintintenumuintIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLuint -> IO ()) -> GLuint -> GLint -> GLenum -> GLuint -> m ()
ffiuintintenumuintIOV fp v0 v1 v2 v3 = liftIO (dynuintintenumuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintenumsizeiIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ()) -> GLuint -> GLint -> GLenum -> GLsizei -> IO ()

ffiuintintenumsizeiIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ()) -> GLuint -> GLint -> GLenum -> GLsizei -> m ()
ffiuintintenumsizeiIOV fp v0 v1 v2 v3 = liftIO (dynuintintenumsizeiIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintenumsizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()

ffiuintintenumsizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> m ()
ffiuintintenumsizeiPtrVIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintintenumsizeiPtrVIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintint64EXTIOV :: FunPtr (GLuint -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> IO ()

ffiuintint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> m ()
ffiuintint64EXTIOV fp v0 v1 = liftIO (dynuintint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtrint64EXTIOV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ()) -> GLuint -> Ptr GLint64EXT -> IO ()

ffiuintPtrint64EXTIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLint64EXT -> IO ()) -> GLuint -> Ptr GLint64EXT -> m ()
ffiuintPtrint64EXTIOV fp v0 v1 = liftIO (dynuintPtrint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintuint64EXTIOV :: FunPtr (GLuint -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> IO ()

ffiuintuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> m ()
ffiuintuint64EXTIOV fp v0 v1 = liftIO (dynuintuint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintPtruint64EXTIOV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ()) -> GLuint -> Ptr GLuint64EXT -> IO ()

ffiuintPtruint64EXTIOV :: MonadIO m => FunPtr (GLuint -> Ptr GLuint64EXT -> IO ()) -> GLuint -> Ptr GLuint64EXT -> m ()
ffiuintPtruint64EXTIOV fp v0 v1 = liftIO (dynuintPtruint64EXTIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynuintint64EXTint64EXTIOV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> GLint64EXT -> IO ()

ffiuintint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> GLint64EXT -> m ()
ffiuintint64EXTint64EXTIOV fp v0 v1 v2 = liftIO (dynuintint64EXTint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintuint64EXTuint64EXTIOV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiuintuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> GLuint64EXT -> m ()
ffiuintuint64EXTuint64EXTIOV fp v0 v1 v2 = liftIO (dynuintuint64EXTuint64EXTIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintint64EXTint64EXTint64EXTIOV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()

ffiuintint64EXTint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
ffiuintint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynuintint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintuint64EXTuint64EXTuint64EXTIOV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiuintuint64EXTuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
ffiuintuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 = liftIO (dynuintuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintint64EXTint64EXTint64EXTint64EXTIOV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()

ffiuintint64EXTint64EXTint64EXTint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ()) -> GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> m ()
ffiuintint64EXTint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintint64EXTint64EXTint64EXTint64EXTIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintuint64EXTuint64EXTuint64EXTuint64EXTIOV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()

ffiuintuint64EXTuint64EXTuint64EXTuint64EXTIOV :: MonadIO m => FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ()) -> GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> m ()
ffiuintuint64EXTuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4 = liftIO (dynuintuint64EXTuint64EXTuint64EXTuint64EXTIOV fp v0 v1 v2 v3 v4)

foreign import CALLCONV "dynamic" dynuintenumbooleanuintIOV :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ()) -> GLuint -> GLenum -> GLboolean -> GLuint -> IO ()

ffiuintenumbooleanuintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ()) -> GLuint -> GLenum -> GLboolean -> GLuint -> m ()
ffiuintenumbooleanuintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumbooleanuintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintenumbooleanPtruintIOV :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ()

ffiuintenumbooleanPtruintIOV :: MonadIO m => FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ()) -> GLuint -> GLenum -> GLboolean -> Ptr GLuint -> m ()
ffiuintenumbooleanPtruintIOV fp v0 v1 v2 v3 = liftIO (dynuintenumbooleanPtruintIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynuintintenumbooleansizeiPtrVIOV :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> IO ()

ffiuintintenumbooleansizeiPtrVIOV :: MonadIO m => FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> IO ()) -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> m ()
ffiuintintenumbooleansizeiPtrVIOV fp v0 v1 v2 v3 v4 v5 = liftIO (dynuintintenumbooleansizeiPtrVIOV fp v0 v1 v2 v3 v4 v5)

foreign import CALLCONV "dynamic" dynuintsizeiPtrhalfNVIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ()) -> GLuint -> GLsizei -> Ptr GLhalfNV -> IO ()

ffiuintsizeiPtrhalfNVIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ()) -> GLuint -> GLsizei -> Ptr GLhalfNV -> m ()
ffiuintsizeiPtrhalfNVIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrhalfNVIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtrshortIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ()) -> GLuint -> GLsizei -> Ptr GLshort -> IO ()

ffiuintsizeiPtrshortIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ()) -> GLuint -> GLsizei -> Ptr GLshort -> m ()
ffiuintsizeiPtrshortIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrshortIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtrubyteIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> IO ()

ffiuintsizeiPtrubyteIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> IO ()) -> GLuint -> GLsizei -> Ptr GLubyte -> m ()
ffiuintsizeiPtrubyteIOV fp v0 v1 v2 = liftIO (dynuintsizeiPtrubyteIOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintPtruintPtruint64EXTIOenum :: FunPtr (GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> IO GLenum) -> GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> IO GLenum

ffiuintPtruintPtruint64EXTIOenum :: MonadIO m => FunPtr (GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> IO GLenum) -> GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> m GLenum
ffiuintPtruintPtruint64EXTIOenum fp v0 v1 v2 = liftIO (dynuintPtruintPtruint64EXTIOenum fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynsyncbitfielduint64IOV :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ()) -> GLsync -> GLbitfield -> GLuint64 -> IO ()

ffisyncbitfielduint64IOV :: MonadIO m => FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ()) -> GLsync -> GLbitfield -> GLuint64 -> m ()
ffisyncbitfielduint64IOV fp v0 v1 v2 = liftIO (dynsyncbitfielduint64IOV fp v0 v1 v2)

foreign import CALLCONV "dynamic" dynuintsizeiPtruintPtrfloatIOV :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ()

ffiuintsizeiPtruintPtrfloatIOV :: MonadIO m => FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ()) -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> m ()
ffiuintsizeiPtruintPtrfloatIOV fp v0 v1 v2 v3 = liftIO (dynuintsizeiPtruintPtrfloatIOV fp v0 v1 v2 v3)

foreign import CALLCONV "dynamic" dynintPtrbyteIOV :: FunPtr (GLint -> Ptr GLbyte -> IO ()) -> GLint -> Ptr GLbyte -> IO ()

ffiintPtrbyteIOV :: MonadIO m => FunPtr (GLint -> Ptr GLbyte -> IO ()) -> GLint -> Ptr GLbyte -> m ()
ffiintPtrbyteIOV fp v0 v1 = liftIO (dynintPtrbyteIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrdoubleIOV :: FunPtr (GLint -> Ptr GLdouble -> IO ()) -> GLint -> Ptr GLdouble -> IO ()

ffiintPtrdoubleIOV :: MonadIO m => FunPtr (GLint -> Ptr GLdouble -> IO ()) -> GLint -> Ptr GLdouble -> m ()
ffiintPtrdoubleIOV fp v0 v1 = liftIO (dynintPtrdoubleIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrfloatIOV :: FunPtr (GLint -> Ptr GLfloat -> IO ()) -> GLint -> Ptr GLfloat -> IO ()

ffiintPtrfloatIOV :: MonadIO m => FunPtr (GLint -> Ptr GLfloat -> IO ()) -> GLint -> Ptr GLfloat -> m ()
ffiintPtrfloatIOV fp v0 v1 = liftIO (dynintPtrfloatIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrintIOV :: FunPtr (GLint -> Ptr GLint -> IO ()) -> GLint -> Ptr GLint -> IO ()

ffiintPtrintIOV :: MonadIO m => FunPtr (GLint -> Ptr GLint -> IO ()) -> GLint -> Ptr GLint -> m ()
ffiintPtrintIOV fp v0 v1 = liftIO (dynintPtrintIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynintPtrshortIOV :: FunPtr (GLint -> Ptr GLshort -> IO ()) -> GLint -> Ptr GLshort -> IO ()

ffiintPtrshortIOV :: MonadIO m => FunPtr (GLint -> Ptr GLshort -> IO ()) -> GLint -> Ptr GLshort -> m ()
ffiintPtrshortIOV fp v0 v1 = liftIO (dynintPtrshortIOV fp v0 v1)

foreign import CALLCONV "dynamic" dynenumsizeiPtrintIOV :: FunPtr (GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLsizei -> Ptr GLint -> IO ()

ffienumsizeiPtrintIOV :: MonadIO m => FunPtr (GLenum -> GLsizei -> Ptr GLint -> IO ()) -> GLenum -> GLsizei -> Ptr GLint -> m ()
ffienumsizeiPtrintIOV fp v0 v1 v2 = liftIO (dynenumsizeiPtrintIOV fp v0 v1 v2)