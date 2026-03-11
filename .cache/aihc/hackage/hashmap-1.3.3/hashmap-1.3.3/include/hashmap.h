#ifndef HASKELL_HASHMAP_H
#define HASKELL_HASHMAP_H

/*
 * Define INSTANCE_TYPEABLE[0-2]
 */
#if __GLASGOW_HASKELL__ >= 707
#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable tycon
#elif defined(__GLASGOW_HASKELL__)
#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable1 tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable2 tycon
#else
#define INSTANCE_TYPEABLE0(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable tycon where { typeOf _ = mkTyConApp tcname [] }
#define INSTANCE_TYPEABLE1(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable1 tycon where { typeOf1 _ = mkTyConApp tcname [] }; \
  instance Typeable a => Typeable (tycon a) where { typeOf = typeOfDefault }
#define INSTANCE_TYPEABLE2(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable2 tycon where { typeOf2 _ = mkTyConApp tcname [] }; \
  instance Typeable a => Typeable1 (tycon a) where { typeOf1 = typeOf1Default }; \
  instance (Typeable a, Typeable b) => Typeable (tycon a b) where { typeOf = typeOfDefault }
#endif

#endif
