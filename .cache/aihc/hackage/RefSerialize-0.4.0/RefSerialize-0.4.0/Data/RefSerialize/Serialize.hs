{-# LANGUAGE TypeSynonymInstances
            ,FlexibleInstances
            ,UndecidableInstances
            ,OverloadedStrings
            ,NoMonomorphismRestriction
            ,BangPatterns
              #-}
module Data.RefSerialize.Serialize where
import GHC.Exts
import Unsafe.Coerce
import Data.List(isPrefixOf,insertBy,elem,sortBy)
import Data.Char(isAlpha,isAlphaNum,isSpace,isUpper)

import System.Mem.StableName
import System.IO.Unsafe
import Control.Monad (MonadPlus(..))
import Control.Applicative
import Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Search
import qualified Data.HashTable.IO as HT
import Data.Ord
import Data.Monoid

-- All this for myToStrict for compatibility with older versions of bytestring that
-- lack a "toStrict" call
import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import Data.ByteString.Lazy.Internal
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr


myToStrict :: ByteString -> S.ByteString
myToStrict Empty           = S.empty
myToStrict (Chunk c Empty) = c
myToStrict cs0 = S.unsafeCreate totalLen $ \ptr -> go cs0 ptr
  where
    totalLen = foldlChunks (\a c -> a + S.length c) 0 cs0

    go Empty                        !_       = return ()
    go (Chunk (S.PS fp off len) cs) !destptr =
      withForeignPtr fp $ \p -> do
        S.memcpy destptr (p `plusPtr` off) (fromIntegral len)
        go cs (destptr `plusPtr` len)

type MFun=  Char -- usafeCoherced to char to store simply the address of the function
type VarName = String
data ShowF= Expr ByteString | Var Int  deriving Show
type Context =  HT.BasicHashTable Int (  StableName MFun, MFun,[ShowF],Int)

data Error= Error String
data StatW= StatW (Context, [ShowF], ByteString)



data STW a= STW(StatW->  (StatW , a) )

instance Functor STW where
  fmap f (STW stwx)= STW $ \s ->
             let (s',x) = stwx s
             in (s', f x)

instance Applicative STW where
    pure x = STW (\s ->  (s, x))

    STW g <*> STW f = STW (\s ->

                       let (s', x)= g s
                           (s'',y)= f s'
                       in  (s'', x y)
                    )


-- | monadic serialization
instance  Monad STW where
    return  x = STW (\s ->  (s, x))
    STW g >>= f = STW (\s ->

                       let (s', x)= g s
                           STW fun  = f x
                       in    fun s'
                    )



-- HT to map
empty  =    HT.new -- (==) HT.hashInt

assocs = sortBy (comparing fst) . unsafePerformIO . HT.toList


insert  k v ht= unsafePerformIO $! HT.insert ht k v >> return ht



delete  k  ht= unsafePerformIO $! HT.delete ht k  >> return ht


lookup  k ht= unsafePerformIO $! HT.lookup ht k


toList  = unsafePerformIO . HT.toList


fromList = unsafePerformIO . HT.fromList -- HT.hashInt

-- | return a unique hash identifier for an object
-- the context assures that no StableName used in addrStr is garbage collected,
-- so the hashes are constant and the correspondence address - string
-- remain one to one as long as the context is not garbage collected.
-- Left is returned if it is the first time that @addHash@ is called for that variable
addrHash :: Context -> a -> IO (Either Int Int)
addrHash c x =
  case  Data.RefSerialize.Serialize.lookup  hash  c  of
           Nothing -> addc [Var hash] c >>  return (Left hash)
           Just (x,y,z,n)  -> HT.insert c hash (x,y,z,n+1) >> return (Right hash)
  where
  addc str c=  HT.insert c hash (st,unsafeCoerce x,  str,1)
  (hash,st) = hasht x

readContext :: ByteString -> ByteString -> (ByteString, ByteString)
readContext pattern str=
  let (s1,s2)= breakOn (myToStrict pattern) str
  in  (s1, B.drop (fromIntegral $ B.length pattern) s2)

--readContext pattern str= readContext1  mempty str where
--
-- readContext1 :: ByteString -> ByteString -> (ByteString, ByteString)
-- readContext1 s str| B.null str = (s, pack "")
--                   | pattern `B.isPrefixOf` str = (s, B.drop n str)
--                   | otherwise=   readContext1 (snoc s (B.head str)) (B.tail str)
--                    where n= fromIntegral $ B.length pattern


hasht x= unsafePerformIO $ do
       st <- makeStableName $! x
       return (hashStableName st,unsafeCoerce st)

-- | two variables that point to the same address will have identical varname (derived from import System.Mem.StableName)varName:: a -> String
-- . The stable names of during the serializing deserializing process are not deleted
-- . This is assured by the pointers in the context,
-- so the hash values remain and the comparison of varNames is correct.
varName x= "v"++ (show . hash) x
  where hash x= let (ht,_)= hasht x in ht




numVar :: String -> Maybe Int

numVar ('v':var)= Just $ read  var
numVar _ = Nothing



