#include <stdio.h>

char *area_name = NULL;

#define hsc_area(y) \
    area_name = ""y""; \
    puts( \
        "data T_\n" \
        "newtype T = Cons (Area.ForeignPtr T_)\n" \
        "\n" \
        "with :: T -> (Area.Ptr T_ -> IO a) -> IO a\n" \
        "with (Cons p) f = Area.withForeignPtr p f\n" \
        "\n" \
        "-- | Allocate an uninitialized object. (Not exported)\n" \
        "malloc :: IO T\n" \
        "malloc = Area.alloca $ \\p ->\n" \
        "  do Exc.checkResult_ \"Sequencer."y"\" =<< malloc_ p\n" \
        "     fmap Cons (Area.newForeignPtr free =<< Area.peek p)\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_"y"_malloc\"\n" \
        "  malloc_ :: Area.Ptr (Area.Ptr T_) -> IO C.CInt\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h &snd_seq_"y"_free\"\n" \
        "  free :: Area.FunPtr (Area.Ptr T_ -> IO ())\n" \
        "\n" \
        "-- | Copy the content of one object into another.\n" \
        "copy\n" \
        "  :: T     -- ^ Destination\n" \
        "  -> T     -- ^ Source\n" \
        "  -> IO ()\n" \
        "\n" \
        "copy to from =\n" \
        "  with to $ \\p1 ->\n" \
        "  with from $ \\p2 ->\n" \
        "    copy_ p1 p2\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_"y"_copy\"\n" \
        "  copy_ :: Area.Ptr T_ -> Area.Ptr T_ -> IO ()\n" \
        "\n" \
        "-- | Copy the content of an object to a newly created object.\n" \
        "clone :: T -> IO T\n" \
        "clone from =\n" \
        "  do to <- malloc\n" \
        "     copy to from\n" \
        "     return to\n" \
        "\n" \
        "instance Area.C T where\n" \
        "  malloc = malloc\n" \
        "  copy = copy\n" \
        "  clone = clone\n" \
        "\n" \
    );

#define hsc_get_area(x) \
    printf( \
        "get :: Seq.T mode -> "x" -> IO T\n" \
        "get h q =\n" \
        "  do status <- malloc\n" \
        "     Exc.checkResult_ \"get_%s\"\n" \
        "       =<< with status (get_ h q)\n" \
        "     return status\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_get_%s\"\n" \
        "  get_ :: Seq.T mode -> "x" -> Area.Ptr T_ -> IO C.CInt\n" \
    , area_name, area_name);

#define hsc_set_area(x) \
    printf( \
        "set :: Seq.T mode -> "x" -> T -> IO ()\n" \
        "set h q info =\n" \
        "  Exc.checkResult_ \"set_%s\" =<< with info (set_ h q)\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_set_%s\"\n" \
        "  set_ :: Seq.T mode -> "x" -> Area.Ptr T_ -> IO C.CInt\n" \
    , area_name, area_name);



#define hsc_get_set_name() \
    printf( \
        "getName :: T -> IO String\n" \
        "getName i = Area.peekCString =<< with i getName_\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_get_name\"\n" \
        "  getName_ :: Area.Ptr T_ -> IO Area.CString\n" \
        "\n" \
        "setName :: T -> String -> IO ()\n" \
        "setName i c =\n" \
        "  Area.withCAString c $ \\p -> with i (flip setName_ p)\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_set_name\"\n" \
        "  setName_ :: Area.Ptr T_ -> Area.CString -> IO ()\n" \
        "\n" \
    , area_name, area_name);

#define hsc_get_bool(z,nm) \
    printf( \
        "get"nm" :: T -> IO Bool\n" \
        "get"nm" i =\n" \
        "  fmap (0 /=) $ with i get"nm"_\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_get_"z"\"\n" \
        "  get"nm"_ :: Area.Ptr T_ -> IO C.CInt\n" \
        "\n" \
    , area_name);

#define hsc_set_bool(z,nm) \
    printf( \
        "set"nm" :: T -> Bool -> IO ()\n" \
        "set"nm" i c =\n" \
        "  let x = if c then 1 else 0\n" \
        "  in  with i (flip set"nm"_ x)\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_set_"z"\"\n" \
        "  set"nm"_ :: Area.Ptr T_ -> C.CInt -> IO ()\n" \
        "\n" \
    , area_name);

#define hsc_get_set_bool(z,nm) \
    hsc_get_bool(z,nm) \
    hsc_set_bool(z,nm)

#define hsc_get_int_gen(z,nm,t,mk,int) \
    printf( \
        "get"nm" :: T -> IO "t"\n" \
        "get"nm" i =\n" \
        "  fmap "mk" $ with i get"nm"_\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_get_"z"\"\n" \
        "  get"nm"_ :: Area.Ptr T_ -> IO "int"\n" \
        "\n" \
    , area_name);

#define hsc_set_int_gen(z,nm,t,brk,int) \
    printf( \
        "set"nm" :: T -> "t" -> IO ()\n" \
        "set"nm" i c =\n" \
        "  with i (flip set"nm"_ ("brk" c))\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_set_"z"\"\n" \
        "  set"nm"_  :: Area.Ptr T_ -> "int" -> IO ()\n" \
        "\n" \
    , area_name);

#define hsc_get_set_int_gen(z,nm,t,mk,brk,int) \
    hsc_get_int_gen(z,nm,t,mk,int) \
    hsc_set_int_gen(z,nm,t,brk,int)


#define hsc_get_int(z,nm,t,mk) \
    hsc_get_int_gen(z,nm,t,mk,"C.CInt")

#define hsc_set_int(z,nm,t,brk) \
    hsc_set_int_gen(z,nm,t,brk,"C.CInt")

#define hsc_get_set_int(z,nm,t,mk,brk) \
    hsc_get_int(z,nm,t,mk) \
    hsc_set_int(z,nm,t,brk)


#define hsc_get_ptr(z,nm,t) \
    printf( \
        "get"nm" :: T -> IO "t"\n" \
        "get"nm" i =\n" \
        "  Area.peek =<< with i get"nm"_\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_get_"z"\"\n" \
        "  get"nm"_ :: Area.Ptr T_ -> IO (Area.Ptr "t")\n" \
        "\n" \
    , area_name);

#define hsc_set_ptr(z,nm,t) \
    printf( \
        "set"nm" :: T -> "t" -> IO ()\n" \
        "set"nm" i c =\n" \
        "  with i (\\iptr -> Area.with c (set"nm"_ iptr))\n" \
        "\n" \
        "foreign import ccall unsafe \"alsa/asoundlib.h snd_seq_%s_set_"z"\"\n" \
        "  set"nm"_  :: Area.Ptr T_ -> (Area.Ptr "t") -> IO ()\n" \
        "\n" \
    , area_name);

#define hsc_get_set_ptr(z,nm,t) \
    hsc_get_ptr(z,nm,t) \
    hsc_set_ptr(z,nm,t)
