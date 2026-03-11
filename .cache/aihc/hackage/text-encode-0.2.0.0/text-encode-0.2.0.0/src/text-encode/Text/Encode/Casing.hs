module Text.Encode.Casing (
    Casing (..),
    CaseConversion (..),
) where

import Text.Casing qualified as C

data Casing
    = Camel
    | Kebab
    | Pascal
    | QuietSnake
    | ScreamingSnake
    | Snake

class CaseConversion (decoding :: Casing) (encoding :: Casing) where
    encoding :: String -> String
    decoding :: String -> String

instance CaseConversion 'Camel 'Camel where
    encoding = id
    decoding = id

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Camel 'Kebab where
    encoding = C.toKebab . C.fromHumps
    decoding = C.toCamel . C.fromKebab

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Camel 'Pascal where
    encoding = C.toPascal . C.fromHumps
    decoding = C.toCamel . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Camel 'QuietSnake where
    encoding = C.toQuietSnake . C.fromHumps
    decoding = C.toCamel . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Camel 'ScreamingSnake where
    encoding = C.toScreamingSnake . C.fromHumps
    decoding = C.toCamel . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Camel 'Snake where
    encoding = C.toSnake . C.fromHumps
    decoding = C.toCamel . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Kebab 'Camel where
    encoding = C.toCamel . C.fromKebab
    decoding = C.toKebab . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Kebab 'Kebab where
    encoding = id
    decoding = id

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Kebab 'Pascal where
    encoding = C.toPascal . C.fromKebab
    decoding = C.toKebab . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Kebab 'QuietSnake where
    encoding = C.toQuietSnake . C.fromKebab
    decoding = C.toKebab . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Kebab 'ScreamingSnake where
    encoding = C.toScreamingSnake . C.fromKebab
    decoding = C.toKebab . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Kebab 'Snake where
    encoding = C.toSnake . C.fromKebab
    decoding = C.toKebab . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Pascal 'Camel where
    encoding = C.toCamel . C.fromHumps
    decoding = C.toPascal . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Pascal 'Kebab where
    encoding = C.toKebab . C.fromHumps
    decoding = C.toPascal . C.fromKebab

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Pascal 'Pascal where
    encoding = id
    decoding = id

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Pascal 'QuietSnake where
    encoding = C.toQuietSnake . C.fromHumps
    decoding = C.toPascal . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Pascal 'ScreamingSnake where
    encoding = C.toScreamingSnake . C.fromHumps
    decoding = C.toPascal . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Pascal 'Snake where
    encoding = C.toSnake . C.fromHumps
    decoding = C.toPascal . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'QuietSnake 'Camel where
    encoding = C.toCamel . C.fromSnake
    decoding = C.toQuietSnake . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'QuietSnake 'Kebab where
    encoding = C.toKebab . C.fromSnake
    decoding = C.toQuietSnake . C.fromKebab

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'QuietSnake 'Pascal where
    encoding = C.toPascal . C.fromSnake
    decoding = C.toQuietSnake . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'QuietSnake 'QuietSnake where
    encoding = id
    decoding = id

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'QuietSnake 'ScreamingSnake where
    encoding = C.toScreamingSnake . C.fromSnake
    decoding = C.toQuietSnake . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'QuietSnake 'Snake where
    encoding = C.toSnake . C.fromSnake
    decoding = C.toQuietSnake . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'ScreamingSnake 'Camel where
    encoding = C.toCamel . C.fromSnake
    decoding = C.toScreamingSnake . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'ScreamingSnake 'Kebab where
    encoding = C.toKebab . C.fromSnake
    decoding = C.toScreamingSnake . C.fromKebab

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'ScreamingSnake 'Pascal where
    encoding = C.toPascal . C.fromSnake
    decoding = C.toScreamingSnake . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'ScreamingSnake 'QuietSnake where
    encoding = C.toQuietSnake . C.fromSnake
    decoding = C.toScreamingSnake . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'ScreamingSnake 'ScreamingSnake where
    encoding = id
    decoding = id

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'ScreamingSnake 'Snake where
    encoding = C.toSnake . C.fromSnake
    decoding = C.toScreamingSnake . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Snake 'Camel where
    encoding = C.toCamel . C.fromSnake
    decoding = C.toSnake . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Snake 'Kebab where
    encoding = C.toKebab . C.fromSnake
    decoding = C.toSnake . C.fromKebab

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Snake 'Pascal where
    encoding = C.toPascal . C.fromSnake
    decoding = C.toSnake . C.fromHumps

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Snake 'QuietSnake where
    encoding = C.toQuietSnake . C.fromSnake
    decoding = C.toSnake . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Snake 'ScreamingSnake where
    encoding = C.toScreamingSnake . C.fromSnake
    decoding = C.toSnake . C.fromSnake

    {-# INLINE encoding #-}
    {-# INLINE decoding #-}

instance CaseConversion 'Snake 'Snake where
    encoding = id
    decoding = id
    {-# INLINE decoding #-}
    {-# INLINE encoding #-}
