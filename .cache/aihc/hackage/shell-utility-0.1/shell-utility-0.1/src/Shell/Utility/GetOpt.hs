module Shell.Utility.GetOpt where

import qualified System.Console.GetOpt as G


fmapArgDescr :: (a -> b) -> (G.ArgDescr a -> G.ArgDescr b)
fmapArgDescr f d =
    case d of
        G.NoArg a -> G.NoArg $ f a
        G.ReqArg g str -> G.ReqArg (f.g) str
        G.OptArg g str -> G.OptArg (f.g) str

fmapOptDescr :: (a -> b) -> (G.OptDescr a -> G.OptDescr b)
fmapOptDescr f (G.Option short long arg help) =
    G.Option short long (fmapArgDescr f arg) help
