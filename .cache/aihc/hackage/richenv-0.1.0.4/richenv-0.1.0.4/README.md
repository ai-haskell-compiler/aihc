# RichEnv

[![Tests](https://github.com/DavSanchez/richenv/actions/workflows/tests.yml/badge.svg)](https://github.com/DavSanchez/richenv/actions/workflows/tests.yml)

[![Hackage Version](https://img.shields.io/hackage/v/richenv)](https://hackage.haskell.org/package/richenv)
[![richenv on Stackage Nightly](https://stackage.org/package/richenv/badge/nightly)](https://stackage.org/nightly/package/richenv)
[![richenv on Stackage LTS](https://stackage.org/package/richenv/badge/lts)](https://stackage.org/lts/package/richenv)

[![nixpkgs unstable](https://img.shields.io/badge/nixpkgs-unstable-blue.svg?style=round-square&logo=NixOS&logoColor=white)](https://search.nixos.org/packages?size=1&show=richenv&channel=unstable)
[![nixpkgs stable](https://img.shields.io/badge/nixpkgs-stable-blue.svg?style=round-square&logo=NixOS&logoColor=white)](https://search.nixos.org/packages?size=1&show=richenv)

Rich environment variable setup for Haskell

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

This package exposes a type that captures a set of rules that modify an existing environment variable set, be it a provided list of key-value pairs (list of tuples) or the system's environment variable set.

Each rule can be either a prefix, a mapping or a value. Prefixes take environment variable names and prepend a prefix to them, replacing existing prefixes (i.e. fist removing old prefix, then adding the new one) if desired. Mappings replace the name of the environment variable with a different one, and values create the environment variable with the provided value.

## Getting started

The idea behind this library is that you can find a set of rules for setting environment variables that may or may not use the current environment as starting stage, to replace the ones in the current process or pass a custom env to [`System.Process.CreateProcess`](https://hackage.haskell.org/package/process/docs/System-Process.html#t:CreateProcess) to spawn some sub-process.

If your application uses a configuration file, for example in YAML format, you could add a new field to your config like this:

```yaml
# example.yaml
env:
  values:
    VERBOSE: "true"
  mappings:
    NEWNAME: OLDNAME
  prefixes:
    NEW_PREFIX_:
      - PREFIXED_
      - OTHER_PREFIXED_
    OTHER_NEW_PREFIX_: [OTHER_PREFIXED_]
# More configs ...
```

When parsing this new `env` field as the `RichEnv` type (it provides `FromJSON`/`ToJSON` instances), this defines a set of rules:

- `values`: these are simple environment variable definitions with a value (in textual format).
- `mappings`: these will create new environment variables from existing environment variables on an 1-1 basis. In the YAML config above, a `NEWNAME` var will be created with the contents of the `OLDNAME` var.
- `prefixes`: these will create new environment variables from existing environment variable by prefix substitution. In the example, environment variables with the prefixes `OLD_PREFIX_*` and `OTHER_OLD_PREFIX_*` will all be stripped of the prefix and created with the `NEW_PREFIX_*` instead.

Thus, after parsing, you will end up with a set of environment variables that you can:

- Apply to an externally provided list of environment variables and values and then apply the result them to the current process with functions like `setRichEnv`.
- Generate an environment variable list of type `[(Text, Text)]` with `toEnvList`.
- Generate a `[(String, String)]` (with something like `fromEnvironment . toEnvList`) to pass to `System.Process.CreateProcess`.
- etc

You can either provide a list of environment variables (normally of type `[(Text, Text)]`) to apply `RichEnv` rules or use the environment variables from the current process.

### Code example

Assuming you are using the previous YAML example (and a controlled set of environment variables for the current process, see steps 1 and 2 below), you could use `RichEnv` modify the environment variables like this:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Aeson (ParseException)
import GHC.Generics (Generic)
import RichEnv (RichEnv (..), clearEnvironment, setRichEnvFromCurrent)
import System.Environment (getEnvironment, setEnv)

newtype SomeConfig = SomeConfig {env :: RichEnv} deriving (Show, Eq, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  decodedYaml <- decodeFileEither "./example.yaml" :: IO (Either ParseException SomeConfig)
  case decodedYaml of
    Left err -> error $ show err
    Right rEnv -> do
      -- Successfully parsed. Now we can use the RichEnv
      -- 1. clear the environment of the current process
      getEnvironment >>= clearEnvironment
      -- 2. Set an example environment for the current process
      mapM_ (uncurry setEnv) [("FOO", "bar"), ("OLDNAME", "qux"), ("PREFIXED_VAR", "content"), ("OTHER_PREFIXED_VAR2", "content2")]
      -- 3. modify the current environment with the RichEnv
      setRichEnvFromCurrent (env rEnv)
      -- 4. check the environment again
      getEnvironment >>= print

-- printedOutput =
--   [ ("OTHER_NEW_PREFIX_VAR2", "content2"),
--     ("VERBOSE", "true"),
--     ("NEWNAME", "qux"),
--     ("NEW_PREFIX_VAR", "content"),
--     ("NEW_PREFIX_VAR2", "content2")
--   ]
  -- ...
```

As mentioned before, instead of modifying the current process, you use `RichEnv` to spawn processes with custom environments (for example with System.Process' [`proc`](https://hackage.haskell.org/package/process/docs/System-Process.html#v:proc) and [`CreateProcess`](https://hackage.haskell.org/package/process/docs/System-Process.html#t:CreateProcess)' `env` field) defined with your rules, effectively controlling how the environment is forwarded from the current process to the spawned ones.

See the Hackage documentation and [the tests](./test/RichEnvSpec.hs) for more details and examples.
