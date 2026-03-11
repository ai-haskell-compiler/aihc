The `options` package lets library and application developers easily
work with command-line options.

This document discusses a handful of the utilities in this package.
See the API documentation for the `Options` module for more.

## Introductory example

The following example is a full program that can accept two options,
`--message` and `--quiet`:

```haskell
import Control.Applicative
import Options

data MainOptions = MainOptions
  { optMessage :: String
  , optQuiet :: Bool
  }
.
instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "message" "Hello world!"
          "A message to show the user."
    <*> simpleOption "quiet" False
         "Whether to be quiet."

main :: IO ()
main = runCommand $ \opts args ->
  if optQuiet opts
      then return ()
      else putStrLn (optMessage opts)
```

```
$ ./hello
Hello world!
$ ./hello --message='ciao mondo'
ciao mondo
$ ./hello --quiet
$
```

In addition, this library will automatically create documentation options
such as `--help` and `--help-all`:

```
$ ./hello --help
Help Options:
  -h, --help
    Show option summary.
  --help-all
    Show all help options.

Application Options:
  --message :: text
    A message to show the user.
    default: "Hello world!"
  --quiet :: bool
    Whether to be quiet.
    default: false
```

## Defining options

### optionType_bool

`optionType_bool` stores an option as a `Bool`.
The option's value must be either `"true"` or `"false"`.

Boolean options are unary, which means that their value is optional when
specified on the command line.
If a flag is present, the option is set to `True`.

```
$ ./app -q
$ ./app --quiet
```

Boolean options may still be specified explicitly by using long flags with
the `--flag=value` format.
This is the only way to set a unary flag to `"false"`.

```
$ ./app --quiet=true
$ ./app --quiet=false
```

### optionType_enum

`optionType_enum` store an option as one of a set of possible values.
The type must be a bounded enumeration, and the type's `Show` instance
will be used to implement the parser.

This is a simplistic implementation, useful for quick scripts.
Users with more complex requirements for enum parsing are encouraged
to define their own option types using `optionType`.

Example:

```haskell
data Action = Hello | Goodbye
   deriving (Bounded, Enum, Show)

data MainOptions = MainOptions { optAction :: Action }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> defineOption (optionType_enum "action") (\o -> o
          { optionLongFlags = ["action"]
          , optionDefault = Hello
          })

main = runCommand $ \opts args -> do
   putStrLn ("Running action " ++ show (optAction opts))
```

```
$ ./app
Running action Hello
$ ./app --action=Goodbye
Running action Goodbye
```

## Parsing argument lists

### parseOptions

`parseOptions` attempts to convert a list of command-line arguments into an
options value. This can be used by application developers who want finer control
over error handling, or who want to perform additional validation on the options
value.

The argument list must be in the same encoding as the result of
`System.Environment.getArgs`.

Use `parsedOptions`, `parsedArguments`, `parsedError`, and `parsedHelp` to
inspect the result of `parseOptions`.

Example:

```haskell
getOptionsOrDie :: Options a => IO a
getOptionsOrDie = do
  argv <- System.Environment.getArgs
  let parsed = parseOptions argv
  case parsedOptions parsed of
    Just opts -> return opts
    Nothing -> case parsedError parsed of
      Just err -> do
        hPutStrLn stderr (parsedHelp parsed)
        hPutStrLn stderr err
        exitFailure
      Nothing -> do
        hPutStr stdout (parsedHelp parsed)
        exitSuccess
```

### parseSubcommand

`parseSubcommand` attempts to convert a list of command-line arguments into a
subcommand action. This can be used by application developers who want finer
control over error handling, or who want subcommands that run in an unusual
monad.

The argument list must be in the same encoding as the result of
`System.Environment.getArgs`.

Use `parsedSubcommand`, `parsedError`, and `parsedHelp` to inspect the result of
`parseSubcommand`.

Example:

```haskell
runSubcommand :: Options cmdOpts => [Subcommand cmdOpts (IO a)] -> IO a
runSubcommand subcommands = do
  argv <- System.Environment.getArgs
  let parsed = parseSubcommand subcommands argv
  case parsedSubcommand parsed of
    Just cmd -> cmd
    Nothing -> case parsedError parsed of
      Just err -> do
        hPutStrLn stderr (parsedHelp parsed)
        hPutStrLn stderr err
        exitFailure
      Nothing -> do
        hPutStr stdout (parsedHelp parsed)
        exitSuccess
```

## Running main with options

### runCommand

`runCommand` retrieves `System.Environment.getArgs` and attempts to parse it
into a valid value of an `Options` type plus a list of left-over arguments.

The options and arguments are then passed to the provided computation.

If parsing fails, this computation will print an error and call `exitFailure`.

If parsing succeeds, and the user has passed a `--help` flag, and the developer
is using the default help flag definitions, then this computation will print
documentation and call `exitSuccess`.

### runSubcommand

`runSubcommand` is used to run applications that are split into subcommands. Use
`subcommand` to define available commands and their actions, then pass them to
this computation to select one and run it.

If the user specifies an invalid subcommand, this computation will print an
error and call `exitFailure`. In handling of invalid flags or `--help`,
`runSubcommand` acts like `runCommand`.

Example:

```haskell
import Control.Applicative
import Control.Monad (unless)
import Options

data MainOptions = MainOptions { optQuiet :: Bool }
instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "quiet" False "Whether to be quiet."

data HelloOpts = HelloOpts { optHello :: String }
instance Options HelloOpts where
  defineOptions = pure HelloOpts
    <*> simpleOption "hello" "Hello!" "How to say hello."

data ByeOpts = ByeOpts { optName :: String }
instance Options ByeOpts where
  defineOptions = pure ByeOpts
    <*> simpleOption "name" "" "The user's name."

hello :: MainOptions -> HelloOpts -> [String] -> IO ()
hello mainOpts opts args = unless (optQuiet mainOpts) $ do
   putStrLn (optHello opts)

bye :: MainOptions -> ByeOpts -> [String] -> IO ()
bye mainOpts opts args = unless (optQuiet mainOpts) $ do
   putStrLn ("Good bye " ++ optName opts)

main :: IO ()
main = runSubcommand
  [ subcommand "hello" hello
  , subcommand "bye" bye
  ]
```

```
$ ./app hello
Hello!
$ ./app hello --hello='Allo!'
Allo!
$ ./app bye
Good bye
$ ./app bye --name='Alice'
Good bye Alice
```
