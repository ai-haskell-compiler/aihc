-- |
-- Module: Options.Tokenize
-- License: MIT
module Options.Tokenize
  ( Token (..),
    tokenFlagName,
    Tokens (..),
    tokensMap,
    tokenize,
  )
where

import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Map
import Options.Types
import Options.Util

data Token
  = TokenUnary String -- flag name
  | Token String String -- flag name, flag value
  deriving (Eq, Show)

tokenFlagName :: Token -> String
tokenFlagName (TokenUnary s) = s
tokenFlagName (Token s _) = s

data Tokens = Tokens
  { tokensList :: [([OptionKey], Token)],
    tokensArgv :: [String]
  }
  deriving (Show)

tokensMap :: Tokens -> Data.Map.Map OptionKey [Token]
tokensMap tokens = Data.Map.fromListWith (\xs ys -> ys ++ xs) do
  (keys, token) <- tokensList tokens
  key <- keys
  return (key, [token])

data TokState = TokState
  { stArgv :: [String],
    stArgs :: [String],
    stOpts :: [([OptionKey], Token)],
    stShortKeys :: Data.Map.Map Char ([OptionKey], OptionInfo),
    stLongKeys :: Data.Map.Map String ([OptionKey], OptionInfo),
    stSubcommands :: [(String, [OptionInfo])],
    stSubCmd :: Maybe String
  }

newtype Tok a = Tok {unTok :: ExceptT String (StateT TokState Identity) a}

instance Functor Tok where
  fmap = liftM

instance Applicative Tok where
  pure = Tok . pure
  (<*>) = ap

instance Monad Tok where
  m >>= f = Tok (unTok m >>= unTok . f)

instance MonadState Tok where
  type StateType Tok = TokState
  get = Tok get
  put = Tok . put

tokenize :: OptionDefinitions -> [String] -> (Maybe String, Either String Tokens)
tokenize (OptionDefinitions options subcommands) argv = runIdentity do
  let st =
        TokState
          { stArgv = argv,
            stArgs = [],
            stOpts = [],
            stShortKeys = toShortKeys options,
            stLongKeys = toLongKeys options,
            stSubcommands = subcommands,
            stSubCmd = Nothing
          }
  (err, st') <- runStateT (runExceptT (unTok loop)) st
  return
    ( stSubCmd st',
      case err of
        Left err' -> Left err'
        Right _ -> Right (Tokens (reverse (stOpts st')) (stArgs st'))
    )

loop :: Tok ()
loop = do
  ms <- nextItem
  st <- get
  case ms of
    Nothing -> return ()
    Just s -> (>> loop) case stringToGhc704 s of
      '-' : '-' : [] -> put (st {stArgv = [], stArgs = stArgs st ++ stArgv st})
      '-' : '-' : opt -> parseLong opt
      '-' : optChar : optValue -> parseShort optChar optValue
      '-' : [] -> addArg s
      decoded -> case (stSubcommands st, stSubCmd st) of
        ([], _) -> addArg s
        (_, Just _) -> addArg s
        (_, Nothing) -> case lookup decoded (stSubcommands st) of
          Nothing -> throwError ("Unknown subcommand " ++ show decoded ++ ".")
          Just subOptions -> mergeSubcommand decoded subOptions

nextItem :: Tok (Maybe String)
nextItem = do
  st <- get
  case stArgv st of
    [] -> return Nothing
    (x : xs) -> do
      put (st {stArgv = xs})
      return (Just x)

addArg :: String -> Tok ()
addArg s = modify (\st -> st {stArgs = stArgs st ++ [s]})

addOpt :: [OptionKey] -> Token -> Tok ()
addOpt keys val =
  modify
    ( \st ->
        st
          { stOpts = (keys, val) : stOpts st
          }
    )

mergeSubcommand :: String -> [OptionInfo] -> Tok ()
mergeSubcommand name opts = modify \st ->
  st
    { stSubCmd = Just name,
      stShortKeys = Data.Map.unionWith unionKeys (stShortKeys st) (toShortKeys opts),
      stLongKeys = Data.Map.unionWith unionKeys (stLongKeys st) (toLongKeys opts)
    }

-- note: unionKeys assumes that the OptionInfo is equivalent in both maps.
unionKeys :: ([OptionKey], OptionInfo) -> ([OptionKey], OptionInfo) -> ([OptionKey], OptionInfo)
unionKeys (keys1, info) (keys2, _) = (keys1 ++ keys2, info)

parseLong :: String -> Tok ()
parseLong optName = do
  longKeys <- gets stLongKeys
  case break (== '=') optName of
    (before, after) -> case after of
      '=' : value -> case Data.Map.lookup before longKeys of
        Nothing -> throwError ("Unknown flag --" ++ before)
        Just (keys, info) ->
          if optionInfoUnaryOnly info
            then throwError ("Flag --" ++ before ++ " takes no parameters.")
            else addOpt keys (Token ("--" ++ before) value)
      _ -> case Data.Map.lookup optName longKeys of
        Nothing -> throwError ("Unknown flag --" ++ optName)
        Just (keys, info) ->
          if optionInfoUnary info
            then addOpt keys (TokenUnary ("--" ++ optName))
            else do
              next <- nextItem
              case next of
                Nothing -> throwError ("The flag --" ++ optName ++ " requires a parameter.")
                Just value -> addOpt keys (Token ("--" ++ optName) value)

parseShort :: Char -> String -> Tok ()
parseShort optChar optValue = do
  let optName = '-' : [optChar]
  shortKeys <- gets stShortKeys
  case Data.Map.lookup optChar shortKeys of
    Nothing -> throwError ("Unknown flag " ++ optName)
    Just (keys, info) ->
      if optionInfoUnary info
        then -- don't check optionInfoUnaryOnly, because that's only set by --help
        -- options and they define no short flags.
        do
          addOpt keys (TokenUnary optName)
          case optValue of
            [] -> return ()
            nextChar : nextValue -> parseShort nextChar nextValue
        else case optValue of
          "" -> do
            next <- nextItem
            case next of
              Nothing -> throwError ("The flag " ++ optName ++ " requires a parameter.")
              Just value -> addOpt keys (Token optName value)
          _ -> addOpt keys (Token optName optValue)

toShortKeys :: [OptionInfo] -> Data.Map.Map Char ([OptionKey], OptionInfo)
toShortKeys opts = Data.Map.fromListWith (\(keys1, info) (keys2, _) -> (keys2 ++ keys1, info)) do
  opt <- opts
  flag <- optionInfoShortFlags opt
  return (flag, ([optionInfoKey opt], opt))

toLongKeys :: [OptionInfo] -> Data.Map.Map String ([OptionKey], OptionInfo)
toLongKeys opts = Data.Map.fromListWith (\(keys1, info) (keys2, _) -> (keys2 ++ keys1, info)) do
  opt <- opts
  flag <- optionInfoLongFlags opt
  return (flag, ([optionInfoKey opt], opt))

throwError :: String -> Tok a
throwError = Tok . Control.Monad.Except.throwError
