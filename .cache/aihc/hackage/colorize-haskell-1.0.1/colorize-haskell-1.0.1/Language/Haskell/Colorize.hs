module Language.Haskell.Colorize where

import qualified Language.Haskell.Lexer as L
import System.Console.ANSI

-- | The different types of that we recognize.
data Token
  = Comment     -- ^ Comment
  | Reserved    -- ^ Reserved word
  | ReservedOp  -- ^ Reserved operator
  | Var         -- ^ Variables
  | VarOp       -- ^ Variable operatros
  | Con         -- ^ Constructors
  | ConOp       -- ^ Constructor operators
  | Special     -- ^ Special syntax  (e.g., parens,brackets)
  | IntLit      -- ^ Integer lieterals
  | FloatLit    -- ^ Floating point literals
  | CharLit     -- ^ Character literals
  | StringLit   -- ^ String literals

-- | The type of functions that specify how to render a value.
type Style = Token -> String -> ShowS

render :: Style -> String -> ShowS
render how prog k = foldr step k (L.lexerPass0 prog)
  where
  step (y,(_,x)) =
    case y of
      L.Varid              -> how Var x
      L.Conid              -> how Con x
      L.Varsym             -> how VarOp x
      L.Consym             -> how ConOp x
      L.Reservedid
          | x == "_"       -> how Var x
          | otherwise      -> how Reserved x
      L.Reservedop         -> how ReservedOp x
      L.Special            -> how Special x
      L.IntLit             -> how IntLit x
      L.FloatLit           -> how FloatLit x
      L.CharLit            -> how CharLit x
      L.StringLit          -> how StringLit x
      L.Qvarid             -> how Var x
      L.Qconid             -> how Con x
      L.Qvarsym            -> how VarOp x
      L.Qconsym            -> how ConOp x
      L.NestedCommentStart -> how Comment x
      L.NestedComment      -> how Comment x
      L.LiterateComment    -> how Comment x
      L.Commentstart       -> how Comment x
      L.Comment            -> how Comment x

      _                    -> (x ++)

-- | Annotates tokens with ANSI escape sequences, suitable for a dark termianl
ansiDark :: Style
ansiDark t = case t of
  Comment        -> bright Cyan
  Reserved       -> bright Green
  ReservedOp     -> bright Yellow
  VarOp          -> bright Yellow
  ConOp          -> bright Yellow
  IntLit         -> bright Magenta
  FloatLit       -> bright Magenta
  CharLit        -> bright Magenta
  StringLit      -> bright Magenta
  _              -> (++)

  where bright x xs k = setSGRCode [ SetConsoleIntensity BoldIntensity
                                   , SetColor Foreground Vivid x
                                   ]
                ++ xs ++ setSGRCode [Reset] ++ k


-- | Annotates tokens with ANSI escape sequences, suitable for a dark termianl
ansiLight :: Style
ansiLight t = case t of
  Comment        -> dark Blue
  Reserved       -> dark Green
  ReservedOp     -> dark Red
  VarOp          -> dark Red
  ConOp          -> dark Red
  IntLit         -> dark Magenta
  FloatLit       -> dark Magenta
  CharLit        -> dark Magenta
  StringLit      -> dark Magenta
  _              -> (++)

  where dark x xs k = setSGRCode [ SetConsoleIntensity FaintIntensity
                                 , SetColor Foreground Dull x
                                 ]
                ++ xs ++ setSGRCode [Reset] ++ k



{-

-- | Annotates tokens with HTML tags.
html :: Style
html = Style
  { comment        = tag "comment"
  , reserved       = tag "reseved"
  , reservedOp     = tag "resevedOp"
  , var            = tag "var"
  , varOp          = tag "varOp"
  , con            = tag "con"
  , conOp          = tag "conOp"
  , intLit         = tag "intLit"
  , floatLit       = tag "floatLit"
  , charLit        = tag "charLit"
  , stringLit      = tag "stringLit"
  , special        = tag "special"

  , prefix         = showString "<html><head>"
                   . showString css
                   . showString "</head><body><pre>"
  , postfix        = showString "</pre></body></html>"
  }
  where tag x cs  = "<span class='" ++ x ++ "'>"
                                ++ concatMap esc cs ++ "</span>"
        esc c     = case c of
                      '<' -> "&lt;"
                      '>' -> "&gt;"
                      '&' -> "&amp;"
                      _   -> [c]
        css       = unlines
                  [ "<style type='text/css' rel='stylesheet'>"
                  , ".comment { color: blue }"
                  , "</style>"
                  ]


--------------------------------------------------------------------------------


getOptions :: IO Options
getOptions =
  do (fs,non_opt,errs) <- getOpt Permute flags `fmap` getArgs
     case (non_opt,errs) of
       ([],[]) -> return (foldr ($) defaultOptions fs)
       _       -> mapM_ (hPutStrLn stderr) errs >> showUsage

-- | Print usage info and quit
showUsage :: IO a
showUsage = do hPutStrLn stderr (usageInfo "Available options:" flags)
               exitFailure

flags :: [ OptDescr (Options -> Options) ]
flags = [ Option []       ["html"]  (NoArg $ \o -> o { optStyle = HTML })
          "Generate HTML output"
        , Option []       ["ansi"]  (NoArg $ \o -> o { optStyle = ANSI })
          "Generate ANSI output (default)"
        , Option ['h']    ["help"]  (NoArg $ \o -> o { optHelp = True })
          "Display this help."
        ]

data OptStyle = ANSI | HTML

data Options = Options
  { optStyle :: OptStyle
  , optHelp  :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optStyle = ANSI
  , optHelp  = False
  }
-}

