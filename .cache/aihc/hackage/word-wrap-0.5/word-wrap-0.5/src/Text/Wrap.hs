module Text.Wrap
  ( FillStrategy(..)
  , FillScope(..)
  , WrapSettings(..)
  , defaultWrapSettings

  , wrapTextToLines
  , wrapText
  )
where

import Data.Monoid ((<>))
import Data.Char (isSpace)
import qualified Data.Text as T

-- | How should wrapped lines be filled (i.e. what kind of prefix
--   should be attached?)
data FillStrategy
  = NoFill             -- ^ Don't do any filling (default)
  | FillIndent Int     -- ^ Indent by this many spaces
  | FillPrefix T.Text  -- ^ Prepend this text
  deriving (Eq, Show, Read)

fillWidth :: FillStrategy -> Int
fillWidth NoFill         = 0
fillWidth (FillIndent n) = n
fillWidth (FillPrefix t) = T.length t

-- | To which lines should the fill strategy be applied?
data FillScope
  = FillAfterFirst     -- ^ Apply any fill prefix only to lines after
                       --   the first line (default)
  | FillAll            -- ^ Apply any fill prefix to all lines, even
                       --   if there is only one line
  deriving (Eq, Show, Read)

-- | Settings to control how wrapping is performed.
data WrapSettings =
    WrapSettings { preserveIndentation :: Bool
                 -- ^ Whether to indent new lines created by wrapping
                 -- when their original line was indented.
                 , breakLongWords :: Bool
                 -- ^ Whether to break in the middle of the first word
                 -- on a line when that word exceeds the wrapping width.
                 , fillStrategy        :: FillStrategy
                 -- ^ What kind of prefix should be applied to lines
                 --   after wrapping? (default: none)
                 , fillScope           :: FillScope
                 -- ^ To which lines should the fill strategy be applied?
                 --   (default: all but the first)
                 }
                 deriving (Eq, Show, Read)

defaultWrapSettings :: WrapSettings
defaultWrapSettings =
    WrapSettings { preserveIndentation = False
                 , breakLongWords = False
                 , fillStrategy = NoFill
                 , fillScope = FillAfterFirst
                 }

-- | Apply a function to the portion of a list of lines indicated by
--   the 'FillScope'.
withScope :: FillScope -> (a -> a) -> [a] -> [a]
withScope FillAfterFirst = onTail
withScope FillAll        = map

-- | Map a function over the tail of a list.
onTail :: (a -> a) -> [a] -> [a]
onTail _ []     = []
onTail f (a:as) = a : map f as

-- | Apply the fill specified in the 'WrapSettings' to a list of lines.
applyFill :: WrapSettings -> [T.Text] -> [T.Text]
applyFill settings =
    let scope = fillScope settings
    in case fillStrategy settings of
           NoFill       -> id
           FillIndent n -> withScope scope (T.append (T.replicate n (T.pack " ")))
           FillPrefix t -> withScope scope (T.append t)

-- | Wrap text at the specified width. Newlines and whitespace in the
-- input text are preserved. Returns the lines of text in wrapped
-- form.  New lines introduced due to wrapping will have leading
-- whitespace stripped prior to having any fill applied.  Preserved
-- indentation is always placed before any fill.
wrapTextToLines :: WrapSettings -> Int -> T.Text -> [T.Text]
wrapTextToLines settings amt s =
    concat $ fmap (wrapLine settings amt) $ T.lines s

-- | Like 'wrapTextToLines', but returns the wrapped text reconstructed
-- with newlines inserted at wrap points.
wrapText :: WrapSettings -> Int -> T.Text -> T.Text
wrapText settings amt s =
    T.intercalate (T.pack "\n") $ wrapTextToLines settings amt s

data Token = WS T.Text | NonWS T.Text
           deriving (Show)

tokenLength :: Token -> Int
tokenLength = T.length . tokenContent

tokenContent :: Token -> T.Text
tokenContent (WS t) = t
tokenContent (NonWS t) = t

-- | Tokenize text into whitespace and non-whitespace chunks.
tokenize :: T.Text -> [Token]
tokenize t | T.null t = []
tokenize t =
    let leadingWs = T.takeWhile isSpace t
        leadingNonWs = T.takeWhile (not . isSpace) t
        tok = if T.null leadingWs
              then NonWS leadingNonWs
              else WS leadingWs
    in tok : tokenize (T.drop (tokenLength tok) t)

-- | Wrap a single line of text into a list of lines that all satisfy
-- the wrapping width.
wrapLine :: WrapSettings
         -- ^ Settings.
         -> Int
         -- ^ The wrapping width.
         -> T.Text
         -- ^ A single line of text.
         -> [T.Text]
wrapLine settings limit t =
    let restFillWidth      = fillWidth (fillStrategy settings)
        firstLineFillWidth = if fillScope settings == FillAll then restFillWidth else 0

        firstLineLimit = limit - T.length indent - firstLineFillWidth
        restLimit      = limit - T.length indent - restFillWidth

        go _ []     = [T.empty]
        go _ [WS _] = [T.empty]
        go isFirstLine ts =
            let lim = if isFirstLine then firstLineLimit else restLimit
                (firstLine, maybeRest) = breakTokens settings lim ts
                firstLineText = T.stripEnd $ T.concat $ fmap tokenContent firstLine
            in case maybeRest of
                Nothing   -> [firstLineText]
                Just rest -> firstLineText : go False rest
        (indent, modifiedText) = if preserveIndentation settings
                                 then let i = T.takeWhile isSpace t
                                      in (T.take (limit - 1) i, T.drop (T.length i) t)
                                 else (T.empty, t)

        result = go True (tokenize modifiedText)
    in map (indent <>) . applyFill settings $ result

-- | Break a token sequence so that all tokens up to but not exceeding
-- a length limit are included on the left, and if any remain on the
-- right, return Just those too (or Nothing if there weren't any). If
-- this breaks a sequence at at point where the next token after the
-- break point is whitespace, that whitespace token is removed.
breakTokens :: WrapSettings -> Int -> [Token] -> ([Token], Maybe [Token])
breakTokens _ _ [] = ([], Nothing)
breakTokens settings limit ts =
    -- Take enough tokens until we reach the point where taking more
    -- would exceed the line length.
    let go _ []     = ([], [])
        -- Check to see whether the next token exceeds the limit. If so, bump
        -- it to the next line and terminate. Otherwise keep it and continue to
        -- the next token.
        go acc (tok:toks) =
            if tokenLength tok + acc <= limit
            then let (nextAllowed, nextDisallowed) = go (acc + tokenLength tok) toks
                 in (tok : nextAllowed, nextDisallowed)
            else case tok of
                     WS _ -> ([], toks)
                     NonWS _ ->
                         if acc == 0 && breakLongWords settings
                         then let (h, tl) = T.splitAt limit (tokenContent tok)
                              in ([NonWS h], NonWS tl : toks)
                         else if acc == 0 then ([tok], toks)
                         else ([], tok:toks)

        -- Allowed tokens are the ones we keep on this line. The rest go
        -- on the next line, to be wrapped again.
        (allowed, disallowed') = go 0 ts
        disallowed = maybeTrim disallowed'

        -- Trim leading whitespace on wrapped lines.
        maybeTrim [] = []
        maybeTrim (WS _:toks) = toks
        maybeTrim toks = toks

        result = if null disallowed
                 then (allowed, Nothing)
                 else (allowed, Just disallowed)
    in result
