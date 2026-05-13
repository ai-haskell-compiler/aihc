{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Lex.Layout
  ( applyLayoutTokens,
    layoutTransition,
    closeImplicitLayoutContext,
  )
where

import Aihc.Parser.Lex.Types
import Aihc.Parser.Syntax (Extension, SourceSpan (..))
import Data.Maybe (fromMaybe)

ordinaryLayout :: ImplicitLayoutSpec
ordinaryLayout =
  ImplicitLayoutSpec
    { implicitLayoutSemicolons = LayoutEmitSemicolons,
      implicitLayoutIndentPolicy = LayoutAllowNondecreasingIndent,
      implicitLayoutBaseline = LayoutCurrentIndent,
      implicitLayoutChildBaseline = LayoutCurrentIndent,
      implicitLayoutThenElseDepth = Nothing,
      implicitLayoutFlushEmptyAtEOF = False
    }

strictLayout :: ImplicitLayoutSpec
strictLayout = ordinaryLayout {implicitLayoutIndentPolicy = LayoutStrictIndent}

caseAlternativeLayout :: LayoutState -> ImplicitLayoutSpec
caseAlternativeLayout st =
  strictLayout
    { implicitLayoutBaseline = currentChildBaseline (layoutContexts st),
      implicitLayoutFlushEmptyAtEOF = True
    }

multiWayIfLayout :: ImplicitLayoutSpec
multiWayIfLayout =
  strictLayout
    { implicitLayoutSemicolons = LayoutSuppressSemicolons
    }

thenElseDoLayout :: ImplicitLayoutSpec
thenElseDoLayout =
  ordinaryLayout
    { implicitLayoutThenElseDepth = Just 0
    }

thDeclQuoteLayout :: ImplicitLayoutSpec
thDeclQuoteLayout =
  strictLayout
    { implicitLayoutChildBaseline = LayoutColumnZero
    }

applyLayoutTokens :: Bool -> [Extension] -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout exts =
  go (mkInitialLayoutState enableModuleLayout exts)
  where
    go st toks =
      case toks of
        [] ->
          let eofAnchor = NoSourceSpan
              (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
              (pendingInserted, stAfterPending) = flushPendingCaseLayoutAtEOF stAfterModule eofAnchor
           in moduleInserted <> pendingInserted <> closeAllImplicit (layoutContexts stAfterPending) eofAnchor
        tok : rest ->
          let (emitted, stNext) = layoutTransition st tok
           in emitted <> go stNext rest

finalizeModuleLayoutAtEOF :: LayoutState -> SourceSpan -> ([LexToken], LayoutState)
finalizeModuleLayoutAtEOF st anchor =
  case layoutModuleMode st of
    mode
      | mode == ModuleLayoutSeekStart || mode == ModuleLayoutAwaitBody ->
          ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
            st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Nothing}
          )
    _ -> ([], st)

{-# INLINE noteModuleLayoutBeforeToken #-}
noteModuleLayoutBeforeToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutBeforeToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitBody -> st {layoutModuleMode = ModuleLayoutDone}
    ModuleLayoutSeekStart ->
      case lexTokenKind tok of
        TkPragma _ -> st
        TkLineComment -> st
        TkBlockComment -> st
        TkKeywordModule -> st {layoutModuleMode = ModuleLayoutAwaitWhere}
        _ -> st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    _ -> st

{-# INLINE noteModuleLayoutAfterToken #-}
noteModuleLayoutAfterToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutAfterToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitWhere
      | lexTokenKind tok == TkKeywordWhere ->
          st {layoutModuleMode = ModuleLayoutAwaitBody, layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    _ -> st

{-# INLINE openPendingLayout #-}
openPendingLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openPendingLayout st tok =
  case layoutPendingLayout st of
    Nothing -> ([], st, False)
    Just pending ->
      case pending of
        PendingMaybeMultiWayIf ->
          case lexTokenKind tok of
            TkReservedPipe -> openImplicitLayout multiWayIfLayout st tok
            _ -> ([], noteClassicIfInAfterThenElse (st {layoutPendingLayout = Nothing}), False)
        PendingMaybeLambdaCases ->
          case lexTokenKind tok of
            TkSpecialLBrace -> ([], st {layoutPendingLayout = Nothing}, False)
            tkKind
              | closesDelimiter tkKind ->
                  let anchor = lexTokenSpan tok
                      openTok = virtualSymbolToken "{" anchor
                      closeTok = virtualSymbolToken "}" anchor
                   in ([openTok, closeTok], st {layoutPendingLayout = Nothing}, False)
            _ -> openImplicitLayout (caseAlternativeLayout st) st tok
        PendingImplicitLayout spec ->
          case lexTokenKind tok of
            TkSpecialLBrace -> ([], st {layoutPendingLayout = Nothing}, False)
            tkKind
              | closesDelimiter tkKind ->
                  -- Closing delimiter immediately after layout-opener (e.g. [d| |])
                  -- produces empty implicit layout {}, then the closer proceeds normally.
                  let anchor = lexTokenSpan tok
                      openTok = virtualSymbolToken "{" anchor
                      closeTok = virtualSymbolToken "}" anchor
                   in ([openTok, closeTok], st {layoutPendingLayout = Nothing}, False)
            _ -> openImplicitLayout spec st tok

openImplicitLayout :: ImplicitLayoutSpec -> LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openImplicitLayout spec st tok =
  let col = tokenStartCol tok
      parentIndent =
        case implicitLayoutBaseline spec of
          LayoutCurrentIndent -> currentLayoutIndent (layoutContexts st)
          LayoutColumnZero -> 0
      openTok = virtualSymbolToken "{" (lexTokenSpan tok)
      closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
      newContext =
        LayoutImplicit
          LayoutFrame
            { layoutFrameIndent = col,
              layoutFrameSemicolons = implicitLayoutSemicolons spec,
              layoutFrameChildBaseline = implicitLayoutChildBaseline spec,
              layoutFrameThenElseDepth = implicitLayoutThenElseDepth spec
            }
      opensEmpty =
        case implicitLayoutIndentPolicy spec of
          LayoutAllowNondecreasingIndent
            | layoutNondecreasingIndent st -> col < parentIndent
          _ -> col <= parentIndent
   in if opensEmpty
        then ([openTok, closeTok], st {layoutPendingLayout = Nothing}, False)
        else
          ( [openTok],
            st
              { layoutPendingLayout = Nothing,
                layoutContexts = newContext : layoutContexts st
              },
            True
          )

{-# INLINE closeBeforeToken #-}
closeBeforeToken :: LayoutState -> LexToken -> ([LexToken], LayoutState)
closeBeforeToken st tok =
  case lexTokenKind tok of
    kind
      | closesImplicitBeforeDelimiter kind ->
          let (pendingInserted, st0) = flushPendingImplicitLayout st anchor
              (inserted, ctxs') = closeImplicitLayouts anchor (\_ _ -> True) (layoutContexts st0)
           in (pendingInserted <> inserted, st0 {layoutContexts = ctxs'})
      | closesImplicitBeforeLayoutKeyword kind ->
          closeBeforeLayoutKeyword
    _ -> ([], st)
  where
    anchor = lexTokenSpan tok

    closeBeforeLayoutKeyword =
      let col = tokenStartCol tok
          (pendingInserted, st0) = flushPendingImplicitLayout st anchor
          (inserted, ctxs') = closeImplicitLayouts anchor (shouldClose col) (layoutContexts st0)
       in (pendingInserted <> inserted, st0 {layoutContexts = ctxs'})

    shouldClose col indent kind =
      col < indent || (col == indent && closesSameColumnLayout kind)

    closesSameColumnLayout kind =
      case (lexTokenKind tok, kind) of
        (TkKeywordThen, LayoutFrame {layoutFrameThenElseDepth = Just nestedIfs}) -> nestedIfs == 0
        (TkKeywordElse, LayoutFrame {layoutFrameThenElseDepth = Just nestedIfs}) -> nestedIfs == 0
        (TkKeywordThen, _) -> False
        (TkKeywordElse, _) -> False
        _ -> True

flushPendingImplicitLayout :: LayoutState -> SourceSpan -> ([LexToken], LayoutState)
flushPendingImplicitLayout st anchor =
  case layoutPendingLayout st of
    Just (PendingImplicitLayout _) ->
      ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
        st {layoutPendingLayout = Nothing}
      )
    _ -> ([], st)

flushPendingCaseLayoutAtEOF :: LayoutState -> SourceSpan -> ([LexToken], LayoutState)
flushPendingCaseLayoutAtEOF st anchor =
  case layoutPendingLayout st of
    Just (PendingImplicitLayout spec)
      | implicitLayoutFlushEmptyAtEOF spec ->
          ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
            st {layoutPendingLayout = Nothing}
          )
    Just PendingMaybeLambdaCases ->
      ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
        st {layoutPendingLayout = Nothing}
      )
    _ -> ([], st)

{-# INLINE bolLayout #-}
bolLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState)
bolLayout st tok
  | not (isBOL st tok) = ([], st)
  | otherwise =
      let col = tokenStartCol tok
          (inserted, contexts') = closeImplicitLayouts (lexTokenSpan tok) (\indent _ -> col < indent) (layoutContexts st)
          semiAnchor = fromMaybe (lexTokenSpan tok) (layoutPrevTokenEndSpan st)
          eqSemi =
            case currentLayoutIndentMaybe contexts' of
              Just indent
                | col == indent,
                  currentLayoutAllowsSemicolon contexts',
                  lexTokenKind tok /= TkKeywordWhere ->
                    [virtualSymbolToken ";" semiAnchor]
              _ -> []
       in (inserted <> eqSemi, st {layoutContexts = contexts'})

currentLayoutAllowsSemicolon :: [LayoutContext] -> Bool
currentLayoutAllowsSemicolon contexts =
  case contexts of
    LayoutImplicit LayoutFrame {layoutFrameSemicolons = LayoutSuppressSemicolons} : _ -> False
    LayoutImplicit _ : _ -> True
    _ -> False

closeImplicitLayouts :: SourceSpan -> (Int -> LayoutFrame -> Bool) -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeImplicitLayouts anchor shouldClose = go []
  where
    closeTok = virtualSymbolToken "}" anchor

    go acc contexts =
      case contexts of
        LayoutImplicit frame : rest
          | shouldClose (layoutFrameIndent frame) frame -> go (closeTok : acc) rest
        _ -> (reverse acc, contexts)

closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | ctx <- contexts, isImplicitLayoutContext ctx]

{-# INLINE stepTokenContext #-}
stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo
      | layoutPrevTokenKind st == Just TkKeywordThen
          || layoutPrevTokenKind st == Just TkKeywordElse ->
          st {layoutPendingLayout = Just (PendingImplicitLayout thenElseDoLayout)}
      | otherwise -> st {layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    TkKeywordMdo -> st {layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    TkQualifiedDo {}
      | layoutPrevTokenKind st == Just TkKeywordThen
          || layoutPrevTokenKind st == Just TkKeywordElse ->
          st {layoutPendingLayout = Just (PendingImplicitLayout thenElseDoLayout)}
      | otherwise -> st {layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    TkQualifiedMdo {} -> st {layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    TkKeywordOf -> st {layoutPendingLayout = Just (PendingImplicitLayout (caseAlternativeLayout st))}
    TkKeywordCase
      | layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just (PendingImplicitLayout (caseAlternativeLayout st))}
      | otherwise -> st
    TkVarId "cases"
      | layoutLambdaCase st && layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just PendingMaybeLambdaCases}
      | otherwise -> st
    TkKeywordLet -> st {layoutPendingLayout = Just (PendingImplicitLayout strictLayout)}
    TkKeywordRec -> st {layoutPendingLayout = Just (PendingImplicitLayout ordinaryLayout)}
    TkKeywordWhere -> st {layoutPendingLayout = Just (PendingImplicitLayout strictLayout)}
    TkKeywordElse -> decrementAfterThenElseClassicIfDepth st
    TkKeywordIf -> st {layoutPendingLayout = Just PendingMaybeMultiWayIf}
    TkTHDeclQuoteOpen ->
      st
        { layoutContexts = LayoutDelimiter : layoutContexts st,
          layoutPendingLayout = Just (PendingImplicitLayout thDeclQuoteLayout)
        }
    kind
      | opensDelimiter kind ->
          st {layoutContexts = LayoutDelimiter : layoutContexts st}
    kind
      | closesDelimiter kind ->
          st {layoutContexts = popToDelimiter (layoutContexts st)}
    TkSpecialLBrace -> st {layoutContexts = LayoutExplicit : layoutContexts st}
    TkSpecialRBrace -> st {layoutContexts = popOneContext (layoutContexts st)}
    _ -> st

mapAfterThenElse :: (Int -> Int) -> [LayoutContext] -> [LayoutContext]
mapAfterThenElse f = go
  where
    go contexts =
      case contexts of
        LayoutImplicit frame@LayoutFrame {layoutFrameThenElseDepth = Just nestedIfs} : rest ->
          LayoutImplicit frame {layoutFrameThenElseDepth = Just (f nestedIfs)} : rest
        ctx : rest -> ctx : go rest
        [] -> []

noteClassicIfInAfterThenElse :: LayoutState -> LayoutState
noteClassicIfInAfterThenElse st = st {layoutContexts = mapAfterThenElse (+ 1) (layoutContexts st)}

decrementAfterThenElseClassicIfDepth :: LayoutState -> LayoutState
decrementAfterThenElseClassicIfDepth st = st {layoutContexts = mapAfterThenElse (max 0 . subtract 1) (layoutContexts st)}

popToDelimiter :: [LayoutContext] -> [LayoutContext]
popToDelimiter contexts =
  case contexts of
    LayoutDelimiter : rest -> rest
    _ : rest -> popToDelimiter rest
    [] -> []

popOneContext :: [LayoutContext] -> [LayoutContext]
popOneContext contexts =
  case contexts of
    _ : rest -> rest
    [] -> []

currentLayoutIndent :: [LayoutContext] -> Int
currentLayoutIndent contexts = fromMaybe 0 (currentLayoutIndentMaybe contexts)

currentLayoutIndentMaybe :: [LayoutContext] -> Maybe Int
currentLayoutIndentMaybe contexts =
  case contexts of
    LayoutImplicit LayoutFrame {layoutFrameIndent = indent} : _ -> Just indent
    _ -> Nothing

currentChildBaseline :: [LayoutContext] -> LayoutBaseline
currentChildBaseline contexts =
  case contexts of
    LayoutImplicit LayoutFrame {layoutFrameChildBaseline = baseline} : _ -> baseline
    _ -> LayoutCurrentIndent

isImplicitLayoutContext :: LayoutContext -> Bool
isImplicitLayoutContext ctx =
  case ctx of
    LayoutImplicit _ -> True
    LayoutExplicit -> False
    LayoutDelimiter -> False

-- | Tokens that open a delimiter context (parens, brackets, TH quotes,
-- unboxed parens).  These push 'LayoutDelimiter' to suppress implicit-layout
-- closures inside the delimited group.
--
-- Note: 'TkTHDeclQuoteOpen' is intentionally absent here because it is handled
-- explicitly in 'stepTokenContext', where it both pushes 'LayoutDelimiter' and
-- sets up a pending implicit layout for the declaration splice body.
opensDelimiter :: LexTokenKind -> Bool
opensDelimiter kind =
  case kind of
    TkSpecialLParen -> True
    TkSpecialLBracket -> True
    TkTHExpQuoteOpen -> True
    TkTHTypedQuoteOpen -> True
    TkTHTypeQuoteOpen -> True
    TkTHPatQuoteOpen -> True
    TkSpecialUnboxedLParen -> True
    _ -> False

-- | Tokens that close a delimiter context (parens, brackets, TH quotes,
-- unboxed parens).  These pop the context stack back to the matching
-- 'LayoutDelimiter' via 'popToDelimiter' in 'stepTokenContext'.
--
-- Related: 'closesImplicitBeforeDelimiter' determines which closing tokens
-- also force all intervening implicit layouts to emit virtual @}@ tokens
-- before the delimiter is popped.  'TkSpecialRBrace' is present there (but
-- absent here) because it closes 'LayoutExplicit', not 'LayoutDelimiter'.
closesDelimiter :: LexTokenKind -> Bool
closesDelimiter kind =
  case kind of
    TkSpecialRParen -> True
    TkSpecialUnboxedRParen -> True
    TkSpecialRBracket -> True
    TkTHExpQuoteClose -> True
    TkTHTypedQuoteClose -> True
    _ -> False

-- | Tokens before which all intervening implicit layouts must be closed
-- (emitting virtual @}@ tokens).  See 'closesDelimiter' for the relationship
-- between these two predicates.
closesImplicitBeforeDelimiter :: LexTokenKind -> Bool
closesImplicitBeforeDelimiter kind =
  case kind of
    TkSpecialRParen -> True
    TkSpecialUnboxedRParen -> True
    TkSpecialRBracket -> True
    TkTHExpQuoteClose -> True
    TkTHTypedQuoteClose -> True
    TkSpecialRBrace -> True
    _ -> False

-- | Reserved words that cannot start an item in the currently open layout list.
-- At the same column as that layout they trigger the report's parse-error rule:
-- inserting @}@ makes the keyword legal where inserting @;@ would not.
closesImplicitBeforeLayoutKeyword :: LexTokenKind -> Bool
closesImplicitBeforeLayoutKeyword kind =
  case kind of
    TkKeywordWhere -> True
    TkKeywordIn -> True
    TkKeywordThen -> True
    TkKeywordElse -> True
    _ -> False

isBOL :: LayoutState -> LexToken -> Bool
isBOL _ = lexTokenAtLineStart

layoutTransition :: LayoutState -> LexToken -> ([LexToken], LayoutState)
layoutTransition st tok =
  case lexTokenKind tok of
    TkLineComment -> ([tok], st)
    TkBlockComment -> ([tok], st)
    TkEOF ->
      let eofAnchor = fromMaybe (lexTokenSpan tok) (layoutPrevTokenEndSpan st)
          (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
          (pendingInserted, stAfterPending) = flushPendingCaseLayoutAtEOF stAfterModule eofAnchor
       in ( moduleInserted <> pendingInserted <> closeAllImplicit (layoutContexts stAfterPending) eofAnchor <> [tok],
            stAfterPending {layoutContexts = [], layoutBuffer = []}
          )
    _ ->
      let stModule = noteModuleLayoutBeforeToken st tok
          (preInserted, stBeforePending) = closeBeforeToken stModule tok
          (pendingInserted, stAfterPending, skipBOL) = openPendingLayout stBeforePending tok
          (bolInserted, stAfterBOL) = if skipBOL then ([], stAfterPending) else bolLayout stAfterPending tok
          stAfterToken = noteModuleLayoutAfterToken (stepTokenContext stAfterBOL tok) tok
          newEndSpan =
            if lexTokenOrigin tok == FromSource
              then Just (lexTokenSpan tok)
              else layoutPrevTokenEndSpan stAfterToken
          stNext =
            stAfterToken
              { layoutPrevTokenKind = Just (lexTokenKind tok),
                layoutPrevTokenEndSpan = newEndSpan,
                layoutBuffer = []
              }
       in (preInserted <> pendingInserted <> bolInserted <> [tok], stNext)

closeImplicitLayoutContext :: LayoutState -> Maybe LayoutState
closeImplicitLayoutContext st =
  case layoutContexts st of
    LayoutImplicit _ : rest -> Just (closeWith rest)
    _ -> Nothing
  where
    anchor = fromMaybe noSpan (layoutPrevTokenEndSpan st)
    noSpan =
      SourceSpan
        { sourceSpanSourceName = "",
          sourceSpanStartLine = 0,
          sourceSpanStartCol = 0,
          sourceSpanEndLine = 0,
          sourceSpanEndCol = 0,
          sourceSpanStartOffset = 0,
          sourceSpanEndOffset = 0
        }
    closeWith rest =
      st
        { layoutContexts = rest,
          layoutBuffer = virtualSymbolToken "}" anchor : layoutBuffer st
        }
