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

applyLayoutTokens :: Bool -> [Extension] -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout exts =
  go (mkInitialLayoutState enableModuleLayout exts)
  where
    go st toks =
      case toks of
        [] ->
          let eofAnchor = NoSourceSpan
              (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
           in moduleInserted <> closeAllImplicit (layoutContexts stAfterModule) eofAnchor
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
        TkKeywordModule -> st {layoutModuleMode = ModuleLayoutAwaitWhere}
        _ -> st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    _ -> st

{-# INLINE noteModuleLayoutAfterToken #-}
noteModuleLayoutAfterToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutAfterToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitWhere
      | lexTokenKind tok == TkKeywordWhere ->
          st {layoutModuleMode = ModuleLayoutAwaitBody, layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
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
            TkReservedPipe -> openImplicitLayout LayoutMultiWayIf st tok
            _ -> ([], noteClassicIfInAfterThenElse (st {layoutPendingLayout = Nothing}), False)
        PendingMaybeLambdaCases ->
          case lexTokenKind tok of
            TkSpecialLBrace -> ([], st {layoutPendingLayout = Nothing}, False)
            TkReservedRightArrow -> ([], st {layoutPendingLayout = Nothing}, False)
            _ -> openImplicitLayout LayoutCaseAlternative st tok
        PendingImplicitLayout kind ->
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
            _ -> openImplicitLayout kind st tok

openImplicitLayout :: ImplicitLayoutKind -> LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openImplicitLayout kind st tok =
  let col = tokenStartCol tok
      parentIndent = currentLayoutIndent (layoutContexts st)
      openTok = virtualSymbolToken "{" (lexTokenSpan tok)
      closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
      newContext = LayoutImplicit col kind
      -- Under NondecreasingIndentation, a nested context at the same level
      -- as its parent is allowed (produces normal layout, not empty {}).
      opensEmpty = if layoutNondecreasingIndent st then col < parentIndent else col <= parentIndent
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
    TkKeywordWhere -> closeBeforeWhere
    TkKeywordIn ->
      let (inserted, ctxs') = closeLeadingImplicitLet anchor (layoutContexts st)
       in (inserted, st {layoutContexts = ctxs'})
    kind
      | closesImplicitBeforeDelimiter kind ->
          let (inserted, ctxs') = closeImplicitLayouts anchor (\_ _ -> True) (layoutContexts st)
           in (inserted, st {layoutContexts = ctxs'})
    TkKeywordThen -> closeBeforeThenElse
    TkKeywordElse -> closeBeforeThenElse
    _ -> ([], st)
  where
    anchor = lexTokenSpan tok
    closeTok = virtualSymbolToken "}" anchor

    -- Close an immediately enclosing LayoutCaseAlternative when 'where'
    -- appears at or to the left of its indent column.  This is a single-step
    -- check on the top of the context stack (not a loop).
    closeBeforeWhere =
      case layoutContexts st of
        LayoutImplicit indent LayoutCaseAlternative : rest
          | tokenStartCol tok <= indent ->
              let openTok = virtualSymbolToken "{" anchor
                  (pendingInserted, st') =
                    case layoutPendingLayout st of
                      Just (PendingImplicitLayout _) ->
                        ([openTok, closeTok], st {layoutPendingLayout = Nothing})
                      _ -> ([], st)
               in (pendingInserted <> [closeTok], st' {layoutContexts = rest})
        _ -> ([], st)

    -- Close implicit layouts before 'then'/'else'.
    -- A `then do`/`else do` block stays open across nested conditionals inside
    -- the block. Only a `then`/`else` at or to the left of the block's own
    -- layout column can terminate it.
    closeBeforeThenElse =
      let col = tokenStartCol tok
          go ctxs =
            case ctxs of
              LayoutImplicit indent kind : rest
                | LayoutAfterThenElse nestedIfs <- kind,
                  nestedIfs == 0,
                  col <= indent ->
                    ([closeTok], rest)
                | col < indent ->
                    let (inner, rest') = go rest
                     in (closeTok : inner, rest')
              _ -> ([], ctxs)
          (closed, ctxs') = go (layoutContexts st)
       in (closed, st {layoutContexts = ctxs'})

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
    LayoutImplicit _ LayoutMultiWayIf : _ -> False
    LayoutImplicit _ _ : _ -> True
    _ -> False

closeImplicitLayouts :: SourceSpan -> (Int -> ImplicitLayoutKind -> Bool) -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeImplicitLayouts anchor shouldClose = go []
  where
    closeTok = virtualSymbolToken "}" anchor

    go acc contexts =
      case contexts of
        LayoutImplicit indent kind : rest
          | shouldClose indent kind -> go (closeTok : acc) rest
        _ -> (reverse acc, contexts)

closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | ctx <- contexts, isImplicitLayoutContext ctx]

closeLeadingImplicitLet :: SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeLeadingImplicitLet anchor contexts =
  case contexts of
    LayoutImplicit _ LayoutLetBlock : rest -> ([virtualSymbolToken "}" anchor], rest)
    _ -> ([], contexts)

{-# INLINE stepTokenContext #-}
stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo
      | layoutPrevTokenKind st == Just TkKeywordThen
          || layoutPrevTokenKind st == Just TkKeywordElse ->
          st {layoutPendingLayout = Just (PendingImplicitLayout (LayoutAfterThenElse 0))}
      | otherwise -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordMdo -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordOf -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutCaseAlternative)}
    TkKeywordCase
      | layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just (PendingImplicitLayout LayoutCaseAlternative)}
      | otherwise -> st
    TkVarId "cases"
      | layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just PendingMaybeLambdaCases}
      | otherwise -> st
    TkKeywordLet -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutLetBlock)}
    TkKeywordRec -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordWhere -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordElse -> decrementAfterThenElseClassicIfDepth st
    TkKeywordIf -> st {layoutPendingLayout = Just PendingMaybeMultiWayIf}
    TkTHDeclQuoteOpen ->
      st
        { layoutContexts = LayoutDelimiter : layoutContexts st,
          layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)
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
        LayoutImplicit indent (LayoutAfterThenElse nestedIfs) : rest ->
          LayoutImplicit indent (LayoutAfterThenElse (f nestedIfs)) : rest
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
    LayoutImplicit indent _ : _ -> Just indent
    _ -> Nothing

isImplicitLayoutContext :: LayoutContext -> Bool
isImplicitLayoutContext ctx =
  case ctx of
    LayoutImplicit _ _ -> True
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

isBOL :: LayoutState -> LexToken -> Bool
isBOL _ = lexTokenAtLineStart

layoutTransition :: LayoutState -> LexToken -> ([LexToken], LayoutState)
layoutTransition st tok =
  case lexTokenKind tok of
    TkEOF ->
      let eofAnchor = fromMaybe (lexTokenSpan tok) (layoutPrevTokenEndSpan st)
          (moduleInserted, stAfterModule) = finalizeModuleLayoutAtEOF st eofAnchor
       in ( moduleInserted <> closeAllImplicit (layoutContexts stAfterModule) eofAnchor <> [tok],
            stAfterModule {layoutContexts = [], layoutBuffer = []}
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
              { layoutPrevLine = Just (tokenEndLine tok),
                layoutPrevTokenKind = Just (lexTokenKind tok),
                layoutPrevTokenEndSpan = newEndSpan,
                layoutBuffer = []
              }
       in (preInserted <> pendingInserted <> bolInserted <> [tok], stNext)

closeImplicitLayoutContext :: LayoutState -> Maybe LayoutState
closeImplicitLayoutContext st =
  case layoutContexts st of
    LayoutImplicit _ _ : rest -> Just (closeWith rest)
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
