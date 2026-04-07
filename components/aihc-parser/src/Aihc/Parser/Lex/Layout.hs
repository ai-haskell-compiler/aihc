{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Lex.Layout
  ( applyLayoutTokens,
    layoutTransition,
    closeImplicitLayoutContext,
  )
where

import Aihc.Parser.Lex.Types
import Aihc.Parser.Syntax (SourceSpan (..))
import Data.Maybe (fromMaybe)

applyLayoutTokens :: Bool -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout =
  go (mkInitialLayoutState enableModuleLayout)
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
    ModuleLayoutSeekStart ->
      ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
        st {layoutModuleMode = ModuleLayoutDone}
      )
    ModuleLayoutAwaitBody ->
      ( [virtualSymbolToken "{" anchor, virtualSymbolToken "}" anchor],
        st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Nothing}
      )
    _ -> ([], st)

noteModuleLayoutBeforeToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutBeforeToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitBody -> st {layoutModuleMode = ModuleLayoutDone}
    ModuleLayoutSeekStart ->
      case lexTokenKind tok of
        TkPragmaLanguage _ -> st
        TkPragmaInstanceOverlap _ -> st
        TkPragmaWarning _ -> st
        TkPragmaDeprecated _ -> st
        TkPragmaDeclaration _ -> st
        TkKeywordModule -> st {layoutModuleMode = ModuleLayoutAwaitWhere}
        _ -> st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    _ -> st

noteModuleLayoutAfterToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutAfterToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitWhere
      | lexTokenKind tok == TkKeywordWhere ->
          st {layoutModuleMode = ModuleLayoutAwaitBody, layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    _ -> st

openPendingLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openPendingLayout st tok =
  case layoutPendingLayout st of
    Nothing -> ([], st, False)
    Just pending ->
      case pending of
        PendingMaybeMultiWayIf ->
          case lexTokenKind tok of
            TkReservedPipe -> openImplicitLayout LayoutMultiWayIf st tok
            _ -> ([], st {layoutPendingLayout = Nothing}, False)
        PendingImplicitLayout kind ->
          case lexTokenKind tok of
            TkSpecialLBrace -> ([], st {layoutPendingLayout = Nothing}, False)
            _ -> openImplicitLayout kind st tok

openImplicitLayout :: ImplicitLayoutKind -> LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openImplicitLayout kind st tok =
  let col = tokenStartCol tok
      parentIndent = currentLayoutIndent (layoutContexts st)
      openTok = virtualSymbolToken "{" (lexTokenSpan tok)
      closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
      newContext = LayoutImplicit col kind
   in if col <= parentIndent
        then ([openTok, closeTok], st {layoutPendingLayout = Nothing}, False)
        else
          ( [openTok],
            st
              { layoutPendingLayout = Nothing,
                layoutContexts = newContext : layoutContexts st
              },
            True
          )

closeBeforeToken :: LayoutState -> LexToken -> ([LexToken], LayoutState)
closeBeforeToken st tok =
  closeWith $
    case lexTokenKind tok of
      TkKeywordIn -> closeLeadingImplicitLet (lexTokenSpan tok)
      kind
        | closesImplicitBeforeDelimiter kind ->
            closeImplicitLayouts (lexTokenSpan tok) (\_ _ -> True)
      TkKeywordThen -> closeBeforeThenElse
      TkKeywordElse -> closeBeforeThenElse
      TkKeywordWhere ->
        closeImplicitLayouts (lexTokenSpan tok) (\indent _ -> tokenStartCol tok <= indent)
      _ -> noLayoutClosures
  where
    closeBeforeThenElse =
      let col = tokenStartCol tok
       in closeImplicitLayouts (lexTokenSpan tok) $
            \indent kind -> col < indent || (kind == LayoutAfterThenElse && col <= indent)

    closeWith closeContexts =
      let (inserted, contexts') = closeContexts (layoutContexts st)
       in (inserted, st {layoutContexts = contexts'})

    noLayoutClosures contexts = ([], contexts)

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
                | col == indent && currentLayoutAllowsSemicolon contexts' ->
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

stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo
      | layoutPrevTokenKind st == Just TkKeywordThen
          || layoutPrevTokenKind st == Just TkKeywordElse ->
          st {layoutPendingLayout = Just (PendingImplicitLayout LayoutAfterThenElse)}
      | otherwise -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordMdo -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordOf -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordCase
      | layoutPrevTokenKind st == Just TkReservedBackslash ->
          st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
      | otherwise -> st
    TkKeywordLet -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutLetBlock)}
    TkKeywordRec -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordWhere -> st {layoutPendingLayout = Just (PendingImplicitLayout LayoutOrdinary)}
    TkKeywordIf -> st {layoutPendingLayout = Just PendingMaybeMultiWayIf}
    kind
      | opensDelimiter kind ->
          st {layoutContexts = LayoutDelimiter : layoutContexts st}
    kind
      | closesDelimiter kind ->
          st {layoutContexts = popToDelimiter (layoutContexts st)}
    TkSpecialLBrace -> st {layoutContexts = LayoutExplicit : layoutContexts st}
    TkSpecialRBrace -> st {layoutContexts = popOneContext (layoutContexts st)}
    _ -> st

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

opensDelimiter :: LexTokenKind -> Bool
opensDelimiter kind =
  case kind of
    TkSpecialLParen -> True
    TkSpecialLBracket -> True
    TkTHExpQuoteOpen -> True
    TkTHTypedQuoteOpen -> True
    TkTHDeclQuoteOpen -> True
    TkTHTypeQuoteOpen -> True
    TkTHPatQuoteOpen -> True
    TkSpecialUnboxedLParen -> True
    _ -> False

closesDelimiter :: LexTokenKind -> Bool
closesDelimiter kind =
  case kind of
    TkSpecialRParen -> True
    TkSpecialUnboxedRParen -> True
    TkSpecialRBracket -> True
    TkTHExpQuoteClose -> True
    TkTHTypedQuoteClose -> True
    _ -> False

closesImplicitBeforeDelimiter :: LexTokenKind -> Bool
closesImplicitBeforeDelimiter kind =
  case kind of
    TkSpecialRParen -> True
    TkSpecialRBracket -> True
    TkTHExpQuoteClose -> True
    TkTHTypedQuoteClose -> True
    TkSpecialRBrace -> True
    _ -> False

isBOL :: LayoutState -> LexToken -> Bool
isBOL st tok =
  case layoutPrevLine st of
    Just prevLine -> tokenStartLine tok > prevLine
    Nothing -> False

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
              { layoutPrevLine = Just (tokenStartLine tok),
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
