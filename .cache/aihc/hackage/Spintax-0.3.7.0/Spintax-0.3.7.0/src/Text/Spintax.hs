{-# LANGUAGE FlexibleContexts  #-}

module Text.Spintax where

import           Control.Applicative  ((<|>))
import           Control.Monad.Reader (ask, runReaderT)
import           Data.Attoparsec.Text
import           Data.List            as L (intersperse)
import qualified Data.List.Extra      as E
import qualified Data.Text            as T
import           System.Random.MWC

-- | Generate random texts based on a spinning syntax template, with nested alternatives and empty options.
--
-- >λ> spintax "{{Oh my God|Awesome|Can't believe that}, {a|the}|A|The} {quick {and dirty |||}||}{brown |pink |grey |black |yellow }{fox|flea|elephant|panther|bear} jumps over {the|a} {lazy |smelly |sleepy |grouchy }{dog|cat|whale}{|||, that's {really |||}amazing}{.|!|...}"
-- > Right "Awesome, the quick pink fox jumps over a sleepy whale."
--
spintax :: T.Text -> IO (Either String T.Text)
spintax template =
  createSystemRandom >>= runReaderT (spin template)
    where
      spin t = go T.empty [] t (0::Int)
        where

          go o as i l
            | l < 0  = parseFail
            | l == 0 =
              case parse spinSyntax i of
                Done r m  ->
                  case m of
                    "{"                      -> go o as r (l+1)
                    n | n == "}" || n == "|" -> parseFail
                    _                        -> go (o <> m) as r l
                Partial _ -> pure (Right $ o <> i)
                Fail {}   -> parseFail
            | l == 1 =
              case parse spinSyntax i of
                Done r m ->
                  case m of
                    "{" -> go o (addAlter m) r (l+1)
                    "}" -> do
                      a <- spin =<< randAlter =<< ask
                      case a of
                        Right t' -> go (o <> t') [] r (l-1)
                        Left _   -> parseFail
                    "|" ->
                      if E.null as
                        then go o ["",""] r l
                        else go o (E.snoc as "") r l
                    _   -> go o (addAlter m) r l
                Partial _ -> parseFail
                Fail {} -> parseFail
            | l > 1 =
              case parse spinSyntax i of
                Done r m ->
                  case m of
                    "{" -> go o (addAlter m) r (l+1)
                    "}" -> go o (addAlter m) r (l-1)
                    _   -> go o (addAlter m) r l
                Partial _ -> parseFail
                Fail {}   -> parseFail
            where
              addAlter n =
                  case E.unsnoc as of
                    Just (xs,x) -> E.snoc xs (x <> n)
                    Nothing     -> pure n
              randAlter g =
                (\r -> (!!) as (r-1)) <$> uniformR (1,E.length as) g
          go _ _ _ _ = parseFail

          parseFail = fail "Spintax template parsing failure"

          spinSyntax =
            openBrace <|> closeBrace <|> pipe <|> content
              where
                openBrace = string "{"
                closeBrace = string "}"
                pipe = string "|"
                content =
                  takeWhile1 ctt
                    where
                      ctt '{' = False
                      ctt '}' = False
                      ctt '|' = False
                      ctt _   = True

-- * Utils

-- | Write a spintax alternative
--
-- >λ>  writeSpintaxAlternative ["apple","apricot","banana","coconut"]
-- "{apple|apricot|banana|coconut}"
writeSpintaxAlternative :: [T.Text] -> T.Text
writeSpintaxAlternative = writeSpintaxExpression "|"

writeSpintaxExpression :: T.Text -> [T.Text] -> T.Text
writeSpintaxExpression s l = "{" <> T.concat (L.intersperse s l) <> "}"

