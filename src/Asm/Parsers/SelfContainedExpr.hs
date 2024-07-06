module Asm.Parsers.SelfContainedExpr (parseSelfContainedExpr) where

import Asm.EvalExpr (evalExprNoSymbols)
import Asm.Parsers.Expr (parseExpr)
import Asm.Types.Parser (ParseAttempt (ParseError, ParseSuccess), Parser)


-- | Parse an expression that can be entirely evaluated in pass 2 (in other
--   words, one that doesn't use symbols), and evaluate it to a number
parseSelfContainedExpr :: Parser Int
parseSelfContainedExpr cs = do
  (cs', expr) <- parseExpr cs
  case evalExprNoSymbols expr of
    Left err -> ParseError err
    Right n  -> ParseSuccess (cs', n)
