module Asm.Parsers.Expr (parseExpr) where

import Data.Char (digitToInt)

import Asm.ArtifactAccumulator (___, use, used1, used2)
import Asm.BaseParser (parseSuccessFromCs)
import Asm.Parsers.Meta (parseAny)
import Asm.Parsers.Tokens
  ( parseAmpersand
  , parseAsterisk
  , parseBar
  , parseCloseParens
  , parseDigits
  , parseDollar
  , parseGreaterThan
  , parseHexDigits
  , parseLabel
  , parseLessThan
  , parseMinus
  , parseOpenParens
  , parseOptionalSpace
  , parsePlus
  , parseε
  )
import Asm.Types.CharStream (CharStream)
import Asm.Types.Expr
  ( Expr
  , ExprBase (..)
  , ExprBitwiseTerm (..)
  , ExprBitwiseTerm' (..)
  , ExprFactor (..)
  , ExprFactor' (..)
  , ExprMinusOrBase (..)
  , ExprTerm (..)
  , ExprTerm' (..)
  )
import Asm.Types.Parser (ParseAttempt (ParseSuccess), Parser)


-- | Parse a numeric expression, in abstract form
parseExpr :: Parser Expr
parseExpr =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseExprBitwiseTerm)
    >>= use (getCs)
    >>= used2 (\(expr, cs') -> (cs, cs', expr))
  where
    getCs :: Parser CharStream
    getCs cs = ParseSuccess(cs, cs)


-- Bitwise terms — loosest binding

parseExprBitwiseTerm :: Parser ExprBitwiseTerm
parseExprBitwiseTerm =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseExprTerm)
    >>= use (parseExprBitwiseTerm')
    >>= used2 (\(a1, a2) -> ExprBitwiseTerm a1 a2)

parseExprBitwiseTerm' :: Parser ExprBitwiseTerm'
parseExprBitwiseTerm' = parseAny
  [ parseExprBitwiseTerm'And
  , parseExprBitwiseTerm'Or
  , parseExprBitwiseTerm'ε
  ]

parseExprBitwiseTerm'And :: Parser ExprBitwiseTerm'
parseExprBitwiseTerm'And =
  parseBinaryOperator
    parseAmpersand
    parseExprTerm
    parseExprBitwiseTerm'
    (\(a1, a2) -> ExprBitwiseTerm'And a1 a2)

parseExprBitwiseTerm'Or :: Parser ExprBitwiseTerm'
parseExprBitwiseTerm'Or =
  parseBinaryOperator
    parseBar
    parseExprTerm
    parseExprBitwiseTerm'
    (\(a1, a2) -> ExprBitwiseTerm'Or a1 a2)

parseExprBitwiseTerm'ε :: Parser ExprBitwiseTerm'
parseExprBitwiseTerm'ε = parseε ExprBitwiseTerm'ε


-- Addition/subtraction terms

parseExprTerm :: Parser ExprTerm
parseExprTerm =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseExprFactor)
    >>= use (parseExprTerm')
    >>= used2 (\(a1, a2) -> ExprTerm a1 a2)

parseExprTerm' :: Parser ExprTerm'
parseExprTerm' = parseAny
  [ parseExprTerm'Plus
  , parseExprTerm'Minus
  , parseExprTerm'ε
  ]

parseExprTerm'Plus :: Parser ExprTerm'
parseExprTerm'Plus =
  parseBinaryOperator
    parsePlus
    parseExprFactor
    parseExprTerm'
    (\(a1, a2) -> ExprTerm'Plus a1 a2)

parseExprTerm'Minus :: Parser ExprTerm'
parseExprTerm'Minus =
  parseBinaryOperator
    parseMinus
    parseExprFactor
    parseExprTerm'
    (\(a1, a2) -> ExprTerm'Minus a1 a2)

parseExprTerm'ε :: Parser ExprTerm'
parseExprTerm'ε = parseε ExprTerm'ε


-- Multiplication terms

parseExprFactor :: Parser ExprFactor
parseExprFactor =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseExprMinusOrBase)
    >>= use (parseExprFactor')
    >>= used2 (\(a1, a2) -> ExprFactor a1 a2)

parseExprFactor' :: Parser ExprFactor'
parseExprFactor' = parseAny
  [ parseExprFactor'Times
  , parseExprFactor'ε
  ]

parseExprFactor'Times :: Parser ExprFactor'
parseExprFactor'Times =
  parseBinaryOperator
    parseAsterisk
    parseExprFactor
    parseExprFactor'
    (\(a1, a2) -> ExprFactor'Times a1 a2)

parseExprFactor'ε :: Parser ExprFactor'
parseExprFactor'ε = parseε ExprFactor'ε


-- Unary minus, low-byte and high-byte - tightest binding

parseExprMinusOrBase :: Parser ExprMinusOrBase
parseExprMinusOrBase =
  parseAny
    [ parseExprMinusOrBaseLoByte
    , parseExprMinusOrBaseHiByte
    , parseExprMinusOrBaseMinus
    , parseExprMinusOrBaseBase
    ]

parseExprMinusOrBaseLoByte :: Parser ExprMinusOrBase
parseExprMinusOrBaseLoByte =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseLessThan)
    >>= use (parseExprBase)
    >>= used1 (\exp -> ExprMinusOrBaseLoByte exp)

parseExprMinusOrBaseHiByte :: Parser ExprMinusOrBase
parseExprMinusOrBaseHiByte =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseGreaterThan)
    >>= use (parseExprBase)
    >>= used1 (\exp -> ExprMinusOrBaseHiByte exp)

parseExprMinusOrBaseMinus :: Parser ExprMinusOrBase
parseExprMinusOrBaseMinus =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseMinus)
    >>= use (parseExprBase)
    >>= used1 (\exp -> ExprMinusOrBaseMinus exp)

parseExprMinusOrBaseBase :: Parser ExprMinusOrBase
parseExprMinusOrBaseBase =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseExprBase)
    >>= used1 (\exp -> ExprMinusOrBaseBase exp)


-- Base expressions (literals, symbols, parenthetic expressions)

parseExprBase :: Parser ExprBase
parseExprBase =
  parseAny
    [ parseDecLiteral
    , parseHexLiteral
    , parseExprBaseSymbol
    , parseExprBaseParensTerm
    ]

parseExprBaseSymbol :: Parser ExprBase
parseExprBaseSymbol =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseLabel)
    >>= used1 (\str -> ExprBaseSymbol $ str)

parseExprBaseParensTerm :: Parser ExprBase
parseExprBaseParensTerm =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseOpenParens)
    >>= ___ (parseOptionalSpace)
    >>= use (parseExprTerm)
    >>= ___ (parseOptionalSpace)
    >>= ___ (parseCloseParens)
    >>= used1 (\exp -> ExprBaseParensTerm exp)


-- Helper functions

parseBinaryOperator
  :: Parser operator
  -> Parser rhs
  -> Parser next
  -> ((rhs, next) -> result)
  -> Parser result
parseBinaryOperator parseOperator parseRhs parseNext emitter =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseOptionalSpace)
    >>= ___ (parseOperator)
    >>= ___ (parseOptionalSpace)
    >>= use (parseRhs)
    >>= use (parseNext)
    >>= used2 (emitter)


parseDecLiteral :: Parser ExprBase
parseDecLiteral =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseDigits)
    >>= used1 (\str -> ExprBaseLiteral $ read str)


parseHexLiteral :: Parser ExprBase
parseHexLiteral =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseDollar)
    >>= use (parseHexDigits)
    >>= used1 (\str -> ExprBaseLiteral $ hexToInt str)
  where
    hexToInt :: String -> Int
    hexToInt str = hTI' (reverse str)

    hTI' ""     = 0
    hTI' (x:xs) = ((hTI' xs) * 16) + digitToInt x
