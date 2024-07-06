module Asm.Types.Expr
  ( Expr
  , ExprBitwiseTerm (..)
  , ExprBitwiseTerm' (..)
  , ExprTerm (..)
  , ExprTerm' (..)
  , ExprFactor (..)
  , ExprFactor' (..)
  , ExprMinusOrBase (..)
  , ExprBase (..)
  )
where

import Asm.Types.CharStream (CharStream)


-- | A parsing of a numeric expression, in abstract form
type Expr =
  ( CharStream       -- start of expression
  , CharStream       -- end of expression
  , ExprBitwiseTerm  -- top-level term
  )

data ExprBitwiseTerm
  = ExprBitwiseTerm ExprTerm ExprBitwiseTerm'
  deriving (Eq, Show)

data ExprBitwiseTerm'
  = ExprBitwiseTerm'And ExprTerm ExprBitwiseTerm'  -- x & y
  | ExprBitwiseTerm'Or  ExprTerm ExprBitwiseTerm'  -- x | y
  | ExprBitwiseTerm'ε
  deriving (Eq, Show)

data ExprTerm
  = ExprTerm ExprFactor ExprTerm'
  deriving (Eq, Show)

data ExprTerm'
  = ExprTerm'Plus  ExprFactor ExprTerm'  -- x + y
  | ExprTerm'Minus ExprFactor ExprTerm'  -- x - y
  | ExprTerm'ε
  deriving (Eq, Show)

data ExprFactor
  = ExprFactor ExprMinusOrBase ExprFactor'
  deriving (Eq, Show)

data ExprFactor'
  = ExprFactor'Times ExprFactor ExprFactor'  -- x * y
  | ExprFactor'ε
  deriving (Eq, Show)

data ExprMinusOrBase
   = ExprMinusOrBaseMinus ExprBase   -- x - y
   | ExprMinusOrBaseLoByte ExprBase  -- >x
   | ExprMinusOrBaseHiByte ExprBase  -- <x
   | ExprMinusOrBaseBase ExprBase    -- -x
  deriving (Eq, Show)

data ExprBase
  = ExprBaseLiteral Int          -- a decimal or hexadecimal constant
  | ExprBaseSymbol String        -- a symbol or label reference
  | ExprBaseParensTerm ExprTerm  -- a parenthesized expression
  deriving (Eq, Show)
