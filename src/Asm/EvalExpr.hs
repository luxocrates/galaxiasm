module Asm.EvalExpr (evalExprWithSymbols, evalExprNoSymbols) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Map (lookup)

import Asm.Types.AsmFailable
  ( AsmErrorDetails
    ( LocalUseButNoParentError
    , UndefSymbolError
    , UnavailSymbolError
    )
  , AsmErrorLoc (CsRange)
  , AsmFailable
  )
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
import Asm.Types.Pass2 (SymTable)


-- | Evaluate an expression, given a symbol table
evalExprWithSymbols
  :: SymTable         -- symbol table
  -> Maybe String     -- the name of the parent label, if there is one
  -> Expr             -- the expression to evaluate
  -> AsmFailable Int  -- the value of the expression, or an error reason
evalExprWithSymbols syms maybeParent = evalExpr lookerUpper where
  lookerUpper :: String -> Either AsmErrorDetails Int
  lookerUpper localOrGlobalSym = do

    -- If the symbol is a local label, convert it to a global one
    globalSym <- case localOrGlobalSym of
      '_':str -> case maybeParent of
        Nothing          -> Left LocalUseButNoParentError
        Just parentLabel -> Right $ parentLabel ++ "_" ++ str
      _       -> Right localOrGlobalSym

    case Data.Map.lookup globalSym syms of
      Just n  -> let (val, _) = n in Right val
      Nothing -> Left $ UndefSymbolError globalSym


-- | Evaluate an expression that uses no symbols
evalExprNoSymbols :: Expr -> AsmFailable Int
evalExprNoSymbols = evalExpr lookerUpper where
  lookerUpper _ = Left UnavailSymbolError


-- | Evaluate an expression
evalExpr
  :: (String -> Either AsmErrorDetails Int)  -- symbol table lookup function
  -> Expr                                    -- abstract expression parsing
  -> AsmFailable Int
evalExpr lookup (cs, cs', expr)
  = case evalExprBitwiseTerm expr of
      -- Augment error details with the missing location information
      Left errDetails -> Left (CsRange cs cs', errDetails)
      Right x         -> Right x
  where
    -- Bitwise terms — loosest binding

    evalExprBitwiseTerm :: ExprBitwiseTerm -> Either AsmErrorDetails Int
    evalExprBitwiseTerm (ExprBitwiseTerm term term') = do
      val <- evalExprTerm term
      evalExprBitwiseTerm' val term'

    evalExprBitwiseTerm' :: Int -> ExprBitwiseTerm' -> Either AsmErrorDetails Int
    evalExprBitwiseTerm' val term' = case term' of
      ExprBitwiseTerm'And term term'2 -> helper term term'2 (.&.)
      ExprBitwiseTerm'Or  term term'2 -> helper term term'2 (.|.)
      ExprBitwiseTerm'ε               -> return val
      where
        helper term term'2 fn = do
          val2 <- evalExprTerm term
          evalExprBitwiseTerm' (fn val val2) term'2

    -- Addition/subtraction terms

    evalExprTerm :: ExprTerm -> Either AsmErrorDetails Int
    evalExprTerm (ExprTerm base term') = do
      val <- evalExprFactor base
      evalExprTerm' val term'

    evalExprTerm' :: Int -> ExprTerm' -> Either AsmErrorDetails Int
    evalExprTerm' val term' = case term' of
      ExprTerm'Plus  factor term'2 -> helper factor term'2 (+)
      ExprTerm'Minus factor term'2 -> helper factor term'2 (-)
      ExprTerm'ε                   -> return val
      where
        helper factor term'2 fn = do
          val2 <- evalExprFactor factor
          evalExprTerm' (fn val val2) term'2

    -- Multiplication terms

    evalExprFactor :: ExprFactor -> Either AsmErrorDetails Int
    evalExprFactor (ExprFactor minusOrBase factor') = do
      val <- evalExprMinusOrBase minusOrBase
      evalExprFactor' val factor'

    evalExprFactor' :: Int -> ExprFactor' -> Either AsmErrorDetails Int
    evalExprFactor' val factor' = case factor' of
      ExprFactor'Times factor factor'2 -> do
        val2 <- evalExprFactor factor
        evalExprFactor' (val * val2) factor'2
      ExprFactor'ε                     -> return val

    -- Unary minus, low-byte and high-byte - tightest binding

    evalExprMinusOrBase :: ExprMinusOrBase -> Either AsmErrorDetails Int
    evalExprMinusOrBase (ExprMinusOrBaseMinus expr) = do
      n <- evalExprBase expr
      return (-n)
    evalExprMinusOrBase (ExprMinusOrBaseLoByte expr) = do
      n <- evalExprBase expr
      return $ n .&. 0xff
    evalExprMinusOrBase (ExprMinusOrBaseHiByte expr) = do
      n <- evalExprBase expr
      return $ (shiftR n 8) .&. 0xff
    evalExprMinusOrBase (ExprMinusOrBaseBase expr)  = evalExprBase expr

    -- Base expressions (literals, symbols, parenthetic expressions)

    evalExprBase :: ExprBase -> Either AsmErrorDetails Int
    evalExprBase (ExprBaseLiteral n)    = return n
    evalExprBase (ExprBaseSymbol s)     = lookup s
    evalExprBase (ExprBaseParensTerm t) = evalExprTerm t
