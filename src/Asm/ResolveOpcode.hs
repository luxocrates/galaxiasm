module Asm.ResolveOpcode (resolveOpcode) where

import Control.Monad (foldM)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word32, Word8)

import Asm.EvalExpr (evalExprWithSymbols)
import Asm.Types.AsmFailable
  ( AsmErrorDetails (BoundsError, InvalidRstError)
  , AsmErrorLoc (CsRange)
  , AsmFailable
  )
import Asm.Types.Expr (Expr)
import Asm.Types.Pass2
  ( Ast (..)
  , Ast_PlusMinus (Ast_Plus)
  , SymTable
  , UnresolvedBitsInContext
  )
import Common.BitOps (byteSwap)


-- | Applies the symbol table to some marked-up bit sequences to yield a final
--   byte sequence
resolveOpcode
  :: UnresolvedBitsInContext
  -> SymTable
  -> AsmFailable [Word8]

resolveOpcode (addr, maybeParent, (base, len, parts), _) syms = do
  -- Reify each component into final bits, in-position in a Word32, then
  -- bitwise-or everything together
  w32 <- foldM (
    \acc unresolved -> do
      resolved <- resolveAst syms maybeParent addr unresolved
      return $ acc .|. resolved
    )
    base
    parts

  -- Convert the Word32 into a sequence of bytes, based on what we were told
  -- the length for the opcode is
  return $ case len of
        1 ->  [ fromIntegral $ (shiftR w32 24) .&. 0xff
              ]
        2 ->  [ fromIntegral $ (shiftR w32 24) .&. 0xff
              , fromIntegral $ (shiftR w32 16) .&. 0xff
              ]
        3 ->  [ fromIntegral $ (shiftR w32 24)
              , fromIntegral $ (shiftR w32 16) .&. 0xff
              , fromIntegral $ (shiftR w32 8 ) .&. 0xff
              ]
        4 ->  [ fromIntegral $ (shiftR w32 24)
              , fromIntegral $ (shiftR w32 16) .&. 0xff
              , fromIntegral $ (shiftR w32 8 ) .&. 0xff
              , fromIntegral $ (shiftR w32 0 ) .&. 0xff
              ]
        _ ->  []


-- | Applies the symbol table to a single AST to yield one component bit
--   sequence
resolveAst
  :: SymTable      -- global symbols
  -> Maybe String  -- parent label
  -> Int           -- destination address of start of opcode
  -> Ast           -- component to resolve
  -> AsmFailable Word32

-- A constant bit sequence
resolveAst _ _ _ (Ast_Const w32) = Right w32

-- Expression for 'b' parameter
resolveAst syms maybeParent _ (Ast_B start expr) = do
  val <- evalExprWithSymbols syms maybeParent expr
  maskedVal <- rangeCheck 0 7 0x7 val expr
  return $ fromIntegral $ shiftL maskedVal (32 - 3 - start)

-- Expression for 'n' parameter
resolveAst syms maybeParent _ (Ast_N start expr) = do
  val <- evalExprWithSymbols syms maybeParent expr
  maskedVal <- rangeCheck (-128) 255 0xff val expr
  return $ fromIntegral $ shiftL maskedVal (32 - 8 - start)

-- Expression for ix/iy plus-minus delta parameter
resolveAst syms maybeParent _ (Ast_PlusMinusD plusMinus start expr) = do
  val <- evalExprWithSymbols syms maybeParent expr
  let d = if plusMinus == Ast_Plus then val else (-val)
  maskedD <- rangeCheck (-128) 127 0xff d expr
  return $ shiftL (fromIntegral maskedD) (32 - 8 - start)

-- Expression for 'nn' parameter
resolveAst syms maybeParent _ (Ast_Nn start expr) = do
  val <- evalExprWithSymbols syms maybeParent expr
  maskedVal <- rangeCheck (-32768) 65535 0xffff val expr
  return $ fromIntegral $ shiftL (byteSwap maskedVal) (32 - 16 - start)

-- Expression for 'p' parameter
resolveAst syms maybeParent _ (Ast_P start expr) = do
  val <- evalExprWithSymbols syms maybeParent expr
  if ((fromIntegral val :: Word32) .&. complement 0x38) /= 0
    then
      let (start, end, _) = expr
      in Left (CsRange start end, InvalidRstError)
    else
      Right $ fromIntegral $ shiftL (shiftR val 3) (32 - 3 - start)

-- Expression for 'e-2' parameter
resolveAst syms maybeParent addr (Ast_EMinus2 start expr) = do
  val <- evalExprWithSymbols syms maybeParent expr
  -- let relVal = (fromIntegral val :: Word32) - (fromIntegral addr :: Word32)

  -- TODO: These ranges need work. Are they before or after? What do we tell users on error?
  maskedVal <- rangeCheck (-128) 255 0xff (val - addr - 2) expr

  return $ fromIntegral $ shiftL maskedVal (32 - 8 - start)


-- | Checks if a value is within a min and max. If it is, applies a bitmask
--   to zero-out unwanted bits of negative numbers. If it isn't, reports an
--   error.
rangeCheck
  :: Int   -- minimum allowed value
  -> Int   -- maximum allowed value
  -> Int   -- bitmask to apply over final result
  -> Int   -- value to check
  -> Expr  -- source expression, for error attribution only
  -> AsmFailable Int

rangeCheck min max mask val expr
  | (val < min) || (val > max)
    = let (cs, cs', _) = expr in Left (CsRange cs cs', BoundsError val min max)
  | otherwise
    = Right (val .&. mask)
