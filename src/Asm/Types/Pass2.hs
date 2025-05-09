module Asm.Types.Pass2
  ( Ast (..)
  , Ast_IxIy (..)
  , Ast_PlusMinus (..)
  , Symbol
  , SymTable
  , SymbolType (Label, Equ)
  , Pass2Artifacts
  , UnresolvedBits
  , UnresolvedBitsInContext
  , OpcodeOrDirective
  )
where

import Data.Map (Map)
import Data.Word (Word32)

import Asm.Types.AsmFailable (AsmFailable)
import Asm.Types.Expr (Expr)
import Asm.Types.LineAndSource (LineAndSource)


-- | Assembler artifacts generated by the end of pass2, if successful
type Pass2Artifacts =
  ( SymTable                   -- symbol table
  , [UnresolvedBitsInContext]  -- all opcodes and directives
  , Maybe Int                  -- .checksumbalance position
  )

type SymTable = Map String Symbol
type Symbol = (Int, SymbolMeta)
type SymbolMeta = (SymbolType, LineAndSource)
data SymbolType = Label | Equ deriving (Eq, Show)


-- | A 1-, 2-, 3- or 4-byte bit sequence that comprises some known bits, and
--   some number of abstract syntax trees that can be resolved (once the symbols
--   are grounded) to a concrete bit sequence
type UnresolvedBits
  = ( Word32  -- base bit pattern (aligned left)
    , Int     -- number of bytes in bit pattern
    , [Ast]   -- ASTs that can be resolved to finalize the bit sequence
    )


-- | An unresolved bit sequence, plus contextual information that its resolution
--   might depend on, and location information for error reporting
type UnresolvedBitsInContext =
  ( Int             -- target address of bits in memory
  , Maybe String    -- parent label
  , UnresolvedBits  -- nascent bit sequence
  , LineAndSource   -- provenance
  )


-- | The artifact of a successful parsing of an opcode or a directive is a
--   function which, given a current address, yields an unresolved bit sequence
--   appropriate for that opcode/directive being at that address
type OpcodeOrDirective =
  Int ->                -- start address for this opcode/directive
  AsmFailable
    ( [UnresolvedBits]  -- resolvers to build final bytes
    , Bool              -- is this the checksum balance directive?
    )


-- | An abstract syntax tree that describes a group of bits within an
--   instruction. Bits are left-oriented, bit position 0 being the most
--   significant bit of the first byte.
data Ast
  = Ast_Const        -- a term with a fixed contribution to the bits
      Word32         --   bit pattern
  | Ast_B            -- a 3-bit expression
      Int            --   bit position
      Expr           --   numeric expression
  | Ast_N            -- an 8-bit expression
      Int            --   bit position
      Expr           --   numeric expression
  | Ast_PlusMinusD   -- +d/-d in ix+d, iy+d expressions
      Ast_PlusMinus  --   is plus offset or minus offset
      Int            --   bit position of numeric expression
      Expr           --   numeric expression
  | Ast_Nn           -- a 16-bit expression
      Int            --   bit position
      Expr           --   numeric expression
  | Ast_P            -- an expression for a 'p' value for RST
      Int            --   bit position
      Expr           --   numeric expression
  | Ast_EMinus2      -- an (e-2) expression
      Int            --   bit position
      Expr           --   numeric expression
  deriving (Eq, Show)

-- | An IX or IY term
data Ast_IxIy
  = Ast_Ix
  | Ast_Iy
  deriving (Eq, Show)

-- | The sign of the +d in an IX+d/IY+d term
data Ast_PlusMinus
  = Ast_Plus
  | Ast_Minus
  deriving (Eq, Show)
