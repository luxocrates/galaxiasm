module Asm.Types.AsmFailable
  ( AsmFailable
  , AsmError
  , AsmErrorLoc (..)
  , AsmErrorDetails (..)
  )
where

import Asm.Types.CharStream (CharStream)


-- | A catch-all return class for functions that might want to report an
--   assembler-specific error
type AsmFailable = Either AsmError


-- | A fully-qualified error report. Its purpose is to keep all error reporting
--   abstract, for testing, and delegate the role of making errors human-
--   readable to a separate function.
type AsmError = (AsmErrorLoc, AsmErrorDetails)


-- | A record of where something went wrong
data AsmErrorLoc
  = Nowhere                        -- an unknown location (eg. command line)
  | CsPos CharStream               -- a specific point
  | CsRange CharStream CharStream  -- a range
  deriving Show


-- | An abstract report of what went wrong
data AsmErrorDetails
  = OtherError String         -- any generic error
  | LocalUseButNoParentError  -- attempt to reference a local label when there's no parent
  | LocalDefButNoParentError  -- attempt to define a local label when there's no parent
  | RedefLabelError String    -- attempt to re-define a label
  | RedefSymbolError String   -- attempt to re-define a symbol
  | UndefSymbolError String   -- attempt to reference an undefined symbol
  | UnavailSymbolError        -- attempt to reference a symbol in too early a pass
  | InvalidRstError           -- bad value for RST opcode
  | BoundsError               -- expression value out of bounds
      Int                     --   value
      Int                     --   minimum
      Int                     --   maximum
  | OrgTooSmallError          -- attempt for .org address to go backwards
      Int                     --   requested address
      Int                     --   current target
  | OrgTooBigError Int        -- attempt for .org address to go beyond memory
  | AlignTooBigError Int      -- attempt for .align address to go beyond memory
  | FillByteError Int         -- invalid value for filling memory in .org/.align
  | SyntaxError               -- syntax error
  | AddressError Int          -- attempt to emit bytes beyond end of memory
  | CsBalanceRedefError       -- attempt to re-define .checksumbalance position
  deriving Show
