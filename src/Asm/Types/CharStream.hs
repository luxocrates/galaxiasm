module Asm.Types.CharStream (Source, CharStream (CS)) where

-- | Auditing information for a CharStream's provenance
type Source =
  ( String  -- file name
  , Int     -- line number (1-indexed)
  )

-- | A character stream which preserves the original string and the current
--   position within it
data CharStream =
  CS
    String  -- full line from Source
    Int     -- cursor position (0-indexed)
    Int     -- limit (index of first char excluded)
    Source  -- provenance
  deriving (Eq, Show)
