module Asm.Assemble (assemble) where

import Asm.Pass1 (pass1)
import Asm.Pass2 (pass2)
import Asm.Pass3 (pass3)
import Asm.Pass4 (pass4)
import Asm.ShowError (showError)

import Asm.Types.LineAndSource (LineAndSource)
import Asm.Types.Pass4 (Pass4Artifacts)

-- | A pure assembler: source lines in; Z80 bytes, or an error description, out
assemble
  :: [LineAndSource]  -- the corpus of text to assemble
  -> Either String Pass4Artifacts
assemble attribLines
  = case result of
    Left err   -> Left $ showError err
    Right arts -> Right arts
  where
    result = pass1 attribLines
         >>= pass2
         >>= pass3
         >>= pass4
