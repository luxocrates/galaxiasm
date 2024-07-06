module Asm.Assemble (assemble) where

import Asm.Pass1 (pass1)
import Asm.Pass2 (pass2)
import Asm.Pass3 (pass3)
import Asm.Pass4 (pass4)
import Asm.ShowError (showError)
import Asm.Types.Pass4 (Pass4Artifacts)


-- | A pure assembler: text in; Z80 bytes, or an error description, out
assemble
  :: String  -- the corpus of text to assemble
  -> String  -- the name of the source file, only for quoting in errors
  -> Either String Pass4Artifacts
assemble program filename
  = case result of
    Left err   -> Left $ showError err
    Right arts -> Right arts
  where
    result = pass1 program filename
         >>= pass2
         >>= pass3
         >>= pass4
