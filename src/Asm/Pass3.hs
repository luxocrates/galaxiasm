module Asm.Pass3 (pass3) where

import Control.Monad (foldM)
import Data.Word (Word8)

import Asm.ResolveOpcode (resolveOpcode)
import Asm.Types.AsmFailable (AsmFailable)
import Asm.Types.Pass2 (Pass2Artifacts, SymTable, UnresolvedBitsInContext)
import Asm.Types.Pass3 (Pass3Artifacts)


-- | Pass 3: produce an output byte array by applying the final symbol table to
--   the unresolved bits generated in pass 2
pass3 :: Pass2Artifacts -> AsmFailable Pass3Artifacts
pass3 pass2Artifacts =
  case
    foldM (folder symbols) [] unresolved
  of
    Left err    -> Left err
    Right bytes -> Right (bytes, pass2Artifacts)
  where
    (symbols, unresolved, _) = pass2Artifacts

folder :: SymTable -> [Word8] -> UnresolvedBitsInContext -> AsmFailable [Word8]
folder symbols bytes unresolved
  = do
      newBytes <- resolveOpcode unresolved symbols
      -- There's got to be less expensive ways of doing this...
      return $ bytes ++ newBytes
