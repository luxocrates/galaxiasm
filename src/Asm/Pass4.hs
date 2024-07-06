module Asm.Pass4 (pass4) where

import Asm.Types.AsmFailable (AsmFailable)
import Asm.Types.Pass3 (Pass3Artifacts)
import Asm.Types.Pass4 (Pass4Artifacts)


-- | Pass 4: if a .checksumbalance was present in the source, change a byte at
--   the corresponding offset to make the checksum for the whole file equal zero
pass4 :: Pass3Artifacts -> AsmFailable Pass4Artifacts
pass4 pass3Artifacts = Right (finalBytes, pass3Artifacts) where
  finalBytes =
    let
      (bytes, pass2Artifacts) = pass3Artifacts
      (_, _, csBalancePos)    = pass2Artifacts
    in
      case csBalancePos of
        Nothing -> bytes
        Just pos ->
          take pos bytes
          ++ [-(sum bytes)]
          ++ drop (pos + 1) bytes
