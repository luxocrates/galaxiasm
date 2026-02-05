module Test.Pass4 (test, rig) where

import Data.Word (Word8)

import Asm.Pass1 (pass1)
import Asm.Pass2 (pass2)
import Asm.Pass3 (pass3)
import Asm.Pass4 (pass4)
import Asm.Types.AsmFailable (AsmFailable)
import Asm.Types.Pass4 (Pass4Artifacts)


test :: Bool
test = rig ".db 2\n.checksumbalance" == Right [0x02, 0xfe]


rig :: String -> Either String [Word8]
rig program =
  case assembleToPass4 program of
    Left (_, details) -> Left $ show details
    Right (bytes, _)  -> Right bytes
  where
    assembleToPass4 :: String -> AsmFailable Pass4Artifacts
    assembleToPass4 program =
          pass1 program "(unit test)"
      >>= pass2
      >>= pass3
      >>= pass4
