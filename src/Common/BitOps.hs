module Common.BitOps (byteSwap) where

import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))

-- | Swap the most-significant and least-significant bytes of a 16-bit word
byteSwap :: (Bits a, Num a) => a -> a
byteSwap x = (shiftL (x .&. 0xff) 8) .|. (shiftR x 8)
