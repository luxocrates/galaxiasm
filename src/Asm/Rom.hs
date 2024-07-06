module Asm.Rom where

import Data.Word (Word8)

romSize = 2048  -- Assume 2716's

-- | Segments a binary stream of arbitrary length into batches for individual
--   ROM files, padded to fit
segment
  :: [Word8]     -- bytes to split
  -> [( String   -- filename for ROM
      , Int      -- starting address
      , Int      -- amount of padding added
      , [Word8]  -- ROM contents
     )]
segment w8s = helper 0 (length w8s) w8s names

names =
  [ "galmidw.u"
  , "galmidw.v"
  , "galmidw.w"
  , "galmidw.y"
  , "7l"
  ] ++ map (\x -> "extra_" ++ show x) [1..]

helper
  :: Int         -- starting address
  -> Int         -- count of bytes remaining
  -> [Word8]     -- bytes remaining
  -> [String]    -- filename generator
  -> [( String   -- output filename
      , Int      -- first address of output ROM
      , Int      -- amount of padding added
      , [Word8]  -- ROM contents
     )]
helper start rem w8s (name:names)
  | rem > romSize = (name, start, 0, take romSize w8s)
                    : helper
                        (start + romSize)
                        (rem - romSize)
                        (drop romSize w8s)
                        names
  | otherwise     = [( name
                     , start
                     , romSize - (length w8s)
                     , w8s ++ replicate (romSize - rem) 0
                    )]

-- not actually reachable
helper _ _ _ [] = []
