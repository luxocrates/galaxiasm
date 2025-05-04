module Asm.Rom where

import Data.Word (Word8)
import CommandLine (RomFormat (..))

-- | Segments a binary stream of arbitrary length into batches for individual
--   ROM files, padded to fit
segment
  :: [Word8]     -- bytes to split
  -> RomFormat   -- output format
  -> String      -- base source filename
  -> [( String   -- filename for ROM
      , Int      -- starting address
      , Int      -- amount of padding added
      , [Word8]  -- ROM contents
     )]
segment w8s format basename = helper
                                0
                                (length w8s)
                                (sizeForFormat format)
                                w8s
                                (namesForFormat format basename)

namesForFormat :: RomFormat -> String -> [String]
namesForFormat Default _ = [ "galmidw.u"
                           , "galmidw.v"
                           , "galmidw.w"
                           , "galmidw.y"
                           , "7l"
                           ] ++ map (\x -> "extra_" ++ show x) [1..]

namesForFormat (Kb _) basename = basename
                                 : map
                                     (\x -> basename ++ "_extra_" ++ show x)
                                     [1..]

sizeForFormat :: RomFormat -> Int
sizeForFormat Default = 2048        -- assume 2716's
sizeForFormat (Kb k)  = k * 1024


helper
  :: Int         -- starting address
  -> Int         -- count of bytes remaining
  -> Int         -- capacity of each ROM
  -> [Word8]     -- bytes remaining
  -> [String]    -- filename generator
  -> [( String   -- output filename
      , Int      -- first address of output ROM
      , Int      -- amount of padding added
      , [Word8]  -- ROM contents
     )]
helper start rem cap w8s (name:names)
  | rem > cap = (name, start, 0, take cap w8s)
                : helper
                    (start + cap)
                    (rem - cap)
                    cap
                    (drop cap w8s)
                    names
  | otherwise = [( name
                  , start
                  , cap - (length w8s)
                  , w8s ++ replicate (cap - rem) 0
                )]

-- not actually reachable
helper _ _ _ _ [] = []
