module Common.String (showHex2, showHex4, padStart, padEnd) where

import Numeric (showHex)


-- | Display a number as $xx
showHex2 :: Integral a => a -> [Char]
showHex2 x = "$" ++ padStart 2 '0' (showHex x "")

-- | Display a number as $xxxx
showHex4 :: Integral a => a -> [Char]
showHex4 x = "$" ++ padStart 4 '0' (showHex x "")

-- | Pad the start of a string with a filler char until it's a fixed length
padStart :: Int -> Char -> String -> String
padStart len filler str
  = replicate (len - length str) filler
    ++ str

-- | Pad the end of a string with a filler char until it's a fixed length.
--   Crops if the padded length is less than the source length.
padEnd :: Int -> Char -> String -> String
padEnd len filler str
  = take len (str ++ repeat filler)
