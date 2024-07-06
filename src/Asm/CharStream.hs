module Asm.CharStream (confine, isFurtherThan, makeCs, takeChar) where

import Asm.Types.CharStream (CharStream (CS), Source)


-- | Creates a CharStream from a string and audit information
makeCs
  :: String      -- text of line
  -> Source      -- file name and line number
  -> CharStream

makeCs text source =
  CS
    text
    0
    (length text)
    source


-- | Returns the next character of a CharStream, if any are available
takeChar
  :: CharStream   -- input CharStream
  ->
    ( CharStream  -- CharStream after take
    , Maybe Char  -- taken char, if not past limit
    )

takeChar cs =
  let (CS text cursor limit source) = cs in
    if
      cursor == limit
    then
      (cs, Nothing)
    else
      ( CS text (cursor + 1) limit source
      , Just (text !! cursor)
      )


-- | Returns True if the first CharStream's cursor is beyond the second's.
isFurtherThan :: CharStream -> CharStream -> Bool
isFurtherThan a b
  = csPos a > csPos b
  where csPos (CS _ cursor _ _) = cursor


-- | Creates a CharStream from an existing one, but with a premature end.
--   The original text is remembered, for auditing and printing out in error
--   descriptions.
confine
  :: CharStream  -- source
  -> Int         -- number of chars after source's cursor to allow
  -> CharStream  -- confined CharStream

confine (CS text cursor limit source) runway
  | limit' > limit = error "CharStream.confine seeks to expand its source"
  | otherwise      = CS text cursor limit' source
  where
    limit' = cursor + runway
