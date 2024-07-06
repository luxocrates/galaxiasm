module Asm.Types.Parser
  ( ParseAttempt (ParseSuccess, ParseNearly, ParseError)
  , Parser
  )
where

import Asm.Types.AsmFailable (AsmError)
import Asm.Types.CharStream (CharStream)

-- | The result of an attempt to run a parser on a CharStream. It can retain
--   an artifact of arbitrary type on success, which would usually be a tuple
--   containing the post-success CharStream
data ParseAttempt
  a                          -- Parse-specific artifacts
  = ParseSuccess a           -- Post-parse stream and artifacts
  | ParseNearly CharStream   -- A highwater mark of the closest parse attempt
  | ParseError AsmError      -- Parsing proceeded enough to be certain that this
                             -- was the correct parser for the input, but the
                             -- input was bad
  deriving Show


-- | A function that turns a CharStream into a ParseAttempt.
--   If the parsing is successful, yields a new CharStream to allow chaining,
--   and a parser-specific artifact
type Parser a
  = CharStream     -- input stream
  -> ParseAttempt
     ( CharStream  -- a CharStream, advanced to the first character after the
                   -- string being parsed
     , a           -- data specific to the parsed text
     )
