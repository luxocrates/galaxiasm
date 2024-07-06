-- Functions for building parsers that selectively accumulate artifacts from
-- other parsers
module Asm.ArtifactAccumulator
  ( use, ___
  , used0, used1, used2, used3
  , wrangle0, wrangle1, wrangle2, wrangle3
  )
where

import Asm.Types.CharStream (CharStream)
import Asm.Types.Parser
  ( ParseAttempt (ParseError, ParseNearly, ParseSuccess)
  , Parser
  )


-- | Runs a parser and, if successful, adds its artifact to the accumulation.
-- | Can be chained via `>>=`.
use
  :: Parser a                           -- next parser function
  -> (CharStream, b)                    -- previous successful link
  -> ParseAttempt (CharStream, (b, a))

use fn (cs, b) = case fn cs of
  ParseSuccess (cs', a) -> ParseSuccess (cs', (b, a))
  ParseNearly cs'       -> ParseNearly cs'
  ParseError err        -> ParseError err


-- | Runs a parser and discards its artifact.
-- | Can be chained via `>>=`.
___
  :: Parser a                      -- next parser function
  -> (CharStream, b)               -- previous successful link
  -> ParseAttempt (CharStream, b)

___ fn (cs, b) = case fn cs of
  ParseSuccess (cs', _) -> ParseSuccess (cs', b)
  ParseNearly cs'       -> ParseNearly cs'
  ParseError err        -> ParseError err


-- wrangle* functions convert an accumulation of artifacts into a fixed-size
-- tuple

wrangle0 ::    ()             -> ()
wrangle1 ::   ((), a)         -> (a)
wrangle2 ::  (((), a), b)     -> (a, b)
wrangle3 :: ((((), a), b), c) -> (a, b, c)

wrangle0     ()             = ()
wrangle1    ((), a)         = (a)
wrangle2   (((), a), b)     = (a, b)
wrangle3  ((((), a), b), c) = (a, b, c)


-- used* functions wrangle an accumulation of arguments into a tuple, which is
-- transformed by an emitter function to become a (successful) ParseAttempt with
-- new artifacts

used0 :: (        () -> x) -> (cs,    ()             ) -> ParseAttempt (cs, x)
used1 :: (       (a) -> x) -> (cs,   ((), a)         ) -> ParseAttempt (cs, x)
used2 :: (    (a, b) -> x) -> (cs,  (((), a), b)     ) -> ParseAttempt (cs, x)
used3 :: ( (a, b, c) -> x) -> (cs, ((((), a), b), c) ) -> ParseAttempt (cs, x)

used0 emitter (cs, arts) = ParseSuccess (cs, emitter $ wrangle0 arts)
used1 emitter (cs, arts) = ParseSuccess (cs, emitter $ wrangle1 arts)
used2 emitter (cs, arts) = ParseSuccess (cs, emitter $ wrangle2 arts)
used3 emitter (cs, arts) = ParseSuccess (cs, emitter $ wrangle3 arts)
