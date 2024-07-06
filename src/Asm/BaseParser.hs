module Asm.BaseParser
  ( parseSuccessFromCs
  , isParseSuccess
  , isFurtherParseThan
  )
where

import Control.Monad (ap, liftM)

import Asm.CharStream (isFurtherThan)
import Asm.Types.CharStream (CharStream)
import Asm.Types.Parser (ParseAttempt (ParseError, ParseNearly, ParseSuccess))


-- Monad that allows Parser functions to be chained together to create another
-- Parser. `>>=` evaluation continues for as long as ParseSuccess is returned.
instance Monad ParseAttempt where
  (ParseSuccess csAndArts) >>= fn = fn csAndArts
  (ParseError err)         >>= _  = ParseError err
  (ParseNearly cs)         >>= _  = ParseNearly cs

instance Functor ParseAttempt where
  fmap = liftM

instance Applicative ParseAttempt where
  pure  = ParseSuccess
  (<*>) = ap


-- | Create a live ParseAttempt from a CharStream, as a base for chaining
parseSuccessFromCs :: CharStream -> ParseAttempt (CharStream, ())
parseSuccessFromCs cs = ParseSuccess (cs, ())


isParseSuccess :: ParseAttempt a -> Bool
isParseSuccess x = case x of
  (ParseSuccess _) -> True
  _                -> False


isFurtherParseThan :: ParseAttempt a -> ParseAttempt b -> Bool
isFurtherParseThan (ParseNearly aCs) (ParseNearly bCs)
  = aCs `isFurtherThan` bCs
isFurtherParseThan _ _
  = error "only use isFurtherParseThan with ParseNearly parse attempts"
