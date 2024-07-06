-- Functions for building parsers, out of functions or other parsers
module Asm.Parsers.Meta
  ( parseAny
  , parseMaybe
  , parseKeyword
  , peekWhile
  , parseKeywordMap
  )
where

import Data.Char (toLower)

import Asm.BaseParser (isFurtherParseThan, isParseSuccess)
import Asm.CharStream (takeChar)
import Asm.Types.CharStream (CharStream)
import Asm.Types.Parser
  ( ParseAttempt (ParseError, ParseNearly, ParseSuccess)
  , Parser
  )


-- | Peekers: like parsers, but always succeed; just maybe with an empty string
type Peeker = CharStream -> (CharStream, String)


-- | Takes a list of parsers. Succeeds if any of that list succeeds, with the
--   results from the first successful match
parseAny :: [Parser a] -> Parser a
parseAny parsers cs =
  parseAny'
    (ParseNearly cs)
    parsers
    cs
  where
    parseAny'
      :: ParseAttempt (CharStream, a)
      -> [Parser a]
      -> CharStream
      -> ParseAttempt (CharStream, a)

    parseAny' bestYet [] _ = bestYet

    parseAny' bestYet (parser:nextParser) cs =
      let attempt = parser cs in
      case attempt of
        ParseSuccess _ -> attempt
        ParseError   _ -> attempt
        ParseNearly  _ ->
          let
            nextBestYet =
              if (attempt `isFurtherParseThan` bestYet)
                then attempt
                else bestYet
          in
            parseAny' nextBestYet nextParser cs


-- | A parser that always succeeds. It takes a parser as argument.
--   If the supplied parser succeeds, succeeds with a Just of its artifact.
--   If the supplied parser fails, succeeds with a Nothing artifact.
parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe parser cs = case (parser cs) of
  x | isParseSuccess x -> let (ParseSuccess (cs', art)) = x
                          in ParseSuccess (cs', (Just art))
    | otherwise        -> ParseSuccess (cs, Nothing)


-- | Takes a function that decides whether a character is valid. Returns
--   a peeker that advances a CharStream up to the point of the first invalid
--   character.
peekWhile :: (Char -> Bool) -> Peeker
peekWhile test cs =
  case takeChar cs of
    (cs', Just char) | (test char) -> (csFinal, char:next) where
                                        (csFinal, next) = (peekWhile test cs')
    _                              -> (cs, "")


-- | Matches a case-insensitive string, returning an arbitrary value if matched;
--   otherwise a ParseNearly showing the last character to have matched
parseKeyword :: String -> a -> Parser a
parseKeyword []        a haystack = ParseSuccess (haystack, a)
parseKeyword (n:eedle) a haystack =
  case takeChar haystack of
    (haystack', Just char) | toLower char == toLower n
      -> parseKeyword eedle a haystack'
    _
      -> ParseNearly haystack


-- | Takes a list of mappings from strings (case-insensitive) to parser return
--   values
parseKeywordMap :: [(String, a)] -> Parser a
parseKeywordMap mapping = parseAny (map makeKeywordParser mapping)
  where
    makeKeywordParser :: (String, a) -> Parser a
    makeKeywordParser (keyword, value) = parseKeyword keyword value
