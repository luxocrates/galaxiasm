module Test.CharStream (test) where

import Asm.CharStream (confine, isFurtherThan, makeCs, takeChar)
import Asm.Types.CharStream (CharStream (CS))

test :: Bool
test = and
  [
    -- Creating a CharStream
    makeCs "text" ("filename", 1)
    == CS "text" 0 4 ("filename", 1)
  ,
    -- Taking the head character
    takeChar
      (CS "t" 0 1 ("filename", 1))
    ==
      ( (CS "t" 1 1 ("filename", 1))
      , Just 't'
      )
  ,
    -- Being denied taking past the limit
    takeChar
      (CS "t" 1 1 ("filename", 1))
    ==
      ((CS "t" 1 1 ("filename", 1))
      , Nothing
      )
  ,
    -- Finding the furthest-advanced CharStream
    isFurtherThan
      (CS "corpus" 1 5 ("", 1))
      (CS "corpus" 3 5 ("", 1))
    ==
      False
  ,
    isFurtherThan
      (CS "corpus" 4 5 ("", 1))
      (CS "corpus" 2 5 ("", 1))
    ==
      True
  ,
    -- Limiting the scope of a CharStream
    confine
      (CS "corpus" 1 5 ("", 1))
      3
    ==
      CS "corpus" 1 4 ("", 1)
  ,
    -- Takes it right to the limit
    -- (Blowing past the limit causes an error. You'll have to check that case
    -- by hand.)
    confine
      (CS "c" 1 1 ("", 1))
      0
    ==
      (CS "c" 1 1 ("", 1))
  ]
