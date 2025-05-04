module Asm.Types.LineAndSource (LineAndSource) where

type LineAndSource =
  ( String    -- line text
  , ( String  -- filename
    , Int     -- line number
    )
  )
