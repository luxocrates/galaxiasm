import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStr, stderr)

import Test.CharStream   (test)
import Test.Disassemble  (test)
import Test.Parsers.Line (test)
import Test.Pass2        (test)
import Test.Pass3        (test)
import Test.Pass4        (test)


-- This is not a sophisticated test mechanism: if it fails, you'll have to
-- comment out tests until you've isolated the cause
run :: Bool
run = and
  [ True
  , Test.Disassemble.test
  , Test.CharStream.test
  , Test.Parsers.Line.test
  , Test.Pass2.test
  , Test.Pass3.test
  , Test.Pass4.test
  ]

main :: IO ()
main =
  if run
  then putStrLn "All unit tests passed"
  else do
    hPutStr stderr "Not all unit tests passed\n"
    exitWith $ ExitFailure 1
