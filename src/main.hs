-- Top-level function for executable, and the only non-pure support functions
-- for the dis/assembler.

import Data.ByteString (pack, readFile, unpack, writeFile)
import System.Environment
import System.IO (hPutStr, readFile, stderr)

import qualified Asm.Assemble (assemble)
import Asm.Rom (segment)
import CommandLine
  ( CliConfig (Assemble, Disassemble, ShowHelp)
  , parseCommandLine
  , showHelp
  )
import Common.String (showHex4)
import qualified Disasm.Disassemble (disassemble)
import System.Directory (createDirectoryIfMissing)


outDirName :: String
outDirName = "out"


-- | Load a file, run it through the pure disassembler, print its output
disassemble
  :: String  -- input filename
  -> IO ()   -- IO for stdout printing
disassemble filename = do
  -- TODO: catch file read errors
  contents <- Data.ByteString.readFile filename
  putStrLn $ Disasm.Disassemble.disassemble $ unpack contents


-- | Load a file, run it through the pure assembler, emit its output, or print
--   errors
assemble
  :: String   -- input filename
  -> IO [()]  -- IO for file writes and stdout/stderr printing
assemble inFilename = do
  -- TODO: catch file read errors
  contents <- System.IO.readFile inFilename
  case Asm.Assemble.assemble contents inFilename of
    Left err
      -> sequence [hPutStr stderr $ err]
    Right arts
      -> do
        let (bs, _) = arts
        createDirectoryIfMissing False outDirName
        sequence $
          concatMap (\(romName, start, padding, w8s) ->
            let filename = outDirName ++ "/" ++ romName in
            [ Data.ByteString.writeFile filename (pack w8s)
            , putStrLn $
              "Wrote " ++ showHex4 start
              ++ "-" ++ showHex4 (start + length w8s)
              ++ " to " ++ filename
              ++ (
                if padding > 0
                  then " (with " ++ show padding ++ " bytes padding)"
                  else ""
                )
            ]) (segment bs)


-- | Main entrypoint for command-line executable
main :: IO [()]
main = do
  argv <- getArgs
  case parseCommandLine argv of
    Left err                     -> sequence [hPutStr stderr $ err ++ "\n"]
    Right (Disassemble filename) -> sequence [disassemble filename]
    Right (Assemble filename)    -> assemble filename
    Right ShowHelp               -> sequence [putStr showHelp]
