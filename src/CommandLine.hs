module CommandLine ( CliConfig (..), parseCommandLine, showHelp) where

import Data.List (intercalate)
import Data.Maybe (isJust, fromMaybe)


defaultFilename = "asm/galaxian.s"

data CliConfig
  = Assemble
      String     -- filename
      Bool       -- emit symbols file
  | Disassemble
      String     -- filename
  | ShowHelp

-- | Parses the command line arguments
parseCommandLine :: [String] -> Either String CliConfig
parseCommandLine argv = do
  acc <- iter argv startingAcc

  return $ case accAction acc of
    AccShowHelp    -> ShowHelp
    AccAssemble    -> Assemble (filename acc) (accEmitSyms acc)
    AccDisassemble -> Disassemble $ filename acc

  where
    filename acc = fromMaybe defaultFilename (accFilename acc)


data CliConfigAcc =
  CliConfigAcc
    { accAction   :: AccAction
    , accFilename :: Maybe String
    , accEmitSyms :: Bool
    }

data AccAction
  = AccAssemble
  | AccDisassemble
  | AccShowHelp
  deriving (Eq, Show)

startingAcc :: CliConfigAcc
startingAcc =
  CliConfigAcc
    { accAction   = AccAssemble
    , accFilename = Nothing
    , accEmitSyms = False
    }


-- | Update the options accumulator based on the next single token from the
--   CLI args, or yield an error
iter :: [String] -> CliConfigAcc -> Either String CliConfigAcc
iter [] config = Right config

iter ("-d":t) config = iter t $ config { accAction = AccDisassemble }

iter (token:t) config
  | (token == "-h") || (token == "-help") || (token == "--help")
    = iter t $ config { accAction = AccShowHelp }
    -- `-h -d` would ignore the -h, but what should we really have done?
  | token == "-s"
    = iter t $ config { accEmitSyms = True }
    -- ought to bark if -s used with -d

iter (filename:t) config =
  if isJust $ accFilename config
    then Left "Only one file can be used as input"
    else iter t $ config { accFilename = Just filename }


showHelp :: String
showHelp = intercalate "\n"
  ( "Usage:"
  : map ("  " ++)
    [ "(no args)      build asm/galaxian.s to out/"
    , "(filename)     assemble (filename) to out/"
    , "-s             emit symbols file (when assembling)"
    , "-d (filename)  disassemble (filename) to stdout"
    , "-help          show this usage info"
    ]
  ) ++ "\n"
