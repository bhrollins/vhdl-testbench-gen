module Options
    ( optionsParser
    , getOutputFile
    , Options(..)
    ) where

import System.FilePath (FilePath, dropExtension, takeExtension)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Options.Applicative


data Options = Options
    { inputFile :: FilePath
    , outputFile :: Maybe FilePath
    , forceWrite :: Bool
    }

getOutputFile :: Options -> FilePath
getOutputFile opts = fromMaybe fromInput (outputFile opts)
    where
        fromInput :: FilePath
        fromInput = let inputName = inputFile opts
                        baseName = dropExtension inputName
                        ext = takeExtension inputName
                    in
                        baseName ++ "_tb" ++ ext

optionsParser :: ParserInfo Options
optionsParser = 
    info 
        (helper <*> programOptions)
        (fullDesc <> progDesc "Simple VHDL testbench generator" 
                  <> header "testbench-gen - simple VHDL testbench generator")

programOptions :: Parser Options
programOptions = 
    Options 
        <$> parseString "input" "Path to top-level VHDL file"
        <*> optional (parseString "output" "Path to save generated testbench file to")
        <*> switch (long "force" <> short 'f' <> help "Force overwrite existing file")
    where
        toUpper' = map toUpper
        
        parseString :: String -> String -> Parser String
        parseString name strHelp = 
            strOption $  long name
                      <> short (head name)
                      <> metavar (toUpper' name)
                      <> help strHelp
        
