module Main where

import Options.Applicative

import Lib
import Options

main :: IO ()
main = do
    opts <- execParser optionsParser
    generateTestbench opts
