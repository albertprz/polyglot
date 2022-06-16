module Main where

import CommandLine.Options (optsInfo)
import CommandLine.Process (process)
import Options.Applicative (execParser)


main :: IO ()
main = execParser optsInfo >>= process
