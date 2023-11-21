module Main where

import ClassyPrelude

import CommandLine.Options (optsInfo)
import CommandLine.Process (process)
import Options.Applicative (execParser)
import System.Directory    (getHomeDirectory)


main :: IO ()
main = join $ process <$> execParser optsInfo <*> getHomeDirectory
