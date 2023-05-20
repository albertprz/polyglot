module Main where

import CommandLine.Options (optsInfo)
import CommandLine.Process (process)
import Control.Monad       (join)
import Options.Applicative (execParser)
import System.Directory    (getHomeDirectory)


main :: IO ()
main = join $ process <$> execParser optsInfo <*> getHomeDirectory
