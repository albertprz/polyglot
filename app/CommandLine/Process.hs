module CommandLine.Process where

import CommandLine.Options   (Opts (..))
import Data.Maybe            (fromMaybe)
import Lexers.Haskell.Layout (adaptLayout)
import System.FilePath       (replaceBaseName, takeBaseName)


process :: Opts -> IO ()
process Opts {sourcePath, targetPath} = readFile sourcePath >>=
                                        emitError . adaptLayout >>=
                                        writeFile targetPath'

  where
    targetName' = takeBaseName sourcePath ++ "_output"
    targetPath' = fromMaybe (replaceBaseName sourcePath targetName') targetPath


emitError :: Show a => Either a b -> IO b
emitError x = either (error . ("\n\n" ++) . show) pure x
