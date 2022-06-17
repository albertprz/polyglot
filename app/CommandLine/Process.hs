module CommandLine.Process where

import CommandLine.Options       (Opts (..))
import Lexers.Haskell.Layout     (adaptLayout)
import Parser                    (runParser)
import Parsers.Haskell.ModuleDef (moduleDef)
import System.FilePath           (dropFileName, takeFileName, (-<.>), (</>))
import Utils.Functor             ((<$$>))
import Utils.Monad               ((>>.))

import qualified Conversions.HaskellToScala.ModuleDef as Conversions


process :: Opts -> IO ()
process Opts {sourcePath, targetPath} =
  readFile sourcePath >>=
  emitError . (adaptLayout >>. convert) >>=
  writeFile targetPath'

  where
    convert = (show
               . Conversions.moduleDef)
              <$$> runParser moduleDef
    setTargetFileName = (</> (takeFileName sourcePath)) . dropFileName
    targetPath' = maybe sourcePath setTargetFileName targetPath -<.> "scala"


emitError :: Show a => Either a b -> IO b
emitError x = either (error . ("\n\n" ++) . show) pure x
