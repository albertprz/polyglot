module CommandLine.Process where

import CommandLine.Options (Opts (..))
import Control.Concurrent
import Control.Monad       (forever)

import Lexers.Haskell.Layout     (adaptLayout)
import Parser                    (runParser)
import Parsers.Haskell.ModuleDef (moduleDef)
import System.FilePath           (dropFileName, takeDirectory, takeFileName,
                                  (-<.>), (</>))
import System.FSNotify
import Utils.Functor             ((<$$>))
import Utils.Monad               ((>>.))

import Parser (ParseError)


import qualified Conversions.HaskellToScala.ModuleDef as Conversions
import           System.Process.Extra                 (readProcess)



process :: Opts -> IO ()
process opts@Opts {watchMode}
  | watchMode = watchPath opts
  | otherwise = action opts emitError


action :: Opts -> (ParseError -> IO ()) -> IO ()
action Opts{sourcePath, targetPath, autoFormat} errorAction =
  readFile sourcePath >>=
  traverse format . (adaptLayout >>. convert) >>=
  either errorAction (writeFile targetPath')


  where
    format = if autoFormat then readProcess formatterPath ["--stdin", targetPath']
             else pure
    convert = (show . Conversions.moduleDef) <$$> runParser moduleDef
    setTargetFileName = (</> (takeFileName sourcePath)) . dropFileName
    targetPath' = maybe sourcePath setTargetFileName targetPath -<.> "scala"


watchPath :: Opts -> IO b
watchPath opts@Opts {sourcePath} =
  withManager (\mgr -> watchTree mgr dir pred' watchAction *>
                     forever (threadDelay 1000000))

  where
    dir = takeDirectory sourcePath
    fileName = takeFileName sourcePath
    pred' = watchPred $ wrapMaybe fileName
    watchAction = const $ action opts (const $ pure ())


watchPred :: Foldable t => t FilePath -> Event -> Bool
watchPred x (Added fp _ _)    = all (== (takeFileName fp)) x
watchPred x (Modified fp _ _) = all (== (takeFileName fp)) x
watchPred _ _                 = False


formatterPath :: FilePath
formatterPath = "scalafmt"


emitError :: ParseError -> IO ()
emitError = error . ("\n\n" ++) . show

wrapMaybe :: Foldable t => t a -> Maybe (t a)
wrapMaybe x = if (not . null) x then Just x else Nothing
