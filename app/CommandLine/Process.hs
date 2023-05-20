module CommandLine.Process where


import CommandLine.FileIO  (convertDirTree, dirPred, emitError, formatterExec,
                            getWatchPath, isDir, moveTree, pathToLanguage,
                            readFileUtf8, reportFailure, toTargetLanguage,
                            watchPred, writeFileUtf8)
import CommandLine.Options (Opts (..))


import Control.Concurrent  (threadDelay)
import Control.Monad       (forever, join, when)
import Control.Monad.Extra (andM, whenM)
import Data.Foldable       (traverse_)
import Data.Text           (pack)
import Utils.Foldable      (wrapMaybe)
import Utils.Functor       ((<<$>>))


import System.Directory.Extra (createDirectoryIfMissing, doesDirectoryExist,
                               getHomeDirectory, removeDirectoryRecursive,
                               removeFile)
import System.Directory.Tree  (AnchoredDirTree (..), failures, filterDir,
                               readDirectoryWith, writeDirectoryWith, (</$>))
import System.FilePath        (equalFilePath, replaceFileName, takeDirectory,
                               takeFileName)
import System.FSNotify        (Action, Event (..), watchTree, withManager)
import System.Process.Extra   (callProcess, readProcess)

import Bookhound.Parser (ParseError)


process :: Opts -> FilePath -> IO ()
process opts@Opts{watchMode, sourcePath, targetPath} homeDir
  | any (equalFilePath homeDir) [sourcePath, targetPath] =
      fail "Neither source nor target paths can be the home directory"
  | isDir sourcePath && not (isDir targetPath) =
      fail "If the input path is a directory then the output path must be a directory as well"
  | equalFilePath sourcePath targetPath =
      fail "The output path cannot be the same as the input path"
  | watchMode                      = watchPath opts
  | null (takeFileName sourcePath)    = actions opts
  | otherwise                      = action emitError opts



action :: (ParseError -> IO ()) -> Opts -> IO ()
action errorAction Opts{language, sourcePath, targetPath, autoFormat} =
  readFileUtf8 sourcePath
  >>= (pack <<$>>) . traverse format . toTargetLanguage language
  >>= either errorAction createDirAndWriteFile

  where
    createDirAndWriteFile x = createDirectoryIfMissing True finalDir *>
                              writeFileUtf8 finalPath x
    finalDir                = takeDirectory finalPath
    finalPath               = pathToLanguage language targetPath'

    targetPath' = if isDir targetPath then
                    replaceFileName targetPath (takeFileName sourcePath)
                  else
                    targetPath

    format      = if autoFormat then
                    readProcess (formatterExec language)
                                ["--stdin", finalPath]
                  else
                    pure


actions :: Opts -> IO ()
actions Opts{language, sourcePath, targetPath, autoFormat, clearContents} =
  when clearContents (removeDirIfExists targetPath)
  >>= const (migrateDirTree sourcePath targetPath)
  >>= writeDirectoryWith writeFileUtf8
  >>= (traverse_ reportFailure) . failures . dirTree
  >>= const format

  where
    removeDirIfExists fp = whenM (removeDirPred fp)
                                 (removeDirectoryRecursive fp)
    removeDirPred fp = andM [doesDirectoryExist fp,
                             (/= fp) <$> getHomeDirectory]
    migrateDirTree fp1 fp2 = convertDirTree language
                             </$> (moveTree fp2
                              <$> (filterDir dirPred
                             </$>  readDirectoryWith readFileUtf8 fp1))
    format = when autoFormat $ callProcess (formatterExec language) [targetPath]



watchPath :: Opts -> IO ()
watchPath opts@Opts {sourcePath} =
  withManager (\mgr -> watchTree mgr dir pred' (watchAction opts)
                     *> putStrLn ("Watching on: " ++ sourcePath)
                     *> putStrLn ". . ."
                     *> loop)
  where
    dir = takeDirectory sourcePath
    fileName = takeFileName sourcePath
    pred' = watchPred $ wrapMaybe fileName
    loop = forever (threadDelay 1000000)


watchAction :: Opts -> Action
watchAction opts (Added fp _ _)    = transformAction fp opts
watchAction opts (Modified fp _ _) = transformAction fp opts
watchAction opts (Removed fp _ _)  = join $ removeFile <$> getWatchPath fp opts
watchAction _ _                    = pure ()


transformAction :: FilePath -> Opts -> IO ()
transformAction fp opts = join $ action (const $ pure ()) <$> newOpts
  where
    newOpts = (\x -> opts{sourcePath = fp, targetPath = x})
              <$> getWatchPath fp opts
