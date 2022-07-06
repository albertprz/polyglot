module CommandLine.Process where


import           Lexers.Haskell.Layout     (adaptLayout)
import           Parser                    (ParseError, runParser)
import qualified Parsers.Haskell.ModuleDef as Parser (moduleDef)

import System.FilePath (equalFilePath, joinPath, replaceFileName,
                        splitDirectories, takeDirectory, takeExtensions,
                        takeFileName, (-<.>), (</>))
import System.FSNotify (Action, ActionPredicate, Event (..), watchTree,
                        withManager)

import CommandLine.Options (Opts (..))
import Control.Concurrent  (threadDelay)
import Control.Monad       (forever, when)
import Control.Monad.Extra (andM)
import Utils.Functor       ((<$$>))
import Utils.Monad         ((>>.))


import System.Directory.Tree (AnchoredDirTree (..), DirTree (..), failures,
                              readDirectory, writeDirectory, (</$>))


import           Control.Monad.Extra                  (whenM)
import qualified Conversions.HaskellToScala.ModuleDef as Conversions
import           Data.Foldable                        (foldl', traverse_)
import           Debug.Trace                          (trace)
import           System.Directory.Extra               (createDirectoryIfMissing,
                                                       doesDirectoryExist,
                                                       removeDirectoryRecursive,
                                                       removeFile, getHomeDirectory)
import           System.Process.Extra                 (callProcess, readProcess)



process :: Opts -> IO ()
process opts@Opts{watchMode, sourcePath, targetPath}
  | isDir sourcePath && not (isDir targetPath) =
      fail "If the input path is a directory then the output path must be a directory as well"
  | equalFilePath sourcePath targetPath =
      fail "The output path cannot be the same as the input path"
  | watchMode                      = trace "Watching..." $ watchPath opts
  | null (takeFileName sourcePath) = trace "Multiple actions" $ actions opts
  | otherwise                      = trace "Single action" $ action opts emitError


action :: Opts -> (ParseError -> IO ()) -> IO ()
action Opts{sourcePath, targetPath, autoFormat} errorAction =
  readFile sourcePath
  >>= traverse format . toScala
  >>= either errorAction createDirAndWriteFile

  where
    createDirAndWriteFile x = createDirectoryIfMissing True finalDir *>
                              writeFile finalPath x

    finalDir    = takeDirectory finalPath
    finalPath   = pathToScala targetPath'
    targetPath' = if isDir targetPath then
                    replaceFileName targetPath (takeFileName sourcePath)
                  else
                    targetPath

    format      = if autoFormat then
                    readProcess formatterExec ["--stdin", finalPath]
                  else
                    pure


actions :: Opts -> IO ()
actions Opts{sourcePath, targetPath, autoFormat, clearContents} =
  when clearContents (removeDirIfExists targetPath)
  >>= const (migrateDirTree sourcePath targetPath)
  >>= writeDirectory
  >>= (traverse_ reportFailure) . failures . dirTree
  >>= const format

  where
    removeDirIfExists fp = whenM (removeDirPred fp)
                                 (removeDirectoryRecursive fp)
    removeDirPred fp = andM [doesDirectoryExist fp,
                             (/= fp) <$> getHomeDirectory]
    migrateDirTree fp1 fp2 = convertDirTree
                             </$> (moveTree fp2 <$> readDirectory fp1)
    format = when autoFormat $ callProcess formatterExec [targetPath]

watchPath :: Opts -> IO b
watchPath opts@Opts {sourcePath} =
  withManager (\mgr -> watchTree mgr dir pred' (watchAction opts) *> loop)

  where
    dir = takeDirectory sourcePath
    fileName = takeFileName sourcePath
    pred' = watchPred $ wrapMaybe fileName
    loop = forever (threadDelay 1000000)


toScala :: String -> Either ParseError String
toScala =  adaptLayout >>. convert
  where
    convert = (show . Conversions.moduleDef) <$$> runParser Parser.moduleDef

pathToScala :: FilePath -> FilePath
pathToScala = (-<.> "scala")


convertDirTree :: DirTree String -> DirTree String
convertDirTree (File x y)
  | takeExtensions x `elem` [".hs", ".lhs"] =
    applyTransform y
    where
      applyTransform = either (Failed x . userError . const "Parse Error")
                              (File $ pathToScala x) .
                      toScala

convertDirTree (Dir x y) = Dir x (convertDirTree <$> y)
convertDirTree x = x


moveTree :: FilePath -> AnchoredDirTree a -> AnchoredDirTree a
moveTree path (x :/ y@(Dir _ _)) = trace (name newDirTree) $ x :/ newDirTree
  where
    newDirTree = foldr (\curr acc -> Dir curr [acc]) y dirs
    dirs = splitDirectories $ takeDirectory path

moveTree _ x                   = x



reportFailure :: DirTree a -> IO ()
reportFailure (Failed x y) = putStrLn $
  "Failure when converting file " ++ x ++ ": " ++ show y
reportFailure _ = pure ()


watchAction :: Opts -> Action
watchAction opts (Added fp _ _)    = transformAction fp opts
watchAction opts (Modified fp _ _) = transformAction fp opts
watchAction opts (Removed fp _ _)  = removeFile $ getWatchPath fp opts
watchAction _ _                    = pure ()


transformAction :: FilePath -> Opts -> IO ()
transformAction fp opts = action newOpts (const $ pure ())
  where
    newOpts = opts{sourcePath = fp, targetPath = getWatchPath fp opts}



watchPred :: Foldable t => t FilePath -> ActionPredicate
watchPred x (Added fp _ _)    = filePathPred fp x
watchPred x (Modified fp _ _) = filePathPred fp x
watchPred x (Removed fp _ _)  = filePathPred fp x
watchPred _ _                 = False


filePathPred :: Foldable t => FilePath -> t FilePath -> Bool
filePathPred fp x = extension `elem` [".hs", ".lhs"]
                    && all (== fileName) x
  where
    fileName = takeFileName fp
    extension = takeExtensions fp

getWatchPath :: FilePath -> Opts -> FilePath
getWatchPath fp Opts{sourcePath, targetPath} =
  maybe targetPath (targetPath </>) (wrapMaybe (fp `diffPath` sourcePath))
  where
    diffPath fp1 fp2 = drop (length fp2) fp1


isDir :: FilePath -> Bool
isDir = null . takeFileName


formatterExec :: FilePath
formatterExec = "scalafmt"

emitError :: ParseError -> IO ()
emitError = error . ("\n\n" ++) . show

wrapMaybe :: Foldable t => t a -> Maybe (t a)
wrapMaybe x = if (not . null) x then Just x else Nothing
