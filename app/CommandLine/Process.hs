module CommandLine.Process where


import Lexers.Haskell.Layout (adaptLayout)
import Parser                (ParseError, runParser)


import CommandLine.Options         (Opts (..))
import Control.Concurrent          (threadDelay)
import Control.Monad               (forever, join, when)
import Control.Monad.Extra         (andM, whenM)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Foldable               (traverse_)
import Data.List                   (isPrefixOf)
import Data.Tuple.Extra            (both)
import Utils.Functor               ((<<$>>))
import Utils.Monad                 ((>>.))


import System.Directory       (canonicalizePath)
import System.Directory.Extra (createDirectoryIfMissing, doesDirectoryExist,
                               getHomeDirectory, removeDirectoryRecursive,
                               removeFile)
import System.Directory.Tree  (AnchoredDirTree (..), DirTree (..), failures,
                               filterDir, readDirectory, writeDirectory, (</$>))
import System.FilePath        (equalFilePath, joinPath, normalise,
                               replaceFileName, splitDirectories, splitPath,
                               takeDirectory, takeExtensions, takeFileName,
                               (-<.>), (</>))
import System.FSNotify        (Action, ActionPredicate, Event (..), watchTree,
                               withManager)
import System.Process.Extra   (callProcess, readProcess)


import qualified Conversions.HaskellToScala.ModuleDef as Conversions
import qualified Parsers.Haskell.ModuleDef            as Parser





process :: Opts -> IO ()
process opts@Opts{watchMode, sourcePath, targetPath}
  | isDir sourcePath && not (isDir targetPath) =
      fail "If the input path is a directory then the output path must be a directory as well"
  | equalFilePath sourcePath targetPath =
      fail "The output path cannot be the same as the input path"
  | watchMode                      = watchPath opts
  | null (takeFileName sourcePath) = actions opts
  | otherwise                      = action emitError opts


action :: (ParseError -> IO ()) -> Opts -> IO ()
action errorAction Opts{sourcePath, targetPath, autoFormat} =
  readFile sourcePath
  >>= traverse format . toScala
  >>= either errorAction createDirAndWriteFile

  where
    createDirAndWriteFile x = createDirectoryIfMissing True finalDir *>
                              writeFile finalPath x
    finalDir                = takeDirectory finalPath
    finalPath               = pathToScala targetPath'

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
    migrateDirTree fp1 fp2 = convertDirTree . filterDir filterPred
                             </$> (moveTree fp1 fp2 <$> readDirectory fp1)
    format = when autoFormat $ callProcess formatterExec [targetPath]


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


toScala :: String -> Either ParseError String
toScala =  adaptLayout >>. convert
  where
    convert = show . Conversions.moduleDef <<$>> runParser Parser.moduleDef


convertDirTree :: DirTree String -> DirTree String
convertDirTree (File x y)
  | isHaskellFile x = applyTransform y
    where
      applyTransform = either (Failed x . userError . const "Parse Error")
                              (File $ pathToScala x)
                       . toScala

convertDirTree (Dir x y) = Dir x (parMap rseq convertDirTree y)
convertDirTree x = x


moveTree :: FilePath -> FilePath -> AnchoredDirTree a -> AnchoredDirTree a
moveTree fp1 fp2 (_ :/ x@(Dir _ _)) = "." :/ newDirTree
  where
    newDirTree    = foldr (\curr acc -> Dir curr [acc]) prunedDirTree
                                                      (init outputDirs)
    prunedDirTree = Dir (last outputDirs) (getDirTreeContents (length inputDirs) x)
    (inputDirs, outputDirs) = both (splitDirectories . takeDirectory . normalise)
                                   (fp1, fp2)
moveTree _ _ x                      = x



reportFailure :: DirTree a -> IO ()
reportFailure (Failed x y) = putStrLn $
  "Failure when converting file " ++ x ++ ": " ++ show y
reportFailure _ = pure ()


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



watchPred :: Foldable t => t FilePath -> ActionPredicate
watchPred x (Added fp _ _)    = filePathPred fp x
watchPred x (Modified fp _ _) = filePathPred fp x
watchPred x (Removed fp _ _)  = filePathPred fp x
watchPred _ _                 = False


filePathPred :: Foldable t => FilePath -> t FilePath -> Bool
filePathPred fp x = isHaskellFile fp
                    && all (== fileName) x
  where
    fileName = takeFileName fp

getWatchPath :: FilePath -> Opts -> IO FilePath
getWatchPath fp Opts{sourcePath, targetPath} =
  maybe targetPath (targetPath </>) . wrapMaybe <$> prunedPath
  where
    prunedPath          = diffPath <$> traverse canonicalizePath (fp, sourcePath)
    diffPath (fp1, fp2) = joinPath $ drop (length $ splitPath fp2)
                                          (splitPath fp1)


getDirTreeContents :: Int -> DirTree a -> [DirTree a]
getDirTreeContents 0 x         = [x]
getDirTreeContents n (Dir _ x) =  x  >>= getDirTreeContents (n - 1)
getDirTreeContents _ x         = [x]

filterPred :: DirTree a -> Bool
filterPred (Dir x _) = (not . isPrefixOf ".")  x
filterPred _         = True


isDir :: FilePath -> Bool
isDir = null . takeFileName

isHaskellFile :: FilePath -> Bool
isHaskellFile = (`elem` [".hs", ".lhs"]) . takeExtensions

pathToScala :: FilePath -> FilePath
pathToScala = (-<.> "scala")

formatterExec :: FilePath
formatterExec = "scalafmt"

emitError :: ParseError -> IO ()
emitError = fail . ("\n\n" ++) . take 50 . show

wrapMaybe :: Foldable t => t a -> Maybe (t a)
wrapMaybe x = if (not . null) x then Just x else Nothing
