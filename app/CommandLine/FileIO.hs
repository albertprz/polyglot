module CommandLine.FileIO where

import CommandLine.Options (Language (..), Opts (..))

import Lexers.Haskell.Layout (adaptLayout)

import Control.Monad               ((>=>))
import Control.Parallel.Strategies (parMap, rseq)
import Data.List                   (isPrefixOf)
import Data.Text                   (Text, pack)
import Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import Data.Tuple.Extra            (both)
import Utils.Foldable              (wrapMaybe)
import Utils.Functor               ((<<$>>))


import System.Directory      (canonicalizePath)
import System.Directory.Tree (AnchoredDirTree (..), DirTree (..))
import System.FilePath       (joinPath, normalise, splitDirectories, splitPath,
                              takeDirectory, takeExtensions, takeFileName,
                              (-<.>), (</>))
import System.FSNotify       (ActionPredicate, Event (..))


import qualified Data.ByteString as B (readFile, writeFile)

import qualified Conversions.ToPurescript.ModuleDef as ToPurescript
import qualified Conversions.ToScala.ModuleDef      as ToScala
import qualified Parsers.Haskell.ModuleDef          as Parser

import Bookhound.Parser (ParseError, runParser)


toTargetLanguage :: Language -> Text -> Either ParseError String
toTargetLanguage language = adaptLayout >=> convert
  where
    convert = syntaxConverter <<$>> runParser Parser.moduleDef
    syntaxConverter = case language of
      Scala      -> show . ToScala.moduleDef
      Purescript -> show . ToPurescript.moduleDef


convertDirTree :: Language -> DirTree Text -> DirTree Text
convertDirTree language (File x y)
  | isHaskellFile x = applyTransform y
    where
      applyTransform = either (Failed x . userError . const "Parse Error")
                              (File $ pathToLanguage language x)
                       . (pack <$>) . toTargetLanguage language

convertDirTree language (Dir x y) = Dir x (parMap rseq (convertDirTree language) y)
convertDirTree _ x = x


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

pathToLanguage :: Language -> FilePath -> FilePath
pathToLanguage language = (-<.> extension)
  where
    extension = case language of
      Scala      -> "scala"
      Purescript -> "purs"

formatterExec :: Language -> FilePath
formatterExec Scala      = "scalafmt"
formatterExec Purescript = "purs-tidy"

emitError :: ParseError -> IO ()
emitError = fail . ("\n\n" ++) . take 50 . show


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = decodeUtf8 <$> B.readFile fp

writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 fp = B.writeFile fp . encodeUtf8
