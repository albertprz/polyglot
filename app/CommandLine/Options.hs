module CommandLine.Options where

import Options.Applicative
import Data.List (intercalate)
import Data.Text (pack)
import Data.Either.Extra (mapLeft)

import qualified Bookhound.Parser as Bookhound
import Bookhound.ParserCombinators


data Language = Scala
  deriving (Eq, Ord, Enum, Show, Bounded)


data Opts
  = Opts
      { sourcePath    :: FilePath
      , targetPath    :: FilePath
      , language      :: Language
      , autoFormat    :: Bool
      , watchMode     :: Bool
      , clearContents :: Bool
      }

opts :: Parser Opts
opts = Opts
    <$> strOption
        ( long "input"
        <> short 'i'
        <> help "Path of input Haskell file or directory" )
    <*> strOption
        ( long "output"
        <> short 'o'
        <> help "Path of output file or directory" )
    <*> parserOption language
        ( long "language"
        <> short 'l'
        <> help "Target language")
    <*> switch
        ( long "format"
        <> short 'f'
        <> help "Apply formatter on output file(s)" )
    <*> switch
        ( long "watch"
        <> short 'w'
        <> help "Watch for changes and convert automatically" )
    <*> switch
        ( long "clear"
        <> help "Clear the output directory contents before conversion" )

optsInfo :: ParserInfo Opts
optsInfo = info (helper <*> opts)
           ( fullDesc
           <> progDesc "Compile Haskell file(s) into a target language. \n"
           <> footer ("Supported languages: "
            <> (intercalate ", " $ show <$> ([minBound .. maxBound] :: [Language])))
           <> header "polyglot" )


language :: Bookhound.Parser Language
language = Scala <$ is "Scala"


parserToReader :: Bookhound.Parser b -> String -> Either String b
parserToReader parser =
  mapLeft show . Bookhound.runParser parser . pack

parserOption :: Bookhound.Parser a -> Options.Applicative.Mod Options.Applicative.OptionFields a -> Parser a
parserOption parser =
  option $ eitherReader $ parserToReader parser
