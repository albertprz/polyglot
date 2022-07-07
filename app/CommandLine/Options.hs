module CommandLine.Options where

import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper,
                            info, long, progDesc, short, strOption, switch)


data Opts
  = Opts
      { sourcePath    :: FilePath
      , targetPath    :: FilePath
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
        <> help "Path of output Scala file or directory" )
    <*> switch
        ( long "format"
        <> short 'f'
        <> help "Apply Scala formatter (scalafmt) on output file(s)" )
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
           <> progDesc "Translate Haskell file(s) into Scala"
           <> header "haskala" )
