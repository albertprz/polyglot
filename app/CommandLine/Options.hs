module CommandLine.Options where

import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper,
                            info, long, optional, progDesc, short, strOption,
                            switch, (<**>))


data Opts
  = Opts
      { sourcePath :: FilePath
      , targetPath :: Maybe FilePath
      , watchMode  :: Bool
      , autoFormat :: Bool
      }

opts :: Parser Opts
opts = Opts
    <$> strOption
        ( long "source"
        <> short 's'
        <> help "Path of input Haskell file (required)" )
    <*> optional (strOption
        ( long "target"
        <> short 't'
        <> help "Path of output Scala file" ))
    <*> switch
        ( long "watch"
        <> short 'w'
        <> help "Watch for changes and convert automatically" )
    <*> switch
        ( long "format"
        <> short 'f'
        <> help "Apply formatter on target file" )

optsInfo :: ParserInfo Opts
optsInfo = info (opts <**> helper)
           ( fullDesc
           <> progDesc "Transpile a Haskell file into Scala"
           <> header "haskala" )
