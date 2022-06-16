module CommandLine.Options where

import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper,
                            info, long, optional, progDesc, short, strOption,
                            (<**>))


data Opts
  = Opts
      { sourcePath :: FilePath
      , targetPath :: Maybe FilePath
      }

opts :: Parser Opts
opts = Opts
    <$> strOption
        ( long "source"
        <> short 's'
        <> help "Path of input Haskell file" )
    <*> optional (strOption
        ( long "target"
        <> short 't'
        <> help "Path of output Scala file" ))

optsInfo :: ParserInfo Opts
optsInfo = info (opts <**> helper)
           ( fullDesc
           <> progDesc "Transpile a Haskell file into Scala"
           <> header "haskala" )
