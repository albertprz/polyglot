# polyglot

## Description

CLI tool to transpile Haskell modules to several target languages.

The CLI can convert individual Haskell files as well as recursively
convert directory trees (or projects, for that matter).

There are a few options available to, for example, 
watch a file / directory and reactively convert it whenever modified,
as well as to format the output target language files.

At the moment, only parsing of Haskell 98 / 2010 standards along with a subset of GHC Syntax Extensions is supported (for example, there is currently no support for either Template Haskell or some of the GHC Syntax Extensions, such as GADTs and Type Families).

## Usage

```
Usage: polyglot (-l|--language ARG) (-i|--input ARG) (-o|--output ARG) 
               [-f|--format] [-w|--watch] [--clear]

  Compile Haskell file(s) into a target language.

Available options:
  -h,--help                Show this help text
  -l,--language ARG        Target language
  -i,--input ARG           Path of input Haskell file or directory
  -o,--output ARG          Path of output file or directory
  -f,--format              Apply formatter on output file(s)
  -w,--watch               Watch for changes and convert automatically
  --clear                  Clear the output directory contents before conversion

Supported languages: Scala
```

## Details

This CLI tool aims to perform a one-to-one mapping between Haskell and target language constructs.
This can be done in most cases, because all of the available target languages support many of Haskell key features that are not necessarily available in other mainstream languages, such as Higher Kinded Types, Typeclasses, GADTs & Higher Rank Polymorphism.

However, the conversion can be lossy, so some information can be lost in the process. At the same time, it can be necessary to provide some extra information in the target language version of the source file (most prominently function signatures, due to type inference).

The resulting files will have a dependency on some kind of prelude library that will expose all of the usual functions, data types, type classes and instances included in the Haskell prelude.

Also, bear in mind that in some cases due to different call semantics (lazy or call-by-need vs strict) and also runtime support for features (such as tail call optimization), the resulting files in the target language will probably need on some cases to be manually adapted post conversion, to preserve or approximate to the original Haskell code runtime characteristics.

In any case, it can be helpful to check the output files and manually adapt them as desired, because many Haskell idioms may not be the best match in the target language (This can be specially true for languages that are not in the ML family, such as Scala). 


## Examples


Sample Haskell snippet:

```haskell

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

```

Converted Scala output (after formatting):

```scala

def action(x: ParseError => IO[Unit])(y: Opts): IO[Unit] =
  (x, y) match
    case (errorAction, Opts(language, sourcePath, targetPath, autoFormat)) =>
      def createDirAndWriteFile =
        createDirectoryIfMissing(true)(finalDir) *> writeFileUtf8(finalPath)(x)
      def finalDir =
        takeDirectory(finalPath)
      def finalPath =
        pathToLanguage(language, targetPath$)
      def targetPath$ =
        if isDir(targetPath) then
          replaceFileName(targetPath)(takeFileName(sourcePath))
        else targetPath
      def format =
        if autoFormat then
          readProcess(formatterExec(language))(List("--stdin", finalPath))
        else pure

      readFileUtf8(sourcePath)
      >>= (pack <<&>> _) ^ traverse(format) ^ toTargetLanguage(language)
      >>= either(errorAction)(createDirAndWriteFile)

```

## Supported GHC Syntax Extensions

``` yaml

# Syntax Sugar
- LambdaCase
- MultiWayIf
- PostfixOperators

# Types
- RankNTypes
- ExplicitForAll
- ScopedTypeVariables

# Records
- DuplicateRecordFields
- NoFieldSelectors
- NamedFieldPuns
- RecordWildCards
- OverloadedRecordDot

# Type Classes
- ConstrainedClassMethods
- MultiParamTypeClasses

# Deriving 
- StandaloneDeriving
- DerivingVia
- NewTypeDeriving
- AnyClassDeriving
- DerivingStrategies

```
