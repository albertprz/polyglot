# haskala

## Description

CLI tool to partially transpile Haskell modules to Scala 3 packages.

The CLI can convert individual Haskell files as well as recursively
convert directory trees (or projects for that matter).

There are a few options available to, for example, 
watch a file / directory and reactively convert it whenever modified,
as well as to format the output Scala files.

This is still WIP. At the moment, parsing of Haskell 98 / 2010 along with a limited subset of GHC Extensions is supported (for example there is currently no support for Template Haskell).


## Details

This CLI tool aims to perform a one-to-one mapping between Haskell and Scala 3 constructs.
This can be done in most cases, because Scala 3 supports many of Haskell key features that are not necessarily available in other mainstream languages, such as Higher Kinded Types, Typeclasses, (G)ADTs & Higher Rank Polymorphism.

However, the conversion can be lossy, so some information can be lost in the process. At the same time, it can be necessary to provide some extra information in the Scala version (most prominently function signatures, due to type inference needs).

The resulting Scala files will have a dependency on at least Cats for the base typeclasses and Cats Effect for a lazy effect monad, as well as on some kind of prelude Scala library.

Also, bear in mind that due to different semantics (call-by-need vs strict) and also runtime characteristics (such as stack safety), the resulting Scala files will probably need on most cases to be manually adapted post conversion, to preserve the original semantics and guarantee there will be no errors at runtime.

In any case, it can be helpful to check the output Scala files and manually adapt them as desired, because many Haskell idioms may not be the best match in Scala. 

## Example


Sample Haskell snippet:

```haskell

action :: (ParseError -> IO ()) -> Opts -> IO ()
action errorAction Opts{sourcePath, targetPath, autoFormat} =
  readFileUtf8 sourcePath
  >>= (pack <<$>>) . traverse format . toScala
  >>= either errorAction createDirAndWriteFile

  where
    createDirAndWriteFile x = createDirectoryIfMissing True finalDir *>
                              writeFileUtf8 finalPath x
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

```

Converted Scala output (after formatting):

```scala

def action(x: ParseError => IO[Unit])(y: Opts): IO[Unit] =
  (x, y) match
    case (errorAction, Opts(sourcePath, targetPath, autoFormat)) =>
      def createDirAndWriteFile =
        createDirectoryIfMissing(true, finalDir) *> writeFileUtf8(finalPath, x)
      def finalDir =
        takeDirectory(finalPath)
      def finalPath =
        pathToScala(targetPath$)
      def targetPath$ =
        if isDir(targetPath) then
          replaceFileName(targetPath, takeFileName(sourcePath))
        else targetPath
      def format =
        if autoFormat then
          readProcess(formatterExec, List("--stdin", finalPath))
        else pure

      readFileUtf8(sourcePath)
      >>= (pack <<&>> _) ^ traverse(format) ^ toScala
      >>= either(errorAction, createDirAndWriteFile)

```

## Supported GHC Extensions

``` yaml

# Syntax
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

```
