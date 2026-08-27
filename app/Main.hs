{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Application
import Tikka

import System.Console.ANSI
import System.Exit
import System.FilePath ((</>), (<.>), takeDirectory)
import System.Directory (getCurrentDirectory)

import Flags.Applicative
import Text.Megaparsec
import Control.Monad
import Data.Text (pack, unpack, init, splitOn)

import System.IO (hFlush, stdout)
import qualified Control.Exception as E

-- Sets up the "help" flag for the compiler
flagsParser :: FlagsParser Flags
flagsParser = Flags
  <$> (flag intVal "trace" "Level of evaluation tracing, 1 - 4" <|> pure 1)
  <*> (flag intVal "verb" "Level of explanantion verbosity, 1 - 4" <|> pure 1)
  <*> (flag stringVal "dump" "Optional file to dump the desugared code to" <|> pure "")
  <*> boolFlag "stc" "Flag to enable static type checking"
  <*> boolFlag "visual" "Flag to enable the visual debugger (rather than on the command line)"
  <*> boolFlag "repl" "Flag to enable command-line repl mode after evaluation, or alternatively don't supply a file to enter straight into the REPL"

main :: IO ()
main = do
  exePath <- getExecutablePath'
  let dirPath p = takeDirectory (takeDirectory exePath) </> p

  writeFile (dirPath $ "imports" </> "local-decl") "" -- clears the local-decl file
  writeFile (dirPath $ "imports" </> "command-line") "" -- clears the command-line file
  (flags, args) <- parseSystemFlagsOrDie flagsParser
  if null args then startRepl flags "" "" else do
    fileOrError <- readFileMain $ head args
    case fileOrError of
      Left err -> printError $ "Error: Could not find the file/path in the following locations: " ++ tail (Prelude.init err)
      Right (filePath, f) -> if visual flags then runApp flags args filePath else do
          o <- parseAndEvaluateProject flags f filePath "main"
          case o of
            Left err -> printError err >> putStrLn ""
            Right (toDump, toPrint, errors) -> do
              when (dump flags /= "") (writeFile (dirPath $ dump flags) toDump)
              mapM_ prettyPrint toPrint
              unless (null errors) (printError errors)
              putStrLn ""
          when (repl flags) $ startRepl flags f filePath

-- Reads the given file if possible, returning (path found, contents)
readFileMain :: String -> IO (Either String (FilePath, String))
readFileMain f = do
  exePath <- getExecutablePath'
  let dirPath = takeDirectory (takeDirectory exePath) </> f
  currDir <- getCurrentDirectory
  let currPath = currDir </> f
  let allPaths = if currPath == dirPath then [currPath, currPath <.> "hs"] else [currPath, currPath <.> "hs", dirPath, dirPath <.> "hs"]

  foundFile <- tryReadAll allPaths
  case foundFile of
    Nothing -> pure $ Left $ show allPaths
    Just x -> pure $ Right x
  where
    tryReadAll :: [FilePath] -> IO (Maybe (FilePath, String))
    tryReadAll [] = pure Nothing
    tryReadAll (p:ps) = do
      result <- E.try (readFile p) :: IO (Either E.IOException String)
      case result of
        Right contents -> pure (Just (takeDirectory p, contents))
        Left _ -> tryReadAll ps

startRepl :: Flags -> String -> FilePath -> IO ()
startRepl flags file fp = do
  setSGR [ SetColor Foreground Vivid Yellow ]
  putStrLn "Define / run function, 'clear' to wipe memory, ':q' or ':quit' to exit"
  setSGR [ SetColor Foreground Vivid White ]
  runRepl flags file fp

runRepl :: Flags -> String -> FilePath -> IO ()
runRepl flags file fp = do
  exePath <- getExecutablePath'
  let dirPath p = takeDirectory (takeDirectory exePath) </> p

  putStr "\n>>> "
  hFlush stdout
  s <- getLine
  putStrLn ""
  when (s == ":q" || s == ":quit") exitSuccess
  if s == "clear" then do
    writeFile (dirPath $ "imports" </> "local-decl") ""
    setSGR [ SetColor Foreground Vivid Yellow ]
    putStrLn "Repl declarations cleared"
    setSGR [ SetColor Foreground Vivid White ]
  else case isFunction (pack s) of
    Nothing -> addToLocalDecl s >> putStrLn s
    Just _ -> do
      writeFile (dirPath $ "imports" </> "command-line") ("commandLineREPL = " ++ s ++ "\n")
      o <- parseAndEvaluateProject flags file fp "commandLineREPL"
      case o of
        Left err -> printError err
        Right (toDump, toPrint, errors) -> do
          when (dump flags /= "") (writeFile (dirPath $ dump flags) toDump)
          mapM_ prettyPrint toPrint
          unless (null errors) (printError errors)
  runRepl flags file fp

prettyPrint :: String -> IO ()
prettyPrint l = do
  let traceOutput = unpack $ head (splitOn "    [" (pack l))
  let traceSkip = if null $ tail (splitOn "    [...Skip" (pack l)) then "" else unpack $ Data.Text.init $ head $ tail (splitOn "    [" (pack l))
  let traceExplanation = if not (null traceSkip) || null (tail (splitOn "    [" (pack l))) then "" else unpack $ Data.Text.init $ head $ tail (splitOn "    [" (pack l))
  putStr traceOutput
  setSGR [ SetColor Foreground Vivid Yellow ]
  unless (null traceExplanation) $ putStr $ "    [" ++ traceExplanation ++ "]"
  setSGR [ SetColor Foreground Vivid White ]
  setSGR [ SetColor Foreground Vivid Green ]
  unless (null traceSkip) $ putStr $ "    [" ++ traceSkip ++ "]"
  setSGR [ SetColor Foreground Vivid White ]
  putStrLn "\n"

printError :: String -> IO ()
printError err = do
  setSGR [ SetColor Foreground Vivid Red ]
  putStr err
  setSGR [ SetColor Foreground Vivid White ]
