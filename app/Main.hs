{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Application
import Tikka

import System.Console.ANSI
import System.Exit

import Flags.Applicative
import Text.Megaparsec
import Control.Monad
import Data.Text (pack, unpack, init, splitOn)

import System.IO (hFlush, stdout)
import qualified Control.Exception as E
import System.IO.Error (isDoesNotExistError)

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
  writeFile "imports/local-decl" "" -- clears the local-decl file
  writeFile "imports/command-line" "" -- clears the command-line file
  (flags, args) <- parseSystemFlagsOrDie flagsParser
  if visual flags then runApp flags args else
    if null args then startRepl flags "" else do
      file <- E.try (readFileMain $ head args) :: IO (Either E.IOException String)
      case file of
        Left err -> if isDoesNotExistError err then printError $ "Error: The file/path '" ++ removeLastFour (head $ words $ show err) ++ "' does not exist" else printError $ "Error: " ++ show err
        Right f -> do
          o <- parseAndEvaluateProject flags f "main"
          case o of
            Left err -> printError err >> putStrLn ""
            Right (toDump, toPrint, errors) -> do
              when (dump flags /= "") (writeFile (dump flags) toDump)
              mapM_ prettyPrint toPrint
              unless (null errors) (printError errors)
              putStrLn ""
          when (repl flags) $ startRepl flags f

readFileMain :: String -> IO String
readFileMain f = do
  original <- E.try (readFile f) :: IO (Either E.IOException String)
  case original of
    Left _ -> readFile $ f ++ ".hs"
    Right file -> pure file

removeLastFour :: [a] -> [a]
removeLastFour xs = take (length xs - 4) xs

startRepl :: Flags -> String -> IO ()
startRepl flags file = do
  setSGR [ SetColor Foreground Vivid Yellow ]
  putStrLn "Define / run function, 'clear' to wipe memory, ':q' or ':quit' to exit"
  setSGR [ SetColor Foreground Vivid White ]
  runRepl flags file

runRepl :: Flags -> String -> IO ()
runRepl flags file = do
  putStr "\n>>> "
  hFlush stdout
  s <- getLine
  putStrLn ""
  when (s == ":q" || s == ":quit") exitSuccess
  if s == "clear" then do
    writeFile "imports/local-decl" ""
    setSGR [ SetColor Foreground Vivid Yellow ]
    putStrLn "Repl declarations cleared"
    setSGR [ SetColor Foreground Vivid White ]
  else case isFunction (pack s) of
    Nothing -> addToLocalDecl s >> putStrLn s
    Just _ -> do
      writeFile "imports/command-line" ("commandLineREPL = " ++ s ++ "\n")
      o <- parseAndEvaluateProject flags file "commandLineREPL"
      case o of
        Left err -> printError err
        Right (toDump, toPrint, errors) -> do
          when (dump flags /= "") (writeFile (dump flags) toDump)
          mapM_ prettyPrint toPrint
          unless (null errors) (printError errors)
  runRepl flags file

prettyPrint :: String -> IO ()
prettyPrint l = do
  let traceOutput = unpack $ head (splitOn "    [" (pack l))
  let traceExplanation = if null $ tail (splitOn "    [" (pack l)) then "" else unpack $ Data.Text.init $ head $ tail (splitOn "    [" (pack l))
  putStr traceOutput
  setSGR [ SetColor Foreground Vivid Yellow ]
  unless (null traceExplanation) $ putStr $ "    [" ++ traceExplanation ++ "]"
  setSGR [ SetColor Foreground Vivid White ]
  putStrLn "\n"

printError :: String -> IO ()
printError err = do
  setSGR [ SetColor Foreground Vivid Red ]
  putStr err
  setSGR [ SetColor Foreground Vivid White ]