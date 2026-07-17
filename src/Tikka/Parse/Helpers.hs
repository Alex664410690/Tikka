{-# LANGUAGE OverloadedStrings #-}

module Tikka.Parse.Helpers where

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Tikka.Expr

type Parser = Parsec String Text

-- Parses a single character
character :: Parser Char
character = do
  char '\''
  c <- anySingle
  char '\''
  pure c

-- Parses a string
string :: Parser [Term]
string = do
  char '\"'
  cs <- many (Exp . Val . Char <$> anySingleBut '\"')
  char '\"'
  pure cs

-- Parses a boolean
boolean :: Parser Bool
boolean = (lexeme "True" >> pure True) <|> (lexeme "False" >> pure False)

-- Parses a decimal number
number :: Parser Double
number = try (lexeme L.float) <|> lexeme L.decimal

{-
    LEXER
-}

-- Consumes at least 1 space token or newline
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- Consumes the next lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Consumes a single symbol (surrounded by whitespace)
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer
