{-# LANGUAGE OverloadedStrings #-}

module Tikka.Parse.Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec hiding (parseError)
import Text.Megaparsec.Char

import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (GT, LT)

import Tikka.Expr
import Tikka.Parse.Helpers as H

import Control.Monad

{-
    EXPRESSION PARSER
-}

-- Parses a variable identifier
variable :: Parser Expr
variable = Var <$> identifier

-- Parses a value
value :: Parser Expr
value =
  Val
    <$> choice
      [ Double <$> number
      , Char <$> lexeme character
      , Bool <$> lexeme boolean
      , List <$> lexeme H.string
      , try (Tuple <$> lexeme unit)
      , try (Tuple <$> lexeme tuple)
      , try consPattern
      , List <$> lexeme emptyList
      , try (List <$> lexeme list)
      , try (lexeme listRange)
      ]

-- Parses anything between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parses a unary operator
unary :: Parser Term
unary =
  choice
    [ symbol "+" >> term
    , do
        symbol "-"
        Exp . UnOp Negation <$> term
    ]

parseError :: Parser Expr
parseError = do
  lexeme "error"
  lexeme "$" <|> lexeme ""
  char '\"' <|> customFailure "Error: The 'error' function in Tikka only accepts strings in the \"...\" notation\n             Strings in other forms or other types are not accepted"
  cs <- many $ anySingleBut '\"'
  char '\"'
  pure (Error cs)

trace :: Parser Term
trace = do
  lexeme "TRACE"
  e <- between (symbol "{") (symbol "}") expr <|> term'
  pure $ Trace e

-- Parses a term in an expression
term :: Parser Term
term =
  choice
    [ trace
    , Exp <$> parseError
    , try application
    , abstraction
    , Exp <$> caseExpr
    , ifStmt
    , try (parens expr)
    , try (parens unary)
    , Exp <$> value
    , Exp <$> variable
    , customFailure "Error: Invalid term\n             This was likely caused by an incorrect character / structure being used, which is not in this language and so cannot be understood"
    ]

-- Similar to term, but used in application where you do not want to nest other applications
term' :: Parser Term
term' =
  choice
    [ trace
    , Exp <$> parseError
    , abstraction
    , Exp <$> caseExpr
    , ifStmt
    , try (parens expr)
    , try (parens unary)
    , Exp <$> value
    , Exp <$> variable
    , customFailure "Error: Invalid term\n             This was likely caused by an incorrect character / structure being used, which is not in this language and so cannot be understood"
    ]

-- Parses an expression, using the operator table below
expr :: Parser Term
expr = makeExprParser term table

-- Operator table used in expression parsing
table :: [[Operator Parser Term]]
table =
  [ [binary' "." comp]
  ,
    [ prefix "floor" (asUnOp Floor)
    , prefix "ceiling" (asUnOp Ceiling)
    ]
  ,
    [ binary "*" (asBinOp Product)
    , binary "/" (asBinOp Division)
    ]
  ,
    [ binary "+" (asBinOp Sum)
    , binary "-" (asBinOp Subtr)
    ]
  ,
    [ binary' ":" (asBinOp Cons)
    , binary' "++" (asBinOp Append)
    ]
  ,
    [ binary "==" (asBinOp Eq)
    , binary "/=" (asBinOp NEq)
    , binary "<" (asBinOp LT)
    , binary "<=" (asBinOp LE)
    , binary ">" (asBinOp GT)
    , binary ">=" (asBinOp GE)
    ]
  , [binary' "&&" (asBinOp And)]
  , [binary' "||" (asBinOp Or)]
  , [binary' "$" App]
  ]
 where
  asBinOp o x y = Exp (BinOp o x y)
  asUnOp o x = Exp (UnOp o x)

-- Function to handle the (.) / composition operator
comp :: Term -> Term -> Term
comp f g = Abs "x" $ App f $ App g $ Exp $ Var "x"

-- Possible fixities for operators
binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ op name)
binary' :: Text -> (a -> a -> a) -> Operator Parser a
binary' name f = InfixR (f <$ op name)
prefix :: Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ op' name)

op :: Text -> Parser Text
op n = (lexeme . try) (Text.Megaparsec.Char.string n <* notFollowedBy symbolChar)

op' :: Text -> Parser Text
op' n = (lexeme . try) (Text.Megaparsec.Char.string n <* notFollowedBy (symbolChar <|> letterChar <|> numberChar))

-- Parses lambda application
application :: Parser Term
application = do
  f <- try (parens expr) <|> (Exp . Var <$> identifier)
  args <- some (try term')
  pure $ foldl App f args

-- Parses lambda abstraction
abstraction :: Parser Term
abstraction = do
  args <- some argument
  b <- expr
  pure $ foldr Abs b args

-- parses a single argument for an abstraction
argument :: Parser String
argument = do
  lexeme "\\"
  Var a <- variable
  lexeme "->"
  pure a

-- Parses an identifier, which must start with a letter and can contain any alphanumeric character
identifier :: Parser String
identifier =
  try undefinedTerm <|> do
    ident <- lexeme ((:) <$> (letterChar <|> single '_') <*> many (alphaNumChar <|> single '_' <|> single '\'') <?> "an indentifier")
    when (ident == "data") (customFailure "Error: Cannot use keyword 'data' as an identifier. You may have tried to define a type starting with a lowercase letter\n             Words with special meanings such as 'case' and 'if' cannot be used as a name for a function or variable, and data types must start with an uppercase letter")
    when (ident `elem` ["error", "case", "of", "if", "then", "else", "Char", "Double", "Bool", "True", "False"]) (customFailure $ "Error: Cannot use keyword '" ++ ident ++ "' as an identifier\n             Words with special meanings such as 'case' and 'if' cannot be used as a name for a function or variable")
    pure ident

-- Parses an identifier, which must start with a lowercase letter and can contain any alphanumeric character
varIdentifier :: Parser String
varIdentifier =
  try undefinedTerm <|> do
    ident <- lexeme ((:) <$> lowerChar <*> many (alphaNumChar <|> single '_' <|> single '\'') <?> "an indentifier")
    when (ident == "data") (customFailure "Error: Cannot use keyword 'data' as an identifier. You may have tried to define a type starting with a lowercase letter\n             Words with special meanings such as 'case' and 'if' cannot be used as a name for a function or variable, and data types must start with an uppercase letter")
    when (ident `elem` ["error", "case", "of", "if", "then", "else", "Char", "Double", "Bool", "True", "False"]) (customFailure $ "Error: Cannot use keyword '" ++ ident ++ "' as an identifier\n             Words with special meanings such as 'case' and 'if' cannot be used as a name for a function or variable")
    pure ident

-- Parses a datatype or constructor identifier, which must start with an uppercase letter and can contain any alphanumeric character
identifier' :: Parser String
identifier' = try undefinedTerm <|> lexeme ((:) <$> upperChar <*> many (alphaNumChar <|> single '\'') <?> "an indentifier")

-- Parses a type identifier (or polymorphic placeholder)
typeIdentifier :: Parser String
typeIdentifier = identifier' <|> identifier

-- Parses any term which is not defined in Tikka
undefinedTerm :: Parser String
undefinedTerm = do
  t <- choice $ map (\p -> lexeme (p <* space1)) ["|", "newtype", "class", "instance", "True", "False"] ++ [lexeme "`"]
  customFailure (chooseError t)
 where
  chooseError "|" = "Error: Guards are not supported in Tikka\n             Consider using a case statement or top-level pattern matching instead"
  chooseError "newtype" = "Error: 'newtype' is not supported in Tikka\n             Consider using the 'data' instead"
  chooseError "class" = "Error: Type classes are not supported in Tikka\n             Consider defining individual functions for applicable datatypes"
  chooseError "instance" = "Error: Type classes are not supported in Tikka\n             Consider defining individual functions for applicable datatypes"
  chooseError "`" = "Error: Infix notation (`...`) is not supported in Tikka\n             Custom operators cannot be defined in Tikka, and functions should be used as f x y rather than x `f` y"
  chooseError x = "Error: The term/operator '" ++ Text.unpack x ++ "' is not supported in Tikka"

-- Parses a case expression
caseExpr :: Parser Expr
caseExpr = do
  lexeme "case"
  e <- expr
  lexeme "of"
  cs <- some $ try oneCase
  pure (Case e cs)

-- Parses a single case in a case expression
oneCase :: Parser (Term, Term)
oneCase = do
  cond <-
    choice
      [ Exp . Val <$> consPattern
      , try term
      , listPattern
      , Exp . Var <$> many (lexeme $ char '_')
      ]
  lexeme "->"
  result <- expr
  lexeme ";"
  pure (cond, result)

-- Parses an if statement
ifStmt :: Parser Term
ifStmt = do
  lexeme "if"
  e <- expr
  lexeme "then"
  t <- expr
  lexeme "else"
  f <- expr
  pure (Exp $ Case e [(Exp $ Val $ Bool True, t), (Exp $ Val $ Bool False, f)])

-- Parses a unit, ()
unit :: Parser [Term]
unit = do
  lexeme "()"
  pure []

-- Parses a tuple of the form (,...,)
tuple :: Parser [Term]
tuple = do
  lexeme "("
  x <- expr
  xs <- some listOrTupleTerm
  lexeme ")"
  pure (x : xs)

-- Parses a term proceeded by a comma
listOrTupleTerm :: Parser Term
listOrTupleTerm = do
  lexeme ","
  lexeme expr

-- Parses the empty list, []
emptyList :: Parser [Term]
emptyList = do
  lexeme "[]"
  pure []

-- Parses a list of the form [,...,]
list :: Parser [Term]
list = do
  lexeme "["
  x <- expr
  xs <- many listOrTupleTerm
  lexeme "]" <|> do
    lexeme "|"
    customFailure "Error: List constructors are not supported in Tikka\n             Consider using a list range or a recursive function instead"
  if all (sameTypeCheck x) xs then pure (x : xs) else customFailure "Error: All values in a list must be of the same type\n       Lists always contain only one type of value. To store different types together consider using a tuple"

-- Parses a list range, of the form [x, y .. z]
listRange :: Parser Value
listRange = do
  lexeme "["
  x <- number
  y <- listRangeStep <|> pure (x + 1)
  lexeme ".."
  z <- Left <$> number <|> pure (Right x) -- ignore right
  lexeme "]"
  case z of
    Left a -> pure $ List [Exp . Val . Double $ val | val <- [x, y .. a]]
    Right _ -> pure $ InfiniteList [Exp . Val . Double $ val | val <- [x, y ..]]

-- Parses a potential step in a list range, 'y'
listRangeStep :: Parser Double
listRangeStep = do
  lexeme ","
  number

-- Parses the cons operator construction when used in a case statement
consPattern :: Parser Value
consPattern = do
  lexeme "("
  x <- try term
  lexeme ":"
  s <- variable
  lexeme ")"
  pure (ListPattern x (Exp s))

-- Parses a list when used in a case statement
listPattern :: Parser Term
listPattern = do
  lexeme "["
  x <- try expr
  xs <- many listOrTupleTerm
  lexeme "]"
  if all (\y -> isVar y || sameTypeCheck (typeVar (x : xs)) y) xs then pure (Exp . Val . List $ x : xs) else customFailure "Error: All values in a pattern matching list must be of the same type, or a variable\n       Lists always contain only one type of value. To store different types together consider using a tuple"
 where
  isVar (Exp (Var _)) = True
  isVar _ = False
  typeVar zs = head (filter (not . isVar) zs)
