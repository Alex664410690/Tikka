{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-name-shadowing #-}

module Tikka.Expr where

import Data.List qualified as List
import Prelude hiding (GT, LT)

import Data.String.Interpolate

-- Represents an arithmetic expression or term
data Expr
  = Var String
  | Val Value
  | Error String
  | UnOp OpUnary Term
  | BinOp OpBinary Term Term
  | Case Term [(Term, Term)]
  deriving
    (Eq)

data OpUnary
  = Negation
  | Floor
  | Ceiling
  deriving (Eq, Ord)

instance Show OpUnary where
  show Negation = "-"
  show Floor = "floor "
  show Ceiling = "ceiling "

data OpBinary
  = Sum
  | Subtr
  | Product
  | Division
  | Cons
  | Append
  | And
  | Or
  | Eq
  | NEq
  | LT
  | LE
  | GT
  | GE
  deriving (Eq, Ord)

isEqualityOp :: OpBinary -> Bool
isEqualityOp o = o `elem` [Eq, NEq]
isNumericOp :: OpBinary -> Bool
isNumericOp o = o `elem` [Sum, Subtr, Product, Division]
isComparisonOp :: OpBinary -> Bool
isComparisonOp o = o `elem` [LT, LE, GT, GE]
isBooleanOp :: OpBinary -> Bool
isBooleanOp o = o `elem` [And, Or]

instance Show OpBinary where
  show = \case
    Sum -> "+"
    Subtr -> "-"
    Product -> "*"
    Division -> "/"
    Cons -> ":"
    Append -> "++"
    And -> "&&"
    Or -> "||"
    Eq -> "=="
    NEq -> "/="
    LT -> "<"
    LE -> "<="
    GT -> ">"
    GE -> ">="

instance Show Expr where
  show :: Expr -> String
  show (Var s) = s
  show (Val v) = show v
  show (Error e) = "error \"" ++ e ++ "\""
  show (UnOp Negation t) = show Negation ++ show t
  show (UnOp o t) = oo ++ addBrackets oo t
   where
    oo = show o
  show (BinOp o t1 t2) = addBrackets oo t1 ++ " " ++ oo ++ " " ++ addBrackets oo t2
   where
    oo = show o
  show (Case c es) = "( case " ++ show c ++ " of" ++ concatMap showCase es ++ " )"
   where
    showCase (a, b) = " " ++ show a ++ " -> " ++ show b ++ ";"

-- Represents a lambda function or expression
data Term = Exp Expr | Abs String Term | App Term Term | Trace Term

-- FUTURE WORK
{-

-- Represents a lambda function or expression
data Term
  = Exp Expr
  | Abs String Term
  | App Term Term
  | Trace Term
  | UnOp OpUnary Term
  | BinOp OpBinary Term
  | Case Term [ (Term, Term )]
  | Var String
  | Val Value
  | Error String

-}

instance Eq Term where
  Exp e1 == Exp e2 = e1 == e2
  Abs s1 l1 == Abs s2 l2 = s1 == s2 && l1 == l2
  App l1 l2 == App l3 l4 = l1 == l3 && l2 == l4
  Trace l1 == Trace l2 = l1 == l2
  -- l1 == Trace l2 = l1 == l2
  -- Trace l1 == l2 = l1 == l2
  _ == _ = False

instance Show Term where
  show (Exp e) = show e
  show (Abs s f) = "\\" ++ s ++ " -> " ++ show f
  show (App f g) = case f of
    (Exp (Var s)) -> case g of
      (Exp (Var t)) -> s ++ " " ++ t
      (Exp (Val e)) -> s ++ " " ++ show e
      _ -> s ++ " (" ++ show g ++ ")"
    (Exp (Val d)) -> case g of
      (Exp (Var t)) -> show d ++ " " ++ t
      (Exp (Val e)) -> show d ++ " " ++ show e
      _ -> show d ++ " (" ++ show g ++ ")"
    _ -> case g of
      (Exp (Var t)) -> "(" ++ show f ++ ") " ++ t
      (Exp (Val e)) -> "(" ++ show f ++ ") " ++ show e
      _ -> "(" ++ show f ++ ") (" ++ show g ++ ")"
  show (Trace l) = show l

-- Represents a value
data Value
  = Double Double
  | Char Char
  | Bool Bool
  | Tuple [Term]
  | List [Term]
  | ListPattern Term Term -- 2nd should always be (Expr (Var ...))
  | InfiniteList [Term]
  | CustomDT String [Term] TermType

instance Eq Value where
  Double d1 == Double d2 = d1 == d2
  Char c1 == Char c2 = c1 == c2
  Bool b1 == Bool b2 = b1 == b2
  Tuple e1 == Tuple e2 = e1 == e2
  List e1 == List e2 = e1 == e2
  ListPattern e1 _ == ListPattern e2 _ = e1 == e2
  InfiniteList e1 == InfiniteList e2 = head e1 == head e2 && head (tail e1) == head (tail e2)
  CustomDT n1 es1 _ == CustomDT n2 es2 _ = n1 == n2 && es1 == es2
  _ == _ = False

instance Show Value where
  show (Double d)
    | d == fromIntegral @Integer (floor d) = show @Integer (floor d)
    | otherwise = show d
  show (Char c) = show c
  show (Bool b) = show b
  show (Tuple []) = "()"
  show (Tuple xs) = "(" ++ showAll xs ++ ")"
  show (List []) = "[]"
  show (List (Exp (Val (Char c)) : cs)) =
    "\"" ++ concatChars (Exp (Val (Char c)) : cs) ++ "\""
  show (List xs) = "[" ++ showAll xs ++ "]"
  show (ListPattern x y) = "(" ++ show x ++ " : " ++ show y ++ ")"
  show (InfiniteList (x : y : z : _)) = "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ " ...]"
  show (InfiniteList _) = error "This list should be infinite"
  show (CustomDT n [] (V l _)) = l ++ "@" ++ n
  show (CustomDT n es (V l _)) = l ++ "@" ++ n ++ " " ++ brackets (unwords (map show es))
  show (CustomDT n es _) = n ++ " " ++ brackets (unwords (map show es))

-- | An TermType is a (approximate) type of an expression.
data TermType
  = -- | Typed value, where the first argument is the type name
    V String [TermType]
  | -- | Function from the first type to the second type.
    F TermType TermType
  | -- | A polymorphic type variable
    N Int

instance Show TermType where
  show (V "list" []) = error "list should never have no type"
  show (V "list" [V "Char" _]) = "String"
  show (V "list" [x]) = "[" ++ show x ++ "]"
  show (V "list" xs) = error $ "list should never have more than 1 type, " ++ show xs
  show (V "tuple" []) = "()"
  show (V "tuple" xs) = "(" ++ showAll xs ++ ")"
  show (V l _) = l
  show (F (F t1 t1') t2) = "(" ++ show (F t1 t1') ++ ") -> " ++ show t2
  show (F t1 t2) = show t1 ++ " -> " ++ show t2
  show (N n) = "a" ++ show n

instance Eq TermType where
  V "list" xs == V "list" ys = xs == ys
  V "tuple" xs == V "tuple" ys = xs == ys
  V l1 _ == V l2 _ = l1 == l2
  F t1 t3 == F t2 t4 = t1 == t2 && t3 == t4
  _ == N _ = True
  N _ == _ = True
  _ == _ = False

------------------------
-- HELPERS

brackets :: String -> String
brackets s
  | null s = s
  | head s `elem` ['(', '['] && last s `elem` [']', ')'] = s
  | length (words s) > 1 = "(" ++ s ++ ")"
  | otherwise = s

-- Concatenate a list of characters
concatChars :: [Term] -> String
concatChars [] = ""
concatChars (Exp (Val (Char c)) : cs) = c : concatChars cs
concatChars _ = error "Invalid character found in string"

showAll :: (Show a) => [a] -> String
showAll xs = List.intercalate ", " $ map show xs

-- Adds brackets only where needed in the display of an expression
addBrackets :: String -> Term -> String
addBrackets o e = if opPrecedence o > exprPrecedence e then "(" ++ show e ++ ")" else show e

-- Adds brackets only where needed in the display of an expression using non-associative operators
addBrackets' :: String -> Term -> String
addBrackets' o e = if opPrecedence o >= exprPrecedence e then "(" ++ show e ++ ")" else show e

opPrecedence :: String -> Integer
opPrecedence "||" = 0
opPrecedence "&&" = 1
opPrecedence "<" = 2
opPrecedence "<=" = 2
opPrecedence ">" = 2
opPrecedence ">=" = 2
opPrecedence "==" = 2
opPrecedence "/=" = 2
opPrecedence "++" = 3
opPrecedence ":" = 3
opPrecedence "+" = 4
opPrecedence "-" = 4
opPrecedence "*" = 5
opPrecedence "/" = 5
opPrecedence "floor " = 6
opPrecedence "ceiling " = 6
opPrecedence op = error [i|invalid operator #{op}|]

exprPrecedence :: Term -> Integer
exprPrecedence (Exp e) = case e of
  BinOp o _ _ -> case o of
    Or -> 0
    And -> 1
    LT -> 2
    LE -> 2
    GT -> 2
    GE -> 2
    Eq -> 2
    NEq -> 2
    Append -> 3
    Cons -> 3
    Sum -> 4
    Subtr -> 4
    Product -> 5
    Division -> 5
  UnOp o _ -> case o of
    Negation -> 7
    Floor -> 6
    Ceiling -> 6
  _ -> 7
exprPrecedence _ = 7

sameType :: LUT Term -> Term -> Term -> Either String (Maybe (TermType, TermType))
sameType fs x y = case inferTermType fs [""] x of
  Left err -> Left err
  Right t -> case inferTermType fs [""] y of
    Left err -> Left err
    Right t' -> if snd (head t) == snd (head t') then Right Nothing else Right $ Just (snd $ head t, snd $ head t')

sameTypeCheck :: Term -> Term -> Bool
sameTypeCheck x y = case sameType [] x y of
  Left _ -> False
  Right Nothing -> True
  Right _ -> False

type LUT a = [(String, a)]

-- Infers the type of a given expression (head of the list of references and their types), or returns a type error
-- 1st arg = list of function references and bodies
-- 2nd arg = the name of the expression (if it is a function body)
-- 3rd arg = the expression to infer
inferExprType :: LUT Term -> [String] -> Expr -> Either String (LUT TermType)
inferExprType fs name (Var s) =
  if s `elem` name
    then Right [(s, N 0)]
    else case getRef fs s of
      Left _ -> Right [(s, N 0)]
      Right e -> addName s $ inferTermType fs (name ++ [s]) e
       where
        addName _ (Left err) = Left err
        addName _ (Right []) = Right []
        addName s' (Right ((_, t) : ts)) = Right ((s', t) : ts)
inferExprType fs name (Val v) = getValueType fs name v
inferExprType _ _ (Error _) = Right [("", N 0)]
inferExprType fs name (UnOp o e) = do
  res <- inferTermType fs name e
  case res of
    ((l, N _) : ts) -> Right ((l, V "Double" []) : ts)
    ((l, V "Double" []) : ts) -> Right ((l, V "Double" []) : ts)
    _ -> Left [i|Error: #{opName o} can only be performed on Doubles|]
 where
  opName Negation = "Negation"
  opName Floor = "The 'floor' function"
  opName Ceiling = "The 'ceiling' function"
inferExprType fs name (Case e es) = do
  casesT <- inferCaseType fs names (map snd es)
  case casesT of
    [] -> error "list of type inferences should never be empty"
    tys@((l, t) : ts) -> do
      tuples <- checkTypesSingle (map (inferCasePatternType fs tys . fst) es)
      t' <- singleBestType tys tuples
      let patternTypes = createPatternTypes fs names e tys t'
      if l == ""
        then Right ((l, t) : patternTypes ++ ts)
        else case getRef patternTypes l of
          Left _ -> Right ((l, t) : patternTypes ++ ts)
          Right t'' -> Right ((l, t'') : patternTypes ++ ts)
 where
  names = getNames (map fst es) ++ name
inferExprType fs name (BinOp o d e) = do
  dRes <- inferTermType fs name d
  eRes <- inferTermType fs name e

  let
    exprRep = \case
      Eq -> "Equals operator (==)"
      NEq -> "Not Equals operator (/=)"
      Sum -> "Addition operator (+)"
      Subtr -> "Subtraction operator (-)"
      Product -> "Multiplication operator (*)"
      Division -> "Division operator (/)"
      LT -> "Less Than operator (<)"
      LE -> "Less Than or Equal operator (<=)"
      GT -> "Greater Than operator (>)"
      GE -> "Greater Than or Equal operator (>=)"
      And -> "And operator (&&)"
      Or -> "Or operator (||)"
      Cons -> "Cons operator (:)"
      Append -> "Append operator (++)"

  let
    handleEq = case (dRes, eRes) of
      -- Both polymorphic
      ((l, N _) : ts, (l', N _) : ts') -> Right (("", V "Bool" []) : ts ++ ts')
      -- Left polymorphic, right typed
      ((l, N _) : ts, (l', ej@(V et ets)) : ts') -> Right (("", V "Bool" []) : (l, ej) : (l', ej) : ts ++ ts')
      -- Right polymorphic, left typed
      ((l, dj@(V dt dts)) : ts, (l', N _) : ts') -> Right (("", V "Bool" []) : (l, dj) : (l', dj) : ts ++ ts')
      -- Both typed
      ((l, dj@(V dt dts)) : ts, (l', ej@(V et ets)) : ts')
        | dj == ej -> Right (("", V "Bool" []) : (l, dj) : (l', ej) : ts ++ ts')
        | otherwise ->
            Left
              [i|Error: #{exprRep o} used on #{d} of type #{dj} and #{e} of type #{ej}
      Equality can only be checked for values of the same type|]
      -- One or the other is a function
      (_, (l', F _ _) : ts') -> Left [i|Error: #{exprRep o} used on #{e}, which is a function|]
      ((l', F _ _) : ts', _) -> Left [i|Error: #{exprRep o} used on #{d}, which is a function|]
      (_, _) -> error [i|The impossible happened! (a subexpression does not have a typing judgment)|]

    handleNC = case (dRes, eRes) of
      -- Left polymorphic, right typed badly
      (_, (l', ej@(V et _)) : _)
        | et /= "Double" ->
            Left [i|Error: #{exprRep o} was used on #{e}, which is of type #{ej} (not a Double)|]
      (_, (l', F _ _) : ts') ->
        Left [i|Error: #{exprRep o} was used on #{e}, which is a function|]
      -- Right polymorphic, left typed badly
      ((l, dj@(V dt _)) : _, _)
        | dt /= "Double" ->
            Left [i|Error: #{exprRep o} was used on #{d}, which is of type #{dj} (not a Double)|]
      ((l', F _ _) : ts', _) ->
        Left [i|Error: #{exprRep o} was used on #{d}, which is a function|]
      -- In all other cases, the correct judgment can be made that both sides are Doubles
      -- Since both sides are either Double or polymorphic
      ((l, _) : ts, (l', _) : ts') ->
        Right ((l, V "Double" []) : (l', V "Double" []) : ts ++ ts')
      (_, _) -> error [i|The impossible happened! (a subexpression does not have a typing judgment)|]

    handleB = case (dRes, eRes) of
      -- Left polymorphic, right typed badly
      (_, (l', ej@(V et _)) : _)
        | et /= "Bool" ->
            Left [i|Error: #{exprRep o} was used on #{e}, which is of type #{ej} (not a Boolean value)|]
      (_, (l', F _ _) : ts') ->
        Left [i|Error: #{exprRep o} was used on #{e}, which is a function|]
      -- Right polymorphic, left typed badly
      ((l, dj@(V dt _)) : _, _)
        | dt /= "Bool" ->
            Left [i|Error: #{exprRep o} was used on #{d}, which is of type #{dj} (not a Boolean value)|]
      ((l', F _ _) : ts', _) ->
        Left [i|Error: #{exprRep o} was used on #{d}, which is a function|]
      -- In all other cases, the correct judgment can be made that both sides are Doubles
      -- Since both sides are either Double or polymorphic
      ((l, _) : ts, (l', _) : ts') ->
        Right ((l, V "Bool" []) : (l', V "Bool" []) : ts ++ ts')
      (_, _) -> error [i|The impossible happened! (a subexpression does not have a typing judgment)|]

  case o of
    Eq -> handleEq
    NEq -> handleEq
    Sum -> handleNC
    Subtr -> handleNC
    Product -> handleNC
    Division -> handleNC
    LT -> handleNC
    LE -> handleNC
    GT -> handleNC
    GE -> handleNC
    And -> handleB
    Or -> handleB
    Cons -> do
      let
        listType ((_, t1) : _) ((_, V "list" (t2 : _)) : _) = if t1 == t2 then Nothing else Just $ "Error: The ':' operator cannot be used on element '" ++ show d ++ "' of type '" ++ show t1 ++ "' and list '" ++ show e ++ "' of type '[" ++ show t2 ++ "]'\n       Lists can only have one type of element, and so you cannot add a value of a different type to the list. If this is needed, maybe consider using tuples"
        listType _ ((_, N _) : _) = Nothing
        listType (_ : _) ((_, t2) : _) = Just $ "Error: The ':' operator cannot be used on '" ++ show e ++ "' of type '" ++ show t2 ++ "'\n       The second argument of the cons operator must be a list (or string)"
        listType _ _ = error "list of type inferences should never be empty"
      case (listType dRes eRes, dRes, eRes) of
        (Nothing, dt : dts, et : ets) -> case singleBestType (dRes ++ eRes) [snd dt, getListType eRes] of
          Left err' -> Left err'
          Right t -> Right (("", V "list" [t]) : (fst dt, t) : (fst et, V "list" [t]) : dts ++ ets)
        (Just err, _, _) -> Left err
        (_, _, _) -> Left "Typing information unavailable for subexpressions"
    Append -> do
      let
        listType ((_, V "list" (t1 : _)) : _) ((_, V "list" (t2 : _)) : _) = if t1 == t2 then Nothing else Just $ "Error: The '++' operator cannot be used on lists '" ++ show d ++ "' of type '[" ++ show t1 ++ "]' and '" ++ show e ++ "' of type '[" ++ show t2 ++ "]'\n       Lists can only have one type of element, and so you cannot append a list of a different type to another list. If this is needed, maybe consider using tuples"
        listType _ ((_, N _) : _) = Nothing
        listType ((_, N _) : _) _ = Nothing
        listType ((_, t1) : _) ((_, V "list" _) : _) = Just $ "Error: The '++' operator cannot be used on '" ++ show d ++ "' of type '" ++ show t1 ++ "'\n       Appending can only be done on two lists (or strings)"
        listType ((_, V "list" _) : _) ((_, t2) : _) = Just $ "Error: The '++' operator cannot be used on '" ++ show e ++ "' of type '" ++ show t2 ++ "'\n       Appending can only be done on two lists (or strings)"
        listType ((_, t1) : _) ((_, t2) : _) = Just $ "Error: The '++' operator cannot be used on '" ++ show d ++ "' of type '" ++ show t1 ++ "' and '" ++ show e ++ "' of type '" ++ show t2 ++ "'\n       Appending can only be done on two lists (or strings)"
        listType _ _ = error "list of type inferences should never be empty"

      case (listType dRes eRes, dRes, eRes) of
        (Nothing, dt : dts, et : ets) ->
          case singleBestType (dRes ++ eRes) [getListType dRes, getListType eRes] of
            Left err' -> Left err'
            Right t -> Right (("", V "list" [t]) : (fst dt, V "list" [t]) : (fst et, V "list" [t]) : dts ++ ets)
        (Just err, _, _) -> Left err
        (_, _, _) -> Left "Typing information unavailable for subexpressions"

-- Find the body for the given function reference
getRef :: LUT a -> String -> Either String a
getRef [] s = Left $ "Error: No identifier '" ++ s ++ "' found\n       Did you spell it correctly?" -- was just 'Var s' which ends evaluation without error
getRef ((i, e) : t) s = if s == i then Right e else getRef t s

-- Infers the type of the input for a given list of cases
inferCasePatternType :: LUT Term -> LUT TermType -> Term -> Either String TermType
inferCasePatternType fs ts (Exp (Var s)) = case getRef ts s of
  Right x -> Right x
  Left _ -> case getRef fs s of
    Left _ -> Right $ getNextNull ts
    Right y -> case inferTermType fs [s] y of
      Left err -> Left err
      Right ((_, z) : _) -> Right z
      Right [] -> error "list of type inferences should never be empty"
inferCasePatternType _ _ (Exp (Val (CustomDT _ _ t))) = Right t -- if custom data type is fully formed
inferCasePatternType fs ts (Exp (Val (ListPattern e _))) = case getRef ts (show e) of
  Left _ -> Right $ V "list" [patternType]
  Right x -> Right $ V "list" [x]
 where
  patternType = case inferTermType fs [""] e of
    Left _ -> getNextNull ts
    Right ((_, t) : _) -> t
    Right [] -> error "list of type inferences should never be empty"
inferCasePatternType fs ts (Exp (Val (Tuple x))) =
  case checkTypesSingle $ map (inferCasePatternType fs ts) x of
    Left err -> Left err
    Right xs -> Right $ V "tuple" xs
inferCasePatternType _ ts (Exp (Val x)) = case getValueType [] [""] x of
  Left _ -> Right $ getNextNull ts
  Right ((_, t) : _) -> Right t
  Right _ -> error "No types received for getValueType"
inferCasePatternType fs _ f = case inferTermType fs [""] f of
  -- if custom data type constructor is used
  Left err -> Left err
  Right ts -> Right $ getOutputType (snd $ head ts)
   where
    getOutputType (F _ b) = getOutputType b
    getOutputType z = z

-- Selects the most useful type information for each variable used in pattern matching
selectBestType :: LUT TermType -> [[TermType]] -> Either String [TermType]
selectBestType _ [] = Right []
selectBestType fs es =
  if all null es
    then Right []
    else case singleBestType fs (map head $ filter (not . null) es) of
      Left err -> Left err
      Right t -> case selectBestType (("", t) : fs) (map tail $ filter (not . null) es) of
        Left err -> Left err
        Right ts -> case singleBestType fs (map head $ filter (not . null) es) of
          Left err -> Left err
          Right t' -> Right (t' : ts)

singleBestType :: LUT TermType -> [TermType] -> Either String TermType
singleBestType fs [] = Right $ getNextNull fs
singleBestType fs (N i : ts) = case singleBestType fs ts of
  Left err -> Left err
  Right (N j) -> if i < j then Right (N i) else Right (N j)
  x -> x
singleBestType fs ((V l t) : ts) = case checkInputTypes (V l t) ts of
  Just s -> Left s
  Nothing -> case selectBestType fs (t : [ts' | (V _ ts') <- ts]) of
    Left _ -> Right $ V l ts -- this happens if ((V l t) : ts) are of the same customDT but different constructors
    Right ts' -> Right $ V l ts'
singleBestType fs ((F a b) : ts) = case singleBestType fs (a : [a' | (F a' _) <- ts]) of
  Left err -> Left err
  Right x -> case singleBestType fs (b : [b' | (F _ b') <- ts]) of
    Left err -> Left err
    Right y -> Right $ F x y

checkInputTypes :: TermType -> [TermType] -> Maybe String
checkInputTypes _ [] = Nothing
checkInputTypes (V "list" t) ((V "list" t') : ts) = if t == t' then checkInputTypes (V "list" t) ts else Just $ "Error: Case expression patterns have different types, " ++ show (V "list" t) ++ " and " ++ show (V "list" t')
checkInputTypes (V "tuple" t) ((V "tuple" t') : ts) = if t == t' then checkInputTypes (V "tuple" t) ts else Just $ "Error: Case expression patterns have different types, " ++ show (V "tuple" t) ++ " and " ++ show (V "list" t')
checkInputTypes (V l t) ((V l' t') : ts) = if l == l' then checkInputTypes (V l t) ts else Just $ "Error: Case expression patterns have different types, " ++ show (V l t) ++ " and " ++ show (V l' t')
checkInputTypes t (t' : ts) = if t == t' then checkInputTypes t ts else Just $ "Error: Case expression patterns have different types, " ++ show t ++ " and " ++ show t'

-- Infers types for each variable used in pattern matching
createPatternTypes :: LUT Term -> [String] -> Term -> LUT TermType -> TermType -> LUT TermType
createPatternTypes _ _ (Exp (Var s)) _ t = [(s, t)]
createPatternTypes _ _ (Exp (Val (Tuple es))) _ (V _ ts) = zip (map show es) ts
createPatternTypes _ _ (Exp (Val (CustomDT n _ t))) _ _ = [(n, t)]
createPatternTypes fs name ((Abs s x)) fs' (F a b) = (s, a) : createPatternTypes fs name x fs' b
createPatternTypes fs name ((App x y)) fs' t = case getRef fs' (show y) of
  Right yt -> createPatternTypes fs name x fs' (F yt t) ++ [(show y, yt)]
  Left _ -> case getRef fs' (show x) of
    Left _ -> createPatternTypes fs name x fs' (F (getNextNull fs') t) ++ createPatternTypes fs name x fs' (getNextNull fs')
    Right (F a b) -> [(show x, F a b), (show y, a)]
    Right _ -> []
createPatternTypes fs name e _ _ = case inferTermType fs name e of
  Left _ -> []
  Right x -> x

-- Infers the type of a given case expression (head of the list of references and their types), or returns a type error
-- 1st arg = list of function references and bodies
-- 2nd arg = the name of the overarching function
-- 3rd arg = the case bodies to infer
inferCaseType :: LUT Term -> [String] -> [Term] -> Either String (LUT TermType)
inferCaseType _ _ [] = Right []
inferCaseType fs name (e : es) = do
  eRes <- inferTermType fs name e
  case eRes of
    [] -> error "list of type inferences should never be empty"
    (l, N i) : ts -> do
      esRes <- inferCaseType fs name es
      case esRes of
        [] -> Right ((l, N i) : ts)
        ((l', t) : ts')
          | l == "" -> Right (((l', t) : ts) ++ ((l, t) : ts'))
          | otherwise -> Right (((l, t) : ts) ++ ((l', t) : ts'))
    ((l, t) : ts) -> do
      esRes <- inferCaseType fs name es
      case esRes of
        [] -> Right ((l, t) : ts)
        ((l', N _) : ts')
          | l == "" -> Right (((l', t) : ts') ++ ((l, t) : ts))
          | otherwise -> Right (((l, t) : ts') ++ ((l', t) : ts))
        ((l', t') : ts')
          | t == t' && length ts > length ts' -> Right ((l, t) : (l', t') : ts ++ ts')
          | t == t' -> Right ((l', t') : (l, t) : ts ++ ts')
          | otherwise -> Left $ "Error: Case expressions '" ++ show e ++ "' and '" ++ show (head es) ++ "' are of different types"

-- Left err -> Left err
-- Right ((l, N i) : ts) -> case inferCaseType fs name es of
--   Left err -> Left err
--   Right
--   Right [] -> Right
-- Right ((l, t) : ts) -> case inferCaseType fs name es of
--   Left err -> Left err
--   Right ((l', N _) : ts') -> if l == "" then Right (((l', t) : ts') ++ ((l, t) : ts)) else Right (((l, t) : ts') ++ ((l', t) : ts))
--   Right ((l', t') : ts') ->
--     if t == t'
--       then
--         if length ts > length ts'
--           then Right ((l, t) : (l', t') : ts ++ ts') -- works out which case infered the most about other types (cheap but dirty)
--           else Right ((l', t') : (l, t) : ts ++ ts')
--       else Left $ "Error: Case expressions '" ++ show e ++ "' and '" ++ show (head es) ++ "' are of different types"
--   Right [] -> Right ((l, t) : ts)
-- Right _ -> error "list of type inferences should never be empty"

-- Infers the type of a given lambda function (head of the list of references and their types), or returns a type error
-- 1st arg = list of function references and bodies
-- 2nd arg = the name of the overarching function (if it is a function body)
-- 3rd arg = the lambda to infer
inferTermType :: LUT Term -> [String] -> Term -> Either String (LUT TermType)
inferTermType fs name (Exp e) = inferExprType fs name e
inferTermType fs name (Abs s f) = case inferTermType fs (s : name) f of
  Left err -> Left err
  Right [] -> error "list of type inferences should never be empty"
  Right ts -> case getRef ts s of
    Left _ -> Right (("", F (getNextNull ts) (snd (head ts))) : ts)
    Right t -> Right (("", F t (snd (head ts))) : ts)
inferTermType fs name (App f g) = do
  ft <- inferTermType fs name f
  case ft of
    ((n, F (N i) x) : ts) -> do
      gt <- inferTermType fs name g
      case gt of
        ((n', g') : ts') -> Right (("", replaceType i g' x) : (n, F g' (replaceType i g' x)) : (n', g') : ts ++ ts')
        _ -> error "list of type inferences should never be empty"
    ((_, F a b) : ts) -> do
      gt <- inferTermType fs name g
      case gt of
        ((n', g') : ts') ->
          if a == g'
            then case singleBestType (ts ++ ts') [a, g'] of
              Left err -> Left err
              Right t -> Right $ ("", foldr (uncurry replaceType) b (getTypesToReplace a g')) : (n', t) : ts' ++ ts
            else Left $ "Error: argument '" ++ show g ++ "' of type " ++ show g' ++ " applied to function '" ++ show f ++ "' of type " ++ show (F a b)
        _ -> error "list of type inferences should never be empty"
    ((n, N i) : ts) -> do
      gt <- inferTermType fs name g
      case gt of
        ((n', g') : ts') -> Right (("", getNextNull ((n, N i) : ts ++ ts')) : (n, F g' (getNextNull ((n, N i) : ts ++ ts'))) : (n', g') : ts' ++ ts)
        _ -> error "list of type inferences should never be empty"
    ((_, f') : _) -> Left $ "Error: argument " ++ show g ++ " applied to " ++ show f ++ " of type " ++ show f'
    [] -> error "list of type inferences should never be empty"
inferTermType fs name (Trace f) = inferTermType fs name f

-- Creates a list of null values and the type that should be replaced
-- 1st arg = type to search
-- 2nd arg = type to reference
getTypesToReplace :: TermType -> TermType -> [(Int, TermType)]
getTypesToReplace (V _ xs) (V _ ys) = concatMap (uncurry getTypesToReplace) (zip xs ys)
getTypesToReplace (F t1 t3) (F t2 t4) = getTypesToReplace t1 t2 ++ getTypesToReplace t3 t4
getTypesToReplace (N _) (N _) = []
getTypesToReplace (N i) t = [(i, t)]
getTypesToReplace _ (N _) = []
getTypesToReplace _ _ = error "these 2 type expressions should be equal"

-- Replaces linked polymorphic types with their inferred type
-- 1st arg = numbered 'null' to replace
-- 2nd arg = type to replace with
-- 3rd arg = type to replace
replaceType :: Int -> TermType -> TermType -> TermType
replaceType i x (N j) = if i == j then x else N j
replaceType i x (V l ts) = V l (map (replaceType i x) ts)
replaceType i x (F a b) = F (replaceType i x a) (replaceType i x b)

-- Gets the type name from a value
getValueType :: LUT Term -> [String] -> Value -> Either String (LUT TermType)
getValueType _ _ (Double _) = Right [("", V "Double" [])]
getValueType _ _ (Char _) = Right [("", V "Char" [])]
getValueType _ _ (Bool _) = Right [("", V "Bool" [])]
getValueType fs name (Tuple es) = case getDTTermTypes fs name es of
  Left err -> Left err
  Right (ts, ts') -> Right $ ("", V "tuple" ts) : ts'
getValueType _ _ (List []) = Right [("", V "list" [N 0])]
getValueType fs name (List (e : _)) = case inferTermType fs name e of
  Left err -> Left err
  Right ((_, t) : _) -> Right [("", V "list" [t])]
  Right _ -> error "list of type inferences should never be empty"
getValueType fs n (ListPattern e _) = case inferTermType fs n e of
  Left err -> Left err
  Right ((_, t) : _) -> Right [("", V "list" [t])]
  Right _ -> error "list of type inferences should never be empty"
getValueType _ _ (InfiniteList []) = Right [("", V "list" [N 0])]
getValueType fs name (InfiniteList (e : _)) = case inferTermType fs name e of
  Left err -> Left err
  Right ((_, t) : _) -> Right [("", V "list" [t])]
  Right _ -> error "list of type inferences should never be empty"
getValueType _ _ (CustomDT _ es (V n ts)) = Right (("", V n ts) : zip (map show es) ts)
getValueType _ _ (CustomDT{}) = error "This should never happen"

-- Gets the types in a tuple or custom datatype
getDTTermTypes :: LUT Term -> [String] -> [Term] -> Either String ([TermType], LUT TermType)
getDTTermTypes _ _ [] = Right ([], [])
getDTTermTypes fs name (x : xs) = case inferTermType fs name x of
  Left err -> Left err
  Right ((s, t) : ts) -> case getDTTermTypes fs name xs of
    Left err -> Left err
    Right (es, ts') -> Right (t : es, ((s, t) : ts) ++ ts')
  Right _ -> error "list of type inferences should never be empty"

checkTypes :: [Either String (LUT TermType)] -> Either String [TermType]
checkTypes [] = Right []
checkTypes (Left err : _) = Left err
checkTypes (Right ((_, t) : _) : ts) = case checkTypes ts of
  Left err -> Left err
  Right xs -> Right (t : xs)
checkTypes (Right [] : _) = error "list of type inferences should never be empty"

checkTypesSingle :: [Either String TermType] -> Either String [TermType]
checkTypesSingle = sequence

-- Adds type signatures to the dump file
addTypeSigs :: LUT Term -> [TermType] -> [(String, Term, TermType)]
addTypeSigs [] [] = []
addTypeSigs ((s, e) : es) (t : ts) = (s, e, t) : addTypeSigs es ts
addTypeSigs ((s, e) : es) [] = (s, e, N 0) : addTypeSigs es []
addTypeSigs _ _ = error "Type signatures and function declarations of different lengths"

-- Gets the current highest 'null' value used in the list of types
getCurrNull :: LUT TermType -> Int
getCurrNull [] = -1
getCurrNull ((_, N i) : xs) = max i (getCurrNull xs)
getCurrNull ((_, V _ ts) : xs) = max (getCurrNull (map ("",) ts)) (getCurrNull xs)
getCurrNull ((_, F a b) : xs) = max (max (getCurrNull [("", a)]) (getCurrNull [("", b)])) (getCurrNull xs)

-- Gets the next lowest 'null' value to be used for polymophic data types
getNextNull :: LUT TermType -> TermType
getNextNull ts = N $ getCurrNull ts + 1

getListType :: LUT TermType -> TermType
getListType ((_, V "list" (t2 : _)) : _) = t2
getListType ((_, N i) : _) = N i
getListType _ = error "list of type inferences should never be empty or list is wrong"

-- | Retrieve all the strings attached to "Var"s in the expressions.
getNames :: [Term] -> [String]
getNames [] = []
getNames (Exp (Var x) : es) = x : getNames es
getNames (Exp (Val v) : es) = getNamesVal v ++ getNames es
getNames (_ : es) = getNames es

getNamesVal :: Value -> [String]
getNamesVal (Tuple es) = getNames es
getNamesVal (List es) = getNames es
getNamesVal (ListPattern d e) = getNames [d, e]
getNamesVal (CustomDT _ es _) = getNames es
getNamesVal _ = []
