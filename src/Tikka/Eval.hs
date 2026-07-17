{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module Tikka.Eval where

import Prelude hiding (GT, LT, exp)

import Control.Monad.Writer.Lazy (Writer, runWriter, tell)

import Data.Char (isUpper)
import Data.String.Interpolate
import Tikka.Expr

gatherErrors :: Writer [String] a -> Either [String] a
gatherErrors w = case runWriter w of
  (v, []) -> Right v
  (_, errors) -> Left errors

evalTerm :: LUT Term -> Term -> Either [String] Term
evalTerm s (Exp x) = gatherErrors (evalExpr s x)
evalTerm _ (Abs var f) = Right $ Abs var f
evalTerm _ (App (Abs [] _) _) = Left ["Error: Names cannot be empty"]
evalTerm _ (App (Abs binder@(x : _) f) g)
  | isUpper x = Left ["Error: Variables cannot start with an uppercase letter\n       Only data type identifiers and constructors can begin with an uppercase letter"]
  | otherwise = Right $ betaReduce binder f g
evalTerm s (App f g) = do
  f' <- evalTerm s f
  Right $ App f' g
-- evalTerm s (Trace l) = evalTerm s l
evalTerm s (Trace l) = do
  l' <- evalTerm s l
  if l == l'
    then Right l
    else Right $ Trace l'

-- instance Eval Value where
evalValue :: LUT Term -> Value -> Either [String] Value
evalValue _ (Tuple []) = Right $ Tuple []
evalValue fs (Tuple (e : es)) = do
  e' <- evalTerm fs e
  if e /= e'
    then Right $ Tuple (e' : es)
    else
      evalValue fs (Tuple es) >>= \case
        (Tuple es') -> Right $ Tuple (e : es')
        _ -> error "Not possible"
evalValue _ (List []) = Right $ List []
evalValue fs (List (e : es)) = do
  e' <- evalTerm fs e
  if e /= e'
    then Right $ List (e' : es)
    else
      evalValue fs (List es) >>= \case
        (List es') -> Right $ List (e : es')
        _ -> error "Not possible"
evalValue fs (ListPattern d e) = do
  d' <- evalTerm fs d
  Right $ ListPattern d' e
evalValue fs (CustomDT n (e : es) t) = do
  e' <- evalTerm fs e
  if e /= e'
    then Right $ CustomDT n (e' : es) t
    else
      evalValue fs (CustomDT n es t) >>= \case
        (CustomDT _ es' _) -> Right $ CustomDT n (e : es') t
        _ -> error "Not possible"
evalValue _ x = Right x

isConcrete :: Term -> Bool
isConcrete (Abs _ _) = False
isConcrete (App _ _) = False
isConcrete (Trace e) = isConcrete e
isConcrete (Exp e) = case e of
  Var _ -> False
  Error _ -> True
  UnOp _ _ -> False
  BinOp{} -> False
  Case _ _ -> False
  Val v -> case v of
    Double _ -> True
    Char _ -> True
    Bool _ -> True
    Tuple ts -> all isConcrete ts
    List ts -> all isConcrete ts
    ListPattern _ _ -> False
    InfiniteList _ -> True -- TODO map onto evaluation?
    CustomDT _ ts _ -> all isConcrete ts

getPrimitive :: Term -> Maybe Value
getPrimitive (Trace (Exp e)) = getPrimitive (Exp e)
getPrimitive (Exp (Val v@(Double _))) = Just v
getPrimitive (Exp (Val v@(Char _))) = Just v
getPrimitive (Exp (Val v@(Bool _))) = Just v
getPrimitive _ = Nothing

-- | Just a helper to avoid writing "pure $ Exp $ ..." everywhere
done :: (Monoid a) => Expr -> Writer a Term
done = pure . Exp

-- Evaluate expression, returning either a term to be evaluated further, or a value
-- 1st arg = list of function references
-- 2nd arg = expression to evaluate
evalExpr :: LUT Term -> Expr -> Writer [String] Term
evalExpr s (Var x) = case getRef s x of
  Left e -> tell [e] >> done (Var x)
  Right y -> pure y
evalExpr s (Val x) = case evalValue s x of
  Left errors -> tell errors >> done (Val x)
  Right v -> done (Val v)
evalExpr _ (Error e) = do
  tell ["Error: " ++ e]
  done (Error e)
evalExpr s (UnOp o x) = do
  let
    appFn Negation = negate
    appFn Floor = fromIntegral @Integer @Double . floor
    appFn Ceiling = fromIntegral @Integer @Double . ceiling

  case evalTerm s x of
    Right x' -> case getPrimitive x' of
      Nothing -> pure x'
      Just v -> case evalUnOp v (appFn o) of
        Left err -> do
          tell [err]
          done (UnOp o x')
        Right d -> done (Val $ Double d)
    Left es -> tell es >> done (UnOp o x)
-- Cons and append get their own special logic
evalExpr _ (BinOp Cons x (Exp (Val (List [])))) = done $ Val $ List [x]
evalExpr fs v@(BinOp Cons x (Exp (Val (List l)))) =
  case sameType fs x (head l) of
    Left err -> tell [err] >> done v
    Right Nothing -> done $ Val $ List $ x : l
    Right (Just (t, t')) -> do
      tell ["Error: The ':' operator cannot be used on element '" ++ show x ++ "' of type '" ++ show t ++ "' and list '" ++ show (List l) ++ "' of type '[" ++ show t' ++ "]'\n       Lists can only have one type of element, and so you cannot add a value of a different type to the list. If this is needed, maybe consider using tuples"]
      done v
evalExpr fs v@(BinOp Cons x (Exp (Val (InfiniteList l)))) = case sameType fs x (head l) of
  Left err -> tell [err] >> done v
  Right Nothing -> done $ Val $ InfiniteList $ x : l
  Right (Just (t, t')) -> do
    tell ["Error: The ':' operator cannot be used on element '" ++ show x ++ "' of type '" ++ show t ++ "' and list '" ++ show (InfiniteList l) ++ "' of type '[" ++ show t' ++ "]'\n       Lists can only have one type of element, and so you cannot add a value of a different type to the list. If this is needed, maybe consider using tuples"]
    done v
evalExpr _ v@(BinOp Cons _ (Exp (Val y))) = do
  tell ["Error: ':' operator used on '" ++ show (Val y) ++ "', which is not a list\n       The second argument of the cons (:) operator must be a list containing elements of the same type as the first argument"]
  done v
evalExpr s v@(BinOp Cons x y) = do
  case evalTerm s y of
    Left es -> tell es >> done v
    Right y' -> done $ BinOp Cons x y'
evalExpr _ (BinOp Append (Exp (Val (List x))) (Exp (Val (List [])))) =
  done $ Val $ List x
evalExpr _ (BinOp Append (Exp (Val (InfiniteList x))) (Exp (Val (List [])))) =
  done $ Val $ InfiniteList x
evalExpr _ (BinOp Append (Exp (Val (List []))) (Exp (Val (List x)))) =
  done $ Val $ List x
evalExpr _ (BinOp Append (Exp (Val (List []))) (Exp (Val (InfiniteList x)))) =
  done $ Val $ InfiniteList x
evalExpr fs v@(BinOp Append (Exp (Val (List l1))) (Exp (Val (List l2)))) =
  case sameType fs (head l1) (head l2) of
    Left err -> do
      tell [err]
      done v
    Right Nothing -> done $ Val $ List $ l1 ++ l2
    Right (Just (t, t')) -> do
      tell ["Error: The '++' operator cannot be used on lists '" ++ show (List l1) ++ "' of type '[" ++ show t ++ "]' and '" ++ show (List l2) ++ "' of type '[" ++ show t' ++ "]'\n       Lists can only have one type of element, and so you cannot append a list of a different type to another list. If this is needed, maybe consider using tuples"]
      done v
evalExpr fs v@(BinOp Append (Exp (Val (List l1))) (Exp (Val (InfiniteList l2)))) =
  case sameType fs (head l1) (head l2) of
    Left err -> do
      tell [err]
      done v
    Right Nothing -> done $ Val $ InfiniteList $ l1 ++ l2
    Right (Just (t, t')) -> do
      tell ["Error: The '++' operator cannot be used on lists '" ++ show (List l1) ++ "' of type '[" ++ show t ++ "]' and '" ++ show (InfiniteList l2) ++ "' of type '[" ++ show t' ++ "]'\n       Lists can only have one type of element, and so you cannot append a list of a different type to another list. If this is needed, maybe consider using tuples"]
      done v
evalExpr fs v@(BinOp Append (Exp (Val (InfiniteList l1))) (Exp (Val (List l2)))) = case sameType fs (head l1) (head l2) of
  Left err -> tell [err] >> done v
  Right Nothing -> done $ Val $ InfiniteList $ l1 ++ l2
  Right (Just (t, t')) -> do
    tell ["Error: The '++' operator cannot be used on lists '" ++ show (InfiniteList l1) ++ "' of type '[" ++ show t ++ "]' and '" ++ show (List l2) ++ "' of type '[" ++ show t' ++ "]'\n       Lists can only have one type of element, and so you cannot append a list of a different type to another list. If this is needed, maybe consider using tuples"]
    done v
evalExpr fs v@(BinOp Append (Exp (Val (InfiniteList l1))) (Exp (Val (InfiniteList l2)))) = case sameType fs (head l1) (head l2) of
  Left err -> tell [err] >> done v
  Right Nothing -> done $ Val $ InfiniteList $ l1 ++ l2
  Right (Just (t, t')) -> do
    tell ["Error: The '++' operator cannot be used on lists '" ++ show (InfiniteList l1) ++ "' of type '[" ++ show t ++ "]' and '" ++ show (InfiniteList l2) ++ "' of type '[" ++ show t' ++ "]'\n       Lists can only have one type of element, and so you cannot append a list of a different type to another list. If this is needed, maybe consider using tuples"]
    done v
evalExpr _ v@(BinOp Append (Exp (Val (List _))) (Exp (Val y))) = do
  tell ["Error: '++' operator used on '" ++ show (Val y) ++ "', which is not a list\n       Both arguments of the append (++) operator must be lists containing elements of the same type as each other"]
  done v
evalExpr _ v@(BinOp Append (Exp (Val (InfiniteList _))) (Exp (Val y))) = do
  tell ["Error: '++' operator used on '" ++ show (Val y) ++ "', which is not a list\n       Both arguments of the append (++) operator must be lists containing elements of the same type as each other"]
  done v
evalExpr s v@(BinOp Append (Exp (Val (List l))) y) = do
  case evalTerm s y of
    Left es -> tell es >> done v
    Right y' -> done $ BinOp Append (Exp (Val (List l))) y'
evalExpr s v@(BinOp Append (Exp (Val (InfiniteList l))) y) = do
  case evalTerm s y of
    Left es -> tell es >> done v
    Right y' -> done $ BinOp Append (Exp (Val (InfiniteList l))) y'
evalExpr _ v@(BinOp Append (Exp (Val x)) _) = do
  tell ["Error: '++' operator used on '" ++ show (Val x) ++ "', which is not a list\n       Both arguments of the append (++) operator must be lists containing elements of the same type as each other"]
  done v
evalExpr s v@(BinOp Append x y) =
  case evalTerm s x of
    Left es -> tell es >> done v
    Right y' -> done $ BinOp Append y' y
evalExpr s v@(Case (Exp (Val x)) ys) =
  case evalValue s x of
    Left es -> tell es >> done v
    Right x'
      | x' /= x -> done (Case (Exp (Val x')) ys)
      | otherwise -> case runWriter (evalCase s x ys) of
          (Left es, []) -> done (Case (Exp (Val x)) es)
          (Right e, []) -> pure e
          (_, errors) -> do
            tell errors
            done $ Case (Exp (Val x)) ys
evalExpr s v@(Case x ys) =
  case evalTerm s x of
    Left es -> tell es >> done v
    Right x' -> done $ Case x' ys
evalExpr s exp@(BinOp o l r) = do
  let
    -- Numeric operators use evalValue
    evalN x y = \case
      Sum -> evalNumeric x y (+)
      Subtr -> evalNumeric x y (-)
      Product -> evalNumeric x y (*)
      Division -> evalNumeric x y (/)
      _ -> error [i|#{o} is not a numeric operation|]
    -- Comparisons use evalComp
    evalC x y = \case
      LT -> evalComp x y (<)
      LE -> evalComp x y (<=)
      GT -> evalComp x y (>)
      GE -> evalComp x y (>=)
      _ -> error [i|#{o} is not a comparison|]
    -- Boolean operators use evalBool
    evalB x y = \case
      And -> evalBool x y (&&)
      Or -> evalBool x y (||)
      _ -> error [i|#{o} is not a Boolean operation|]
    -- Equality operators use evalE (defined directly here)
    evalE x y = \case
      Eq -> Right (x == y)
      NEq -> Right (x /= y)
      _ -> error [i|#{o} is not an Equality operation|]
  case (l, r) of
    (Exp (Val x), Exp (Val y))
      -- Both left and right are ready to be added together
      | isConcrete l && isConcrete r ->
          if
            | isNumericOp o -> case evalN x y o of
                Left err -> tell [err] >> done exp
                Right d -> done $ Val $ Double d
            | isBooleanOp o -> case evalB x y o of
                Left err -> tell [err] >> done exp
                Right d -> done $ Val $ Bool d
            | isComparisonOp o -> case evalC x y o of
                Left err -> tell [err] >> done exp
                Right d -> done $ Val $ Bool d
            | isEqualityOp o -> case evalE x y o of
                Left err -> tell [err] >> done exp
                Right d -> done $ Val $ Bool d
            | otherwise -> error $ "Unhandled binary operator: " ++ show o
    -- Left is ready to be added but right is not
    (Exp (Val _), y) | isConcrete l -> do
      case evalTerm s y of
        Left es -> tell es >> done exp
        Right y' -> done (BinOp o l y')
    -- Neither left nor right are ready to be added
    (x, _) -> do
      case evalTerm s x of
        Left es -> tell es >> done exp
        Right x' -> done (BinOp o x' r)

{- | Replace variables in a term

1nd arg = replace occurrences of this variable ...

2nd arg = ... in this term ...

3rd arg = ... with this term
-}
betaReduce :: String -> Term -> Term -> Term
betaReduce x (Abs y f) l
  -- x is shadowed, nothing more to do
  | y == x = Abs y f
  | otherwise = Abs y (betaReduce x f l)
betaReduce x (App f g) l = App (betaReduce x f l) (betaReduce x g l)
betaReduce x (Trace f) l = Trace $ betaReduce x f l
betaReduce x (Exp (Var y)) l | y == x = l
betaReduce x (Exp e) l = Exp $ betaReduceExpr x e l

{- | Replace variables in a expression
1nd arg = replace occurrences of this variable ...
2nd arg = ... in this expression ...
3rd arg = ... with this term
-}
betaReduceExpr :: String -> Expr -> Term -> Expr
-- This case is handled directly in 'betaReduce'
betaReduceExpr _ (Var x) _ = Var x
betaReduceExpr var (Val x) l = betaReduceValue var x l
betaReduceExpr _ (Error e) _ = Error e
betaReduceExpr var (UnOp o x) l = UnOp o (betaReduce var x l)
betaReduceExpr var (BinOp o x y) l =
  BinOp o (betaReduce var x l) (betaReduce var y l)
betaReduceExpr var (Case x ys) l = Case (betaReduce var x l) (betaReduceCase var ys l)

-- Replace variables in a expression
-- 1nd arg = replace occurrences of this variable ...
-- 2nd arg = ... in this expression ...
-- 3rd arg = ... with this term
betaReduceCase :: String -> [(Term, Term)] -> Term -> [(Term, Term)]
betaReduceCase _ [] _ = []
betaReduceCase var ((a, b) : xs) l = (betaReduce var a l, betaReduce var b l) : betaReduceCase var xs l

-- Replace variables in a value
-- 1nd arg = replace occurrences of this variable ...
-- 2nd arg = ... in this value ...
-- 3rd arg = ... with this term
betaReduceValue :: String -> Value -> Term -> Expr
betaReduceValue var (Tuple t) l =
  Val . Tuple $ map (\x -> betaReduce var x l) t
betaReduceValue var (List ls) l =
  Val . List $ map (\x -> betaReduce var x l) ls
betaReduceValue var (CustomDT n es t) l =
  Val $ CustomDT n (map (\x -> betaReduce var x l) es) t
betaReduceValue _ x _ = Val x

-- Evaluate case expression, returning either an expression to be evaluated further, or a value
-- 1st arg = list of function references
-- 2nd arg = value to check
-- 3rd arg = list of cases
evalCase :: LUT Term -> Value -> [(Term, Term)] -> Writer [String] (Either [(Term, Term)] Term)
evalCase _ x [] = do
  tell ["Error: No case matched '" ++ show x ++ "'\n       This can be fixed by adding a specific case to match, or a wildcard (_) or variable at the end of the case statement to catch any unmatched expression"]
  pure . Left $ []
evalCase _ _ ((Exp (Var "_"), z) : _) = pure . Right $ z
evalCase fs x ((Exp (Var y), z) : ys) =
  if head y `notElem` ['A' .. 'Z']
    then pure . Right $ betaReduce y z (Exp (Val x))
    else case getRef fs y of
      Left _ -> tell ["Error: Variables cannot start with an uppercase letter\n       Only data type identifiers and constructors can begin with an uppercase letter"] >> pure (Left [])
      Right e -> pure . Left $ ((e, z) : ys)
evalCase s x ((Exp (Val y), z) : ys) = do
  m <- matched s x y
  case m of
    Nothing -> evalCase s x ys
    Just mm -> pure . Right $ replaceAll mm z
evalCase s _ v@((y, z) : ys) = do
  case evalTerm s y of
    Left errs -> tell errs >> pure (Left v)
    Right d -> pure $ Left ((d, z) : ys)

-- Replace all binders in a term, to be used when selecting a case
-- 1st arg = list of function references
-- 2nd arg = expression to search
replaceAll :: LUT Term -> Term -> Term
replaceAll [] x = x
replaceAll ((s, e) : xs) x = betaReduce s (replaceAll xs x) e

-- Determines whether a case has matched, returning either a list of variable replacements from the match or nothing, if the pattern did not match
-- 1st arg = list of function references
-- 2nd arg = the value to check
-- 3rd arg = the case to match
matched :: LUT Term -> Value -> Value -> Writer [String] (Maybe (LUT Term))
-- If the pattern is _, match anything and throw away the expression
matched s (Tuple (_ : xs)) (Tuple (Exp (Var "_") : ys)) =
  matched s (Tuple xs) (Tuple ys)
-- If the pattern is a variable 'y', do something different for uppercase vs
-- lowercase (??? is this data type handling?)
matched s (Tuple (x : xs)) (Tuple (Exp (Var y) : ys)) =
  if isUpper (head y)
    then case getRef s y of
      Left err -> tell [err] >> pure Nothing
      Right e -> matched s (Tuple (x : xs)) (Tuple (e : ys))
    else do
      m <- matched s (Tuple xs) (Tuple ys)
      case m of
        Just zs -> pure $ Just ((y, x) : zs)
        Nothing -> pure Nothing
matched s (Tuple (x : xs)) (Tuple (y : ys)) = do
  m <- matchedTerm s x y
  case m of
    Just a -> do
      m' <- matched s (Tuple xs) (Tuple ys)
      case m' of
        Just b -> pure $ Just (a ++ b)
        Nothing -> pure Nothing
    Nothing -> pure Nothing
matched s (List (_ : xs)) (List (Exp (Var "_") : ys)) = matched s (List xs) (List ys)
matched s (List (x : xs)) (List (Exp (Var y) : ys)) = do
  m <- matched s (List xs) (List ys)
  case m of
    Just zs -> pure $ Just ((y, x) : zs)
    Nothing -> pure Nothing
matched s (List (x : xs)) (List (y : ys)) = do
  m <- matchedTerm s x y
  case m of
    Just a -> do
      m' <- matched s (List xs) (List ys)
      case m' of
        Just b -> pure $ Just (a ++ b)
        Nothing -> pure Nothing
    Nothing -> pure Nothing
matched s (List (x : xs)) (ListPattern y (Exp (Var ys))) = do
  m <- matchedTerm s x y
  case m of
    Just a -> pure $ Just (a ++ [(ys, Exp . Val . List $ xs)])
    Nothing -> pure Nothing
matched s (InfiniteList (_ : xs)) (List (Exp (Var "_") : ys)) = matched s (InfiniteList xs) (List ys)
matched s (InfiniteList (x : xs)) (List (Exp (Var y) : ys)) = do
  m <- matched s (InfiniteList xs) (List ys)
  case m of
    Just zs -> pure $ Just ((y, x) : zs)
    Nothing -> pure Nothing
matched s (InfiniteList (x : xs)) (List (y : ys)) = do
  m <- matchedTerm s x y
  case m of
    Just a -> do
      m' <- matched s (InfiniteList xs) (List ys)
      case m' of
        Just b -> pure $ Just (a ++ b)
        Nothing -> pure Nothing
    Nothing -> pure Nothing
matched s (InfiniteList (x : xs)) (ListPattern y (Exp (Var ys))) = do
  m <- matchedTerm s x y
  case m of
    Just a -> pure $ Just (a ++ [(ys, Exp . Val . InfiniteList $ xs)])
    Nothing -> pure Nothing
matched s (CustomDT n es _) (CustomDT n' es' _) = if n == n' then matched s (Tuple es) (Tuple es') else pure Nothing
matched _ x y = if x == y then pure (Just []) else pure Nothing

-- For now just proxy to matchedExpr but this might be incorrect depending
-- on how functions are treated, but I think there shouldn't be any functions
-- either in the value position or the pattern position
matchedTerm :: LUT Term -> Term -> Term -> Writer [String] (Maybe (LUT Term))
matchedTerm l (Exp d) (Exp e) = matchedExpr l d e
matchedTerm _ _ _ = pure Nothing

-- Determines whether an expression has matched, returning either a list of variable replacements from the match or nothing, if the pattern did not match
-- 1st arg = list of function references
-- 2nd arg = the expression to check
-- 3rd arg = the expression to match
matchedExpr :: LUT Term -> Expr -> Expr -> Writer [String] (Maybe (LUT Term))
matchedExpr s (Val x) (Val y) = matched s x y
matchedExpr _ _ (Var "_") = pure $ Just []
matchedExpr _ x (Var y) = pure $ Just [(y, Exp x)]
matchedExpr s x y = do
  res <- evalExpr s y
  case res of
    Exp a
      | x == y -> pure $ Just []
      | a == y -> pure Nothing
      | otherwise -> matchedExpr s x a -- repeatedly simplify pattern
    _ -> pure Nothing

evalUnOp :: Value -> (Double -> Double) -> Either String Double
evalUnOp (Double x) f = Right (f x)
evalUnOp x _ = Left $ "Error: Unary operator used on " ++ show x ++ ", which is not a Double\n       Floor, Ceiling and Negation functions can only be applied to Doubles (numbers)"

-- Determines whether an arithmetic operator can be used on 2 values
-- 1st arg = 1st value
-- 2nd arg = 2nd value
-- 3rd arg = binary function
evalNumeric :: Value -> Value -> (Double -> Double -> Double) -> Either String Double
evalNumeric (Double x) (Double y) f = Right (f x y)
evalNumeric (Double _) y _ = Left $ "Error: Arithmetic operator used on " ++ show y ++ ", which is not a Double\n       Only Doubles (numbers) can be added, subtracted, multiplied and divided"
evalNumeric x (Double _) _ = Left $ "Error: Arithmetic operator used on " ++ show x ++ ", which is not a Double\n       Only Doubles (numbers) can be added, subtracted, multiplied and divided"
evalNumeric x y _ = Left $ "Error: Arithmetic operator used on " ++ show x ++ " and " ++ show y ++ ", neither of which are Doubles\n       Only Doubles (numbers) can be added, subtracted, multiplied and divided"

-- Determines whether a comparison operator can be used on 2 values
-- 1st arg = 1st value
-- 2nd arg = 2nd value
-- 3rd arg = binary operation
evalComp :: Value -> Value -> (Double -> Double -> Bool) -> Either String Bool
evalComp (Double x) (Double y) f = Right (f x y)
evalComp (Double _) y _ = Left $ "Error: Comparison operator used on " ++ show y ++ ", which is not a Double\n       Only Doubles (numbers) can be compared"
evalComp x (Double _) _ = Left $ "Error: Comparison operator used on " ++ show x ++ ", which is not a Double\n       Only Doubles (numbers) can be compared"
evalComp x y _ = Left $ "Error: Comparison operator used on " ++ show x ++ " and " ++ show y ++ ", neither of which are Doubles\n       Only Doubles (numbers) can be compared"

-- Determines whether a logical operator can be used on 2 values
-- 1st arg = 1st value
-- 2nd arg = 2nd value
-- 3rd arg = binary operation
evalBool :: Value -> Value -> (Bool -> Bool -> Bool) -> Either String Bool
evalBool (Bool x) (Bool y) f = Right (f x y)
evalBool (Bool _) y _ = Left $ "Error: Logical operator used on " ++ show y ++ ", which is not a boolean\n       Only booleans (True or False) can be compared by logical operators"
evalBool x (Bool _) _ = Left $ "Error: Logical operator used on " ++ show x ++ ", which is not a boolean\n       Only booleans (True or False) can be compared by logical operators"
evalBool x y _ = Left $ "Error: Boolean operator used on " ++ show x ++ " and " ++ show y ++ ", neither of which are booleans\n       Only booleans (True or False) can be compared by logical operators"
