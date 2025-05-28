{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Helpers where
import Data.Ratio (numerator, denominator)
import Ast
import qualified Data.Map as M
import qualified Data.Set as S
import System.Process
import GHC.IO.Handle (hPutStr)
import System.IO (hClose)
import GHC.IO.Handle (hGetContents)
import Data.List (isPrefixOf)

-- Replace atomic propositions in Expr with Atom Int, and collect mapping.
exprToAtomExpr :: Expr -> Maybe (Expr, M.Map Expr Int, M.Map Int Expr)
exprToAtomExpr expr =
    let isBoolExpr :: Expr -> Bool
        isBoolExpr (BinOp op _ _) = op `elem` [And, Or, Eq, Neq, Lt, Le, Gt, Ge]
        isBoolExpr (UnOp Not _)   = True
        isBoolExpr (Var _)        = True
        isBoolExpr (Lit n)        = n == 0 || n == 1
        isBoolExpr _otw           = False

        go :: Expr -> M.Map Expr Int -> Int -> Maybe (Expr, M.Map Expr Int, Int)
        go e m n =
          case e of
            BinOp op l r | op `elem` [And, Or] -> do
              (l', m1, n1) <- go l m n
              (r', m2, n2) <- go r m1 n1
              return (BinOp op l' r', m2, n2)
            UnOp Not e1 -> do
              (e1', m1, n1) <- go e1 m n
              return (UnOp Not e1', m1, n1)
            BinOp op l r | op `elem` [Eq, Neq, Lt, Le, Gt, Ge] -> insertAtom (BinOp op l r) m n
            Lit x -> insertAtom (Lit x) m n
            Var v -> insertAtom (Var v) m n
            _ | isBoolExpr e -> insertAtom e m n
              | otherwise -> Nothing

        insertAtom :: Expr -> M.Map Expr Int -> Int -> Maybe (Expr, M.Map Expr Int, Int)
        insertAtom atom m n =
          case M.lookup atom m of
            Just i  -> return (Var ("#atom" ++ show i), m, n)
            Nothing -> return (Var ("#atom" ++ show n), M.insert atom n m, n+1)
    in
    case go expr M.empty 1 of
      Just (e', m, _) ->
        let im = M.fromList [ (i, e) | (e, i) <- M.toList m ]
        in Just (e', m, im)
      Nothing -> Nothing

-- | Given an Expr (with atoms replaced), convert to DNF as a set of clauses.
-- Each clause is a set of signed Ints: positive for atom, negative for negated atom.
exprToDNFClauses :: Expr -> S.Set (S.Set Int)
exprToDNFClauses (BinOp Or l r) =
    S.union (exprToDNFClauses l) (exprToDNFClauses r)
exprToDNFClauses (BinOp And l r) =
    S.fromList [ S.union c1 c2 | c1 <- S.toList (exprToDNFClauses l)
                               , c2 <- S.toList (exprToDNFClauses r) ]
exprToDNFClauses (UnOp Not (Var v))
  | Just n <- atomVarToInt v = S.singleton (S.singleton (-n))
exprToDNFClauses (Var v)
  | Just n <- atomVarToInt v = S.singleton (S.singleton n)
-- This is `true`
exprToDNFClauses (Lit 1) = S.singleton S.empty
-- This is `false`
exprToDNFClauses (Lit 0) = S.empty
exprToDNFClauses _ = S.empty

-- | Helper: extract atom int from variable name "#atomN"
atomVarToInt :: String -> Maybe Int
atomVarToInt v =
  case v of
    ('#':'a':'t':'o':'m':rest) -> case reads rest of
                                    [(n,"")] -> Just n
                                    _        -> Nothing
    _ -> Nothing

-- | Top-level: Convert Expr to DNF as set of clauses over Int atoms.
-- Returns (clauses, atom mapping, inverse mapping)
exprToDNFWithAtoms :: Expr -> Maybe (S.Set (S.Set Int), M.Map Expr Int, M.Map Int Expr)
exprToDNFWithAtoms e = do
  (e', m, im) <- exprToAtomExpr e
  let clauses = exprToDNFClauses e'
  return (clauses, m, im)

-- | Simplifies a set of DNF clauses by removing redundant clauses and merging equivalent ones.
simplyfyDnf :: S.Set (S.Set Int) -> S.Set (S.Set Int)
simplyfyDnf clauses =
  let -- Remove clauses with both a variable and its negation (contradictory clauses)
      noContradictory = S.filter (\c -> any (\x -> S.member (-x) c) c) clauses
      -- If any clause is empty, the DNF is always true: return only the empty clause
      result
        | any S.null noContradictory = S.singleton S.empty
        | otherwise = noContradictory
  in result

-- | Simplifies the Expr by converting to DNF and removing redundant clauses and then converting back to Expr.
simplifyExpr :: Expr -> Maybe Expr
simplifyExpr expr = do
  (clauses, _, im) <- exprToDNFWithAtoms expr
  let simplified = simplyfyDnf clauses
      findAtom i = case M.lookup i im of
        Just e -> e
        Nothing -> error $ "Atom with index " ++ show i ++ " not found in mapping."
  let toExpr clause = case S.toList clause of
        [] -> Lit 1  -- Empty clause means True
        xs -> foldr1 (BinOp And)
          [if i > 0 then findAtom i else UnOp Not $ findAtom (-i) | i <- xs]
  let result = case S.toList simplified of
        [] -> Lit 0  -- Empty set means False
        [c] | S.null c -> Lit 1  -- Single empty clause means True
        xs -> let exprs = map toExpr xs in
              if Lit 1 `elem` exprs
              then Lit 1
              else foldr1 (BinOp Or) exprs
  return result

-- | The function `toSmtLib2` converts the `Expr` with the variable types to a SMT-LIB2 compatible string representation.
--   The string is made up of variable declarations and the expression itself.
--   The first part might be ignored if is `Nothihng`.
toSmtLib2 :: Maybe [(Var,Type)] -> Expr -> String
toSmtLib2 mvars expr =
  -- If the type is a Range, we should also declare its range in the SMT-LIB2 format.
  let decls = case mvars of
        Nothing -> ""
        Just vars -> unlines [ "(declare-fun " ++ v ++ " () " ++ typeToSmt t ++ ")" | (v, t) <- vars ]
      body = exprToSmt expr
  in decls
  ++ maybe [] (concatMap (\(v, t) -> case t of
        IntRangeType (lo, hi) ->
          let vstr = v
              loStr = exprToSmt lo
              hiStr = exprToSmt hi
          in "(assert (and (<= " ++ loStr ++ " " ++ vstr ++ ") (<= " ++ vstr ++ " " ++ hiStr ++ ")))\n"
        _ -> ""
     )) mvars ++ "(assert " ++ body ++ ")"
  where
    typeToSmt BoolType = "Bool"
    typeToSmt IntType = "Int"
    typeToSmt DoubleType = "Real"
    typeToSmt (IntRangeType _) = "Int"
    exprToSmt (Var v) = v
    -- exprToSmt (ConstValue s) =
    --   case reads s of
    --     [(n :: Rational, "")] -> rationalToSmt n
    --     _ -> s
    exprToSmt (Lit n) = rationalToSmt n
    exprToSmt (BinOp op e1 e2) = "(" ++ binOpToSmt op ++ " " ++ exprToSmt e1 ++ " " ++ exprToSmt e2 ++ ")"
    exprToSmt (UnOp op e) = "(" ++ unOpToSmt op ++ " " ++ exprToSmt e ++ ")"
    binOpToSmt Add = "+"
    binOpToSmt Sub = "-"
    binOpToSmt Mul = "*"
    binOpToSmt Div = "div"
    binOpToSmt And = "and"
    binOpToSmt Or  = "or"
    binOpToSmt Eq  = "="
    binOpToSmt Neq = "distinct"
    binOpToSmt Lt  = "<"
    binOpToSmt Gt  = ">"
    binOpToSmt Le  = "<="
    binOpToSmt Ge  = ">="
    unOpToSmt Not = "not"
    unOpToSmt Neg = "-"
    rationalToSmt n
      | denominator n == 1 = show (numerator n)
      | otherwise = "(/ " ++ show (numerator n) ++ " " ++ show (denominator n) ++ ")"

-- | Call z3 to verify an SMT-LIB2 formula. Returns True if satisfiable, False if unsatisfiable.
verifyWithZ3 :: String -> IO Bool
verifyWithZ3 smt = do
  -- Debug: add a check-sat and exit command to the SMT-LIB2 string
  smt <- return $ smt ++ "\n(check-sat)\n(exit)\n"
  putStrLn $ "Verifying with Z3:\n" ++ smt
  putStrLn ""
  (Just hin, Just hout, _, ph) <- createProcess (proc "z3" ["-smt2", "-in"]) { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin smt
  hClose hin
  result <- hGetContents hout
  _ <- waitForProcess ph
  putStrLn $ "Z3 result: " ++ result
  return $ "sat" `isPrefixOf` result

-- | Convenience function to verify an expression directly
verifyExpr :: Maybe [(Var,Type)] -> Expr -> IO Bool
verifyExpr vars expr = verifyWithZ3 (toSmtLib2 vars expr)

-- | Example usage of verifyWithZ3
example :: IO Bool
example = do
  let expr = BinOp And 
        (BinOp Lt (Var "x") (Lit 5))
        (BinOp Gt (Var "x") (Lit 0))
      vars = Just [("x", IntType)]
  verifyWithZ3 (toSmtLib2 vars expr)
