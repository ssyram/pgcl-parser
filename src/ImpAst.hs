{-# LANGUAGE LambdaCase #-}
module ImpAst where
import Data.Ratio (denominator)
import GHC.Real (numerator)

{- The target language to convert to:

Example:
  Y = 0;
  X = 0;
  prob {
      0.5 : C = 0;
      0.5 : C = C;
  }
  while (C > 0) {
      C = C - 1;
      nondet {
          | Y = Y + 1;
          | Y = Y;
      }
      prob {
          0.5 : X = X + 1;
          0.5 : X = X;
      }
      if (Y >= 1) {
          while (true) { X = X; }
      } else {
          X = X;
      }
  }

A usual imperative language with a few extensions:
-- 1. Probabilistic choice with `prob { ... }` blocks.
-- 2. Non-deterministic choice with `nondet { ... }` blocks.
-- 3. While loops with `while (condition) { ... }`.
-- 4. If statements with `if (condition) { ... } else { ... }` -- the else branch is mandatory -- add dummy statements like `X = X;` when the branch is empty.
-- 5. Variable assignments with `var = expr;`.
-- 6. Expressions can include arithmetic operations, comparisons, and logical operations.
-- 7. There is no concept of type in the language -- all types are assumed to be integer.
-}

-- Variable names
type Var = String

-- Arithmetic and boolean expressions
data Expr
  = EVar Var
  | ELit Rational
  -- Integer arithmetic operations
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | ENeg Expr
  -- Cmp operations
  | EEq  Expr Expr
  | ENeq Expr Expr
  | ELt  Expr Expr
  | EGt  Expr Expr
  | ELe  Expr Expr
  | EGe  Expr Expr
  -- Boolean operations
  | EAnd Expr Expr
  | EOr  Expr Expr
  | ENot Expr
  deriving (Eq)

instance Show Expr where
  showsPrec p expr = case expr of
    EVar v       -> showString v
    ELit n       -> showRational n
    EAdd a b     -> showParen (p > 6) $
                      showsPrec 6 a . showString " + " . showsPrec 6 b
    ESub a b     -> showParen (p > 6) $
                      showsPrec 6 a . showString " - " . showsPrec 7 b
    EMul a b     -> showParen (p > 7) $
                      showsPrec 7 a . showString " * " . showsPrec 7 b
    EDiv a b     -> showParen (p > 7) $
                      showsPrec 7 a . showString " / " . showsPrec 8 b
    ENeg a       -> showParen True $
                      showString "-" . showsPrec 9 a
    EEq a b      -> showParen (p > 4) $
                      showsPrec 5 a . showString " == " . showsPrec 5 b
    ENeq a b     -> showParen (p > 4) $
                      showsPrec 5 a . showString " != " . showsPrec 5 b
    ELt a b      -> showParen (p > 4) $
                      showsPrec 5 a . showString " < " . showsPrec 5 b
    EGt a b      -> showParen (p > 4) $
                      showsPrec 5 a . showString " > " . showsPrec 5 b
    ELe a b      -> showParen (p > 4) $
                      showsPrec 5 a . showString " <= " . showsPrec 5 b
    EGe a b      -> showParen (p > 4) $
                      showsPrec 5 a . showString " >= " . showsPrec 5 b
    EAnd a b     -> showParen (p > 3) $
                      showsPrec 4 a . showString " && " . showsPrec 4 b
    EOr a b      -> showParen (p > 2) $
                      showsPrec 3 a . showString " || " . showsPrec 3 b
    ENot a       -> showString "!" . showsPrec 9 a

showRational :: Rational -> ShowS
showRational r
  | denominator r == 1 = shows (numerator r)
  | otherwise = shows (fromRational r :: Double)

-- Statements
data Stmt
  = SAssign Var Expr
  | SSeq [Stmt]
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SProb [(Rational, Stmt)]
  | SNondet [Stmt]
  deriving (Eq)


instance Show Stmt where
  showsPrec _ stmt = case stmt of
    SAssign v e ->
      showString v . showString " = " . shows e . showString ";"
    SSeq stmts ->
      case stmts of
        [] -> id
        (x : lst) ->
          foldl (\acc s -> acc . showString "\n" . shows s) (shows x) lst
    SIf cond thn els ->
      showString "if (" . shows cond . showString ") {\n"
      . indent (shows thn) . showString "\n"
      . showString "} else {\n"
      . indent (shows els) . showString "\n"
      . showString "}"
    SWhile cond body ->
      showString "while (" . shows cond . showString ") {\n"
      . indent (shows body) . showString "\n"
      . showString "}"
    SProb branches ->
      showString "prob {\n"
      . foldr (\(p, s) acc ->
          showString "    " . showRational p . showString " : "
          . singleLineOrBlock s . showString "\n" . acc
        ) id branches
      . showString "}"
    SNondet stmts ->
      showString "nondet {\n"
      . foldr (\s acc ->
          showString "    | " . singleLineOrBlock s . showString "\n" . acc
        ) id stmts
      . showString "}"
    where
      indent f = showString "    " . replaceNewlines (f "")
      replaceNewlines [] = id
      replaceNewlines ('\n':xs) = showString "\n    " . replaceNewlines xs
      replaceNewlines (x:xs) = showChar x . replaceNewlines xs
      singleLineOrBlock s =
        case s of
          SSeq _ ->    showString "\n" . indent (indent (shows s))
          SIf{}  ->    showString "\n" . indent (indent (shows s))
          SWhile{} ->  showString "\n" . indent (indent (shows s))
          SProb{} ->   showString "\n" . indent (indent (shows s))
          SNondet{} -> showString "\n" . indent (indent (shows s))
          _      ->    shows s

-- | Notably, the Expr stores values as Rational, but a condition is a boolean expression.
--   This function trys to convert an Expression in a readable way.
showsCond :: Expr -> ShowS
showsCond = \case
  ELit n | n == 0 -> showString "false"
         | otherwise -> showString "true"
  other -> shows other

-- A program is just a statement
type Program = Stmt

-- Example of printing a program
exampleProgram :: Program
exampleProgram =
  SSeq
    [ SAssign "Y" (ELit 0)
    , SAssign "X" (ELit 0)
    , SProb
        [ (0.5, SSeq [SAssign "C" (ELit 0), SAssign "C" (EVar "C")])
        , (0.5, SAssign "C" (EVar "C"))
        ]
    , SWhile (EGt (EVar "C") (ELit 0))
        (SSeq
          [ SAssign "C" (ESub (EVar "C") (ELit 1))
          , SNondet
              [ SAssign "Y" (EAdd (EVar "Y") (ELit 1))
              , SAssign "Y" (EVar "Y")
              ]
          , SProb
              [ (0.5, SAssign "X" (EAdd (EVar "X") (ELit 1)))
              , (0.5, SAssign "X" (EVar "X"))
              ]
          , SIf (EGe (EVar "Y") (ELit 1))
              (SWhile (ELit 1) (SAssign "X" (EVar "X")))
              (SAssign "X" (EVar "X"))
          ]
        )
    ]

