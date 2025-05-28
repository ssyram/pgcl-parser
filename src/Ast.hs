{-
The PCGL language Spec:

A PRISM model file defines a state-based system using modules, variables, and commands. Core components:

## ​​Modules​​
Defined via module [NAME] ... endmodule
Contain local variables and commands.
Multiple modules interact to form the global state.

## ​​Variables​​
Declared as [type] [name] [init_val]; (e.g., bool active false;, int count 0;).
Types: bool, int, double.
Scope: Local to module or global if declared outside modules.

## ​​Commands​​ (Transitions)
Format: [action_label] guard -> p1:update1 + ... + pn:updaten;
- Action Label (optional): Enables synchronization across modules.
- Guard: Boolean expression over variables (triggers the command when true).
- Updates: Probabilities/rates (p1, p2, ...) followed by variable assignments (e.g., (x'=x+1)).

Example:
[send] data_ready -> 0.8: (status'=sent) + 0.2: (status'=error);

## ​​Synchronization​​
Commands with identical action_label across modules execute synchronously (e.g., [tick] in multiple modules triggers simultaneous updates).

## ​​Constants/Formulas​​
Declare global constants: const int MAX_SIZE = 10;.
Formulas: formula is_stable = (x < 5);.

## ​​Model Type Declaration​​ (Optional)
Specify dtmc, ctmc, mdp, or pta *at the file start*.

## ​​System Composition​​
Define module interactions: system "module1" syncWith "module2" endsystem.

## ​​File Structure Example​​
```
// Model type (optional)
dtmc

// Constants
const int MAX = 5;

// Global variables
global var: bool flag false;

module ModuleA
  local_var: int count 0;
  [action] count < MAX -> 0.5: (count'=count+1) + 0.5: (count'=0);
endmodule

module ModuleB
  [action] flag == true -> 1: (flag'=false);
endmodule

// Synchronization
system "ModuleA" syncWith "ModuleB" via action endsystem
```

## ​​Key Notes​​

Updates only modify variables in their own module.
Guards may reference any variable (including other modules).
Probabilities/rates can be expressions (e.g., k*0.1).
Use + to separate parallel updates in a single command.
This structure enables AI to generate an AST with nodes for modules, variables, commands (guards/updates), synchronization labels, and system composition rules.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Ast where
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.List.NonEmpty (groupBy)
import qualified Data.List.NonEmpty as NE

-- | Model Type Declaration
data ModelType = DTMC | CTMC | MDP | PTA | NonDeterministic
  deriving (Show, Eq)

-- | Constants and Formulas
data Constant = Constant
  { constType :: Type
  , constName :: String
  , constValue :: Expr
  } deriving (Show, Eq)

data Formula = Formula
  { formulaName :: String
  , formulaExpr :: Expr
  } deriving (Show, Eq)

-- | Variable Declaration
data Variable = Variable
  { varType :: Type
  , varName :: String
  , varInit :: Maybe Expr
  } deriving (Show, Eq)

-- | Module Definition
data Module = Module
  { moduleName :: String
  , moduleVars :: [Variable]
  , moduleCommands :: [Command]
  } deriving (Show, Eq)

-- | Command (Transition)
data Command = Command
  { actionLabel :: Maybe String
  , guard :: Expr
  , updates :: [(Rational, [Update])]
  } deriving (Show, Eq)

data Update = Update
  { updateVar :: String
  , updateExpr :: Expr
  } deriving (Show, Eq)

-- | Synchronization
data Synchronization = Synchronization
  { syncModules :: (String, String)
  , syncAction :: String
  } deriving (Show, Eq)

-- | Expressions
data Expr
  = Var String
  | ConstValue String
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Lit Literal
  deriving (Show, Eq)

type Literal = Rational

data BinOp = Add | Sub | Mul | Div | And | Or | Eq | Neq | Lt | Gt | Le | Ge
  deriving (Show, Eq)

data UnOp = Not | Neg
  deriving (Show, Eq)

-- | Types
data Type = BoolType | IntType | DoubleType | IntRangeType (Expr, Expr)
  deriving (Show, Eq)

-- | Top-Level Model
data Model = Model
  { modelType :: ModelType
  , constants :: [Constant]
  , formulas :: [Formula]
  , globalVars :: [Variable]
  , modules :: [Module]
  , synchronizations :: [Synchronization]
  } deriving (Show, Eq)

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
-}

-- Firstly, define the AST of the target language:
type Var = String

-- Arithmetic and boolean expressions
data IExpr
  = EVar Var
  | ELit Rational
  -- Integer arithmetic operations
  | EAdd IExpr IExpr
  | ESub IExpr IExpr
  | EMul IExpr IExpr
  | EDiv IExpr IExpr
  | ENeg IExpr
  -- Cmp operations
  | EEq  IExpr IExpr
  | ENeq IExpr IExpr
  | ELt  IExpr IExpr
  | EGt  IExpr IExpr
  | ELe  IExpr IExpr
  | EGe  IExpr IExpr
  -- Boolean operations
  | EAnd IExpr IExpr
  | EOr  IExpr IExpr
  | ENot IExpr
  deriving (Show,Eq)

-- Statements
data Stmt
  = SAssign Var IExpr
  | SSeq [Stmt]
  | SIf IExpr Stmt Stmt
  | SWhile IExpr Stmt
  | SProb [(Rational, Stmt)]
  | SNondet [Stmt]
  deriving (Show,Eq)

type Program = Stmt

smtCompatible :: IExpr -> Bool
smtCompatible _ = undefined



{-
The conversion procedure from the Model to a program --- simply ignore the types -- assume the types in IExpr is all Rational, so types are ignored.

Before processing, the formulas & constants are expanded in the expressions, they are just like macros.

In summary, the conversion generates a program like:
// the initialization of variables
X = initX;
...
// the main loop
while (true) {
  if (g1 and g2 and ... and gn) {
    ...
  }
  else if (g1 and g2 and ... and not gn) {
    ...
  }
  // all the possible logical combination of `g1` to `gn` taking themselves and `not`
  ...
  // treat the final branch as matching none of the guards
  else {
    ...
  }
}

And in each `if` above, let the satisfied set of `gi` be `S`.
Then, the loop body of this branch is that:
  nodet {
    | ...
    | ...
  }
Each `...` in the non-det corresponds to an action label in the commands --- the updates of `S` are gathered together for all commands with the same action label in `S`.
-}

-- | IR AST for the target language

-- A program consists of global initializations and a main loop body
data IRProgram = IRProgram
  { irInitializations :: [IRAssignment]
  , irLoopBody       :: [IRPropBranch]
  } deriving (Show, Eq)

-- An assignment: variable := expression
data IRAssignment = IRAssignment
  { irAssignVar  :: String
  , irAssignExpr :: Expr
  } deriving (Show, Eq)

-- Each branch in the main loop: guarded by a proposition, contains a non-deterministic choice
data IRPropBranch = IRPropBranch
  { irGuard    :: Expr
  , irNondet   :: [IRNondet]
  } deriving (Show, Eq)

-- Each non-deterministic choice: a list of probabilistic choices
newtype IRNondet = IRNondet
  { irProbList :: [IRProb]
  } deriving (Show, Eq)

-- Each probabilistic choice: a probability and a list of assignments
data IRProb = IRProb
  { irProbValue      :: Rational
  , irProbAssignments :: [IRAssignment]
  } deriving (Show, Eq)

-- | Convert IR AST to target Program AST

-- Convert an IRProgram to a Program (Stmt)
irToProgram :: IRProgram -> Program
irToProgram (IRProgram inits branches) =
  SSeq (map irAssignToStmt inits ++ [mainLoop])
  where
    mainLoop = SWhile (ELit 1) (branchesToStmt branches) -- while (true) { ... }

-- Convert all IRPropBranch to a single Stmt (if-else chain)
branchesToStmt :: [IRPropBranch] -> Stmt
branchesToStmt =
  foldr (\branch acc -> SIf (exprToIExpr (irGuard branch)) (nondetToStmt (irNondet branch)) acc)
        (SAssign "X" (EVar "X")) -- dummy assignment for the else branch

-- Convert a list of IRNondet to a SNondet statement
nondetToStmt :: [IRNondet] -> Stmt
nondetToStmt xs = SNondet (map probListToStmt xs)

-- Convert an IRNondet (list of IRProb) to a SProb statement
probListToStmt :: IRNondet -> Stmt
probListToStmt (IRNondet probs) = SProb (map probToPair probs)
  where
    probToPair (IRProb p asgns) = (p, SSeq (map irAssignToStmt asgns))

-- Convert an IRAssignment to SAssign
irAssignToStmt :: IRAssignment -> Stmt
irAssignToStmt (IRAssignment v e) = SAssign v (exprToIExpr e)

-- Convert Expr (from IR) to IExpr (target language)
exprToIExpr :: Expr -> IExpr
exprToIExpr (Var v) = EVar v
exprToIExpr (ConstValue s) =
  case reads s of
    [(n, "")] -> ELit n
    _         -> EVar s
exprToIExpr (BinOp op e1 e2) = binOpToIExpr op (exprToIExpr e1) (exprToIExpr e2)
exprToIExpr (UnOp op e) = unOpToIExpr op (exprToIExpr e)
exprToIExpr (Lit n) = ELit n

binOpToIExpr :: BinOp -> IExpr -> IExpr -> IExpr
binOpToIExpr Add = EAdd
binOpToIExpr Sub = ESub
binOpToIExpr Mul = EMul
binOpToIExpr Div = EDiv
binOpToIExpr And = EAnd
binOpToIExpr Or  = EOr
binOpToIExpr Eq  = EEq
binOpToIExpr Neq = ENeq
binOpToIExpr Lt  = ELt
binOpToIExpr Gt  = EGt
binOpToIExpr Le  = ELe
binOpToIExpr Ge  = EGe

unOpToIExpr :: UnOp -> IExpr -> IExpr
unOpToIExpr Not = ENot
unOpToIExpr Neg = ENeg


-- | Convert a Model to an IRProgram
modelToIR :: Model -> IRProgram
modelToIR model = IRProgram { irInitializations = inits, irLoopBody = branches }
  where
    -- Global variable initializations
    inits = [ IRAssignment { irAssignVar = varName var, irAssignExpr = fromMaybe (defaultValue (varType var)) (varInit var) }
            | var <- globalVars model ]
            ++ concat [
               [ IRAssignment { irAssignVar = varName var, irAssignExpr = fromMaybe (defaultValue (varType var)) (varInit var) }
               | var <- moduleVars mod ]
            | mod <- modules model ]

    -- Default value for each type
    defaultValue (IntRangeType (lower, _)) = lower
    defaultValue BoolType = Lit 1
    defaultValue _ = Lit 0

    -- Get all guards from all commands
    allGuards = nub $ concat [[guard cmd | cmd <- moduleCommands mod] | mod <- modules model]

    -- Generate all possible combinations of guards being true/false
    guardCombinations = generateGuardCombinations allGuards

    -- Convert each combination to a branch
    branches = [ createBranch model comb | comb <- guardCombinations ]

-- | Generate all possible combinations of guards being true/false
generateGuardCombinations :: [Expr] -> [[(Expr, Bool)]]
generateGuardCombinations [] = [[]]
generateGuardCombinations (g:gs) = do
    rest <- generateGuardCombinations gs
    [ (g, True) : rest, (g, False) : rest ]

-- | Create a branch from a guard combination
createBranch :: Model -> [(Expr, Bool)] -> IRPropBranch
createBranch model guardStates = IRPropBranch
    { irGuard = combineGuards guardStates
    , irNondet = createNondetChoices model guardStates
    }

-- | Combine guards into a single expression
combineGuards :: [(Expr, Bool)] -> Expr
combineGuards [] = Lit 1
combineGuards [(g, True)] = g
combineGuards [(g, False)] = UnOp Not g
combineGuards ((g,b):gs) =
    BinOp And (if b then g else UnOp Not g) (combineGuards gs)

-- | Create non-deterministic choices for a branch
createNondetChoices :: Model -> [(Expr, Bool)] -> [IRNondet]
createNondetChoices model guardStates =
    -- Group commands by action label
    let cmds = concat [moduleCommands mod | mod <- modules model]
        matchingCmds = filter (commandMatchesGuards guardStates) cmds
        groupedCmds = groupBy (\c1 c2 -> actionLabel c1 == actionLabel c2) matchingCmds
    in [IRNondet { irProbList = [IRProb { irProbValue = p, irProbAssignments = map toAssignment upds } | (p, upds) <- updates cmd] }
       | cmdGroup <- groupedCmds
       , let cmd = NE.head cmdGroup]

-- | Check if a command matches the guard combination
commandMatchesGuards :: [(Expr, Bool)] -> Command -> Bool
commandMatchesGuards guards cmd =
    all (\(g, b) -> if b
                    then g == guard cmd
                    else UnOp Not g == guard cmd)
        guards

-- | Convert an Update to an IRAssignment
toAssignment :: Update -> IRAssignment
toAssignment upd = IRAssignment { irAssignVar = updateVar upd, irAssignExpr = updateExpr upd }
