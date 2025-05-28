{-# LANGUAGE ScopedTypeVariables #-}
module Parser where

import Ast
    ( Model(..),
      Type(..),
      UnOp(Neg, Not),
      BinOp(..),
      Literal,
      Expr(Lit, BinOp, UnOp, Var),
      Synchronization(..),
      Update(..),
      Command(..),
      Module(..),
      Variable(..),
      Formula(..),
      Constant(..),
      ModelType(..) )
import Data.Void (Void)
import Control.Applicative ((<|>), many, optional)
import Text.Megaparsec (Parsec, between, choice, eof, manyTill, sepBy1, try, runParser, oneOf)
import Text.Megaparsec.Char (char, letterChar, alphaNumChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Char.Lexer (skipLineComment, skipBlockComment)
import Data.Either (lefts, rights)
import Data.Maybe (fromMaybe)



-- Megaparsec parser for the PGCL language

type Parser a = Parsec Void String a

parseFile :: String -> Either String Model
parseFile input = case runParser modelParser "" (removeComments input) of
  Left err -> Left (errorBundlePretty err)
  Right model -> Right model

-- Remove // line comments and /* ... */ block comments
removeComments :: String -> String
removeComments = removeBlockComments . removeLineComments

removeLineComments :: String -> String
removeLineComments [] = []
removeLineComments ('/':'/':xs) = removeLineComments (dropWhile (/= '\n') xs)
removeLineComments (x:xs) = x : removeLineComments xs

removeBlockComments :: String -> String
removeBlockComments [] = []
removeBlockComments ('/':'*':xs) = removeBlockComments (dropBlock xs)
  where
    dropBlock [] = []
    dropBlock ('*':'/':ys) = ys
    dropBlock (_:ys) = dropBlock ys
removeBlockComments (x:xs) = x : removeBlockComments xs

parseConfigs :: Parser ([Constant], [Formula], [Variable])
parseConfigs = do
  -- Parse any number of constants, formulas, and global variables in any order
  items <- many $  (Left          <$> try constantParser)
               <|> (Right . Left  <$> try formulaParser)
               <|> (Right . Right <$> try globalVarParser)
  let cs  = lefts items
      fs  = [f | Right (Left f) <- items]
      gvs = [v | Right (Right v) <- items]
  return (cs, fs, gvs)

moduleOrSyncsParser :: Parser ([Module], [Synchronization])
moduleOrSyncsParser = do
  items <- many $  (Left  <$> try moduleDefParser)
               <|> (Right <$> try syncParser)
  return (lefts items, rights items)

modelParser :: Parser Model
modelParser = do
  sc
  mt <- fromMaybe DTMC <$> optional modelTypeParser
  (cs,fs,gvs) <- try parseConfigs
  (ms,syns) <- try moduleOrSyncsParser
  eof
  return $ Model
    { modelType = mt
    , constants = cs
    , formulas = fs
    , globalVars = gvs
    , modules = ms
    , synchronizations = syns }

-- Space consumer
sc :: Parser ()
sc = L.space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-- Model type
modelTypeParser :: Parser ModelType
modelTypeParser = choice
  [ DTMC <$ symbol "dtmc"
  , CTMC <$ symbol "ctmc"
  , MDP  <$ symbol "mdp"
  , PTA  <$ symbol "pta"
  , NonDeterministic <$ symbol "nondeterministic"
  ]

-- Constants
constantParser :: Parser Constant
constantParser = do
  _ <- symbol "const"
  -- the constant type must be a hard type (bool, int, double) rather than a range [low..high]
  t <- hardTypeParser
  n <- identifier
  _ <- symbol "="
  v <- exprParser
  _ <- symbol ";"
  return $ Constant { constType = t, constName = n, constValue = v }

-- Formulas
formulaParser :: Parser Formula
formulaParser = do
  _ <- symbol "formula"
  n <- identifier
  _ <- symbol "="
  e <- exprParser
  _ <- symbol ";"
  return $ Formula { formulaName = n, formulaExpr = e }

varParser :: Parser Variable
varParser = do
  n <- identifier
  _ <- symbol ":"
  t <- typeParser
  v <- optional (optional (symbol "=") *> exprParser)
  _ <- symbol ";"
  return $ Variable { varType = t, varName = n, varInit = v }

-- Global variable
globalVarParser :: Parser Variable
globalVarParser = symbol "global" *> varParser

-- Type parser
hardTypeParser :: Parser Type
hardTypeParser = choice
  [ BoolType   <$ symbol "bool"
  , IntType    <$ symbol "int"
  , DoubleType <$ symbol "double"
  ]
typeRangeParser :: Parser Type
typeRangeParser = do
  _ <- symbol "["
  low <- exprParser
  _ <- symbol ".."
  high <- exprParser
  _ <- symbol "]"
  return $ IntRangeType (low, high)
typeParser :: Parser Type
typeParser = choice
  [ hardTypeParser
  , typeRangeParser
  ]

-- Module definition
moduleDefParser :: Parser Module
moduleDefParser = do
  _ <- symbol "module"
  n <- identifier
  vs <- many localVarParser
  cs <- many commandParser
  _ <- symbol "endmodule"
  return $ Module { moduleName = n, moduleVars = vs, moduleCommands = cs }

localVarParser :: Parser Variable
localVarParser = varParser

-- Command parser
commandParser :: Parser Command
commandParser = do
  al <- optional (between (char '[') (char ']') identifier)
  sc
  g <- exprParser
  _ <- symbol "->"
  us <- sepBy1 updateBranchParser (symbol "+")
  _ <- symbol ";"
  return $ Command { actionLabel = al, guard = g, updates = us }

updateBranchParser :: Parser (Rational, [Update])
updateBranchParser = do
  p <- numberParser
  _ <- symbol ":"
  sc
  us <- sepBy1 (parens updateParser) (symbol "&")
  return (p, us)

updateParser :: Parser Update
updateParser = do
  v <- identifier
  _ <- symbol "'="
  e <- exprParser
  return $ Update { updateVar = v, updateExpr = e }

-- Synchronization
syncParser :: Parser Synchronization
syncParser = do
  _ <- symbol "system"
  m1 <- stringLiteral
  sc
  _ <- symbol "syncWith"
  m2 <- stringLiteral
  sc
  _ <- symbol "via"
  act <- identifier
  _ <- symbol "endsystem"
  return $ Synchronization { syncModules = (m1, m2), syncAction = act }

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- Expressions (very basic, extend as needed)
-- exprParser :: Parser Expr
-- exprParser = makeExprParser term operatorTable

-- Generic left-associative binary operator parser generator
makeLeftAssoc :: Parser Expr -> [(String, BinOp)] -> Parser Expr
makeLeftAssoc lower ops = do
  let opParser = choice [ BinOp op <$ symbol sym | (sym, op) <- ops ]
  x <- lower
  let rest x = (do
        op <- opParser
        y <- lower
        rest (op x y)) <|> return x
  rest x

-- Operator precedence levels, from lowest to highest
operatorLevels :: [[(String, BinOp)]]
operatorLevels =
  [ [("||", Or), ("|", Or)]
  , [("&&", And), ("&", And)]
  , [("==", Eq), ("=", Eq), ("!=", Neq)]
  , [("<=", Le), (">=", Ge), ("<", Lt), (">", Gt)]
  , [("+", Add), ("-", Sub)]
  , [("*", Mul), ("/", Div)]
  ]

-- Compose levels of binary operator parsing
buildExprParser :: Parser Expr -> [[(String, BinOp)]] -> Parser Expr
buildExprParser atom [] = atom
buildExprParser atom (ops:rest) = makeLeftAssoc (buildExprParser atom rest) ops

-- Unary operator parser
unaryParser :: Parser Expr -> Parser Expr
unaryParser atom =
      (UnOp Not <$ symbol "!" <*> unaryParser atom)
  <|> (UnOp Neg <$ symbol "-" <*> unaryParser atom)
  <|> atom

exprParser :: Parser Expr
exprParser = buildExprParser (unaryParser term) operatorLevels

-- exprParser :: Parser Expr
-- exprParser =
--   parseOr
--   where
--     parseOr = do
--       x <- parseAnd
--       rest x
--       where
--         rest x = (do
--           _ <- symbol "||" <|> symbol "|"
--           y <- parseAnd
--           rest (BinOp Or x y)) <|> return x

--     parseAnd = do
--       x <- parseEq
--       rest x
--       where
--         rest x = (do
--           _ <- symbol "&&" <|> symbol "&"
--           y <- parseEq
--           rest (BinOp And x y)) <|> return x

--     parseEq = do
--       x <- parseRel
--       rest x
--       where
--         rest x = (do
--           op <- (BinOp Eq <$ choice [ symbol "==", symbol "=" ]) <|> (BinOp Neq <$ symbol "!=")
--           y <- parseRel
--           rest (op x y)) <|> return x

--     parseRel = do
--       x <- parseAdd
--       rest x
--       where
--         rest x = (do
--           op <- choice
--             [ BinOp Le <$ symbol "<="
--             , BinOp Ge <$ symbol ">="
--             , BinOp Lt <$ symbol "<"
--             , BinOp Gt <$ symbol ">"
--             ]
--           y <- parseAdd
--           rest (op x y)) <|> return x

--     parseAdd = do
--       x <- parseMul
--       rest x
--       where
--         rest x = (do
--           op <- (BinOp Add <$ symbol "+") <|> (BinOp Sub <$ symbol "-")
--           y <- parseMul
--           rest (op x y)) <|> return x

--     parseMul = do
--       x <- parseUnary
--       rest x
--       where
--         rest x = (do
--           op <- (BinOp Mul <$ symbol "*") <|> (BinOp Div <$ symbol "/")
--           y <- parseUnary
--           rest (op x y)) <|> return x

--     parseUnary =
--       (UnOp Not <$ symbol "!" <*> parseUnary)
--       <|> (UnOp Neg <$ symbol "-" <*> parseUnary)
--       <|> term

term :: Parser Expr
term = choice
  [ Var <$> identifier
  , Lit <$> literalParser
  , parens exprParser
  ]

floatParser :: Parser Rational
floatParser = do
  intPart <- L.signed sc L.decimal
  fracPart <- optional (char '.' *> L.decimal)
  expPart <- optional (oneOf "eE" *> L.signed sc L.decimal)
  let baseNum = case fracPart of
        Nothing -> intPart
        Just (fracInt :: Integer) ->
          let fracLen = length (show fracInt)
          in intPart + (fromIntegral fracInt / (10 ^ fracLen))
  return $ case expPart of
    Nothing -> baseNum
    Just (e :: Integer) -> baseNum * (10 ^ e)
intParser :: Parser Integer
intParser = L.signed sc L.decimal

numberParser :: Parser Rational
numberParser = choice
  [ try floatParser
  , toRational <$> intParser ]

literalParser :: Parser Literal
literalParser = choice
  [ (1 :: Rational) <$ symbol "true"
  , 0 <$ symbol "false"
  , numberParser
  ]
-- literalParser :: Parser Literal
-- literalParser = choice
--   [ LitInt <$> lexeme L.decimal
--   , LitDouble <$> try (toRational <$> L.float)
--   , LitBool True <$ symbol "true"
--   , LitBool False <$ symbol "false"
--   ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")




-- | Example usage
exampleModel :: String
exampleModel = unlines
  [ "dtmc"
  , "module ModuleA"
  , "  count: int /* test block comment */ 0;"
  , "  [action] (count < MAX) -> 0.5: (count'=count+1) + 0.5: (count'=0);"
  , "endmodule"
  , "module ModuleB"
  , "  [action] (flag = true) -> 1: (flag'=false);"
  , "endmodule"
  , "system \"ModuleA\" syncWith \"ModuleB\" via action endsystem"
  ]
parseExampleModel :: IO ()
parseExampleModel = case parseFile exampleModel of
  Left err -> putStrLn $ "Parse error: " ++ err
  Right model -> print model
moduleExample :: String
moduleExample = unlines
  [ "module ModuleA"
  , "  local_var: [0..MAX_VALUE-1] 0;"
  , "  [action] (count < MAX) -> 0.5: (count'=count+1) + 0.5: (count'=0);"
  , "endmodule"
  ]
tryParse :: Show a => Parser a -> String -> IO ()
tryParse parser input = case runParser parser "example" input of
  Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
  Right result -> print result
testModuleExample :: IO ()
testModuleExample = tryParse moduleDefParser moduleExample

