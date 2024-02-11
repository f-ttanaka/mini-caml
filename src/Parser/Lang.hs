module Parser.Lang where

import           Data.Expr
import           Data.Foldable         (Foldable (foldr'))
import           Data.Text.Lazy        (Text)
import           Parser.Combinator
import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)

intLit :: Parser Expr
intLit = LInt . fromIntegral <$> natural

boolLit :: Parser Expr
boolLit = LBool
  <$> (True <$ reserved "true"
      <|> False <$ reserved "false")

-- listExpr :: Parser Expr
-- listExpr = List
--   <$> brackets (commaSep expr)

varExpr :: Parser Expr
varExpr = Var <$> identifier

ifExpr :: Parser Expr
ifExpr = If
  <$> (reserved "if" *> expr)
  <*> (reservedOp "then" *> expr)
  <*> (reserved "else" *> expr)

letBind :: Parser (Name,Expr)
letBind = do
  f <- identifier
  xs <- many identifier
  reservedOp "="
  e <- expr
  return (f, foldr' Lambda e xs)

letExpr :: Parser Expr
letExpr = LetExpr
  <$> (reserved "let" *> option False (reserved "rec" >> return True))
  <*> sepBy1 letBind (symbol "and")
  <*> (reserved "in" *> expr)

lambda :: Parser Expr
lambda = Lambda
  <$> (reserved "fn" *> identifier <* reservedOp "=>")
  <*> expr

aexpr :: Parser Expr
aexpr = parens expr
  <|> intLit
  <|> boolLit
  -- <|> listExpr
  <|> ifExpr
  <|> letExpr
  <|> lambda
  <|> varExpr

term :: Parser Expr
term = aexpr >>= \x ->
  (many1 aexpr >>= \xs -> return (foldl App x xs))
  <|> return x


table :: Operators Expr
table = [
    [
      infixOpAL "*" (\e1 e2 -> Prim (Arith (*) e1 e2))
    ],
    [
      infixOpAL "+" (\e1 e2 -> Prim (Arith (+) e1 e2)),
      infixOpAL "-" (\e1 e2 -> Prim (Arith (-) e1 e2))
    ],
    [
      infixOpAL "==" (\e1 e2 -> Prim (Comp (==) e1 e2)),
      infixOpAL "<" (\e1 e2 -> Prim (Comp (<) e1 e2))
    ]
  ]

expr :: Parser Expr
expr = contents table term

parseExpr :: FilePath -> Text -> Either ParseError Expr
parseExpr = parse expr

-- statement
letStmt :: Parser Stmt
letStmt = LetStmt
  <$> (reserved "let" *> option False (reserved "rec" >> return True))
  <*> sepBy1 letBind (symbol "and")

stmt :: Parser Stmt
stmt = try (ExprStmt <$> expr) <|> letStmt

parseStmt :: FilePath -> Text -> Either ParseError Stmt
parseStmt = parse stmt
