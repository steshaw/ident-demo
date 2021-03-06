--
-- Initially cribbed from
-- http://www.zinkov.com/posts/2016-01-12-indentation-sensitive-parsing/index.html
--
-- Porting to MegaParsec with help from:
-- https://mrkkrp.github.io/megaparsec/tutorials/switch-from-parsec-to-megaparsec.html
-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
--

module MegaParser where

import AST

import Control.Applicative (empty)
import Data.Functor (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

sc :: Parser () -- `sc` stands for "space consumer"
sc = L.space (void spaceChar) lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- =================================================================

reserved :: String -> Parser ()
reserved w =
  -- lexeme (string w *> notFollowedBy identLetter)
  string w *> notFollowedBy identLetter *> sc

reservedWords :: [String]
reservedWords = ["def", "add"]

identStart :: Parser Char
identStart = letterChar <|> char '_'

identLetter :: Parser Char
identLetter = alphaNumChar <|> oneOf ['_', '\'']

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> identStart <*> many identLetter
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an indentifier"
              else pure x

integer :: Parser Integer
integer = lexeme L.integer

reservedOp :: String -> Parser ()
reservedOp = void . symbol

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
  -- Tok.parens lexer . localIndentation Any

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy1 p (symbol ",")

int :: Parser Expr
int = Lit <$> integer

add :: Parser Expr
add = reserved "add" *> (Add <$> expr <*> expr)

var :: Parser Expr
var = Var <$> identifier

app :: Parser Expr
app = App <$> var <*> parens (commaSep expr)

blockExpr :: Pos -> Parser Expr
blockExpr pos = reservedOp ":" *> L.indentGuard sc GT pos *> expr
 -- = reservedOp ":" *> localIndentation Gt (absoluteIndentation expr)

def :: Parser Expr
def = do
  pos <- L.indentLevel
  reserved "def"
  name <- identifier
  args <- parens (commaSep identifier)
  body <- blockExpr pos
  return (Func name args body)

expr :: Parser Expr
expr = def
   <|> try app
   <|> try var
   <|> try add
   <|> int
   <|> parens expr

exprs :: Parser [Expr]
exprs = many expr <* eof

-- indentConfig :: String -> ParserStream
-- indentConfig = mkIndentStream 0 infIndentation True Ge . mkCharIndentStream

parse :: String -> Either (ParseError Char Dec) [Expr]
parse = runParser exprs "[input]"
