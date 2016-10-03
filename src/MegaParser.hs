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

{-
import           Data.Functor.Identity
import           Text.Parsec                   hiding (Empty)
import           Text.Parsec.Text              () -- instances only
import qualified Text.Parsec.Token             as Tok

import           Text.Parsec.Indentation
import           Text.Parsec.Indentation.Char
import qualified Text.Parsec.Indentation.Token as ITok
-}

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

{-
style :: Tok.GenLanguageDef ParserStream st Identity
style = ITok.makeIndentLanguageDef Tok.LanguageDef
    , Tok.opStart         = oneOf "!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf "!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedOpNames = [":"]
    }
-}

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

blockExpr :: Parser Expr
blockExpr = reservedOp ":" *> expr
 -- = reservedOp ":" *> localIndentation Gt (absoluteIndentation expr)

def :: Parser Expr
def = do
  reserved "def"
  name <- identifier
  args <- parens (commaSep identifier)
  body <- blockExpr
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
