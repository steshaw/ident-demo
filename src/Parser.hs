--
-- Cribbed from
-- http://www.zinkov.com/posts/2016-01-12-indentation-sensitive-parsing/index.html
--

module Parser where

import           Data.Functor                  ((<$>), (<$))
import           Control.Applicative           (Applicative(..))
import qualified Control.Monad                 as M
import           Data.Functor.Identity
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Text.Parsec                   hiding (Empty)
import           Text.Parsec.Text              () -- instances only
import qualified Text.Parsec.Expr              as Ex
import qualified Text.Parsec.Token             as Tok

import           Text.Parsec.Indentation
import           Text.Parsec.Indentation.Char
import qualified Text.Parsec.Indentation.Token as ITok

style :: Tok.GenLanguageDef ParserStream st Identity
style = ITok.makeIndentLanguageDef $ Tok.LanguageDef
    { Tok.commentStart    = ""
    , Tok.commentEnd      = ""
    , Tok.nestedComments  = True
    , Tok.identStart      = letter <|> char '_'
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf "!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf "!#$%&*+./<=>?@\\^|-~"
    , Tok.caseSensitive   = True
    , Tok.commentLine     = "#"
    , Tok.reservedOpNames = [":"]
    , Tok.reservedNames   = ["def", "add"]
    }

lexer :: Tok.GenTokenParser ParserStream () Identity
lexer = ITok.makeTokenParser style

integer :: Parser Integer
integer = Tok.integer lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer . localIndentation Any

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

type Name = String
type Args = [Name]

type ParserStream    = IndentStream (CharIndentStream String)
type Parser          = ParsecT     ParserStream () Identity

data Expr
   = Func Name Args Expr
   | Var  Name
   | App  Expr [Expr]
   | Add  Expr Expr
   | Lit  Integer
   deriving (Show)

int :: Parser Expr
int = Lit <$> integer

add :: Parser Expr
add = reserved "add" *> (Add <$> expr <*> expr)

var :: Parser Expr
var = Var <$> identifier

app :: Parser Expr
app = App <$> var <*> parens (commaSep expr)

def :: Parser Expr
def = do
  reserved "def"
  name <- identifier
  args <- parens (commaSep identifier)
  body <- reservedOp ":" *> expr
  return (Func name args body)

expr :: Parser Expr
expr = def
   <|> try app
   <|> try var
   <|> try add
   <|> int
   <|> parens expr

indentConfig :: String -> ParserStream
indentConfig =
    mkIndentStream 0 infIndentation True Ge . mkCharIndentStream

parse :: String -> Either ParseError [Expr]
parse =
    runParser (many expr <* eof) () "[input]" . indentConfig
