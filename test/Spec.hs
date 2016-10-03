import AST
import Parser

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P

test1 :: String
test1 = unlines
  [ "def foo(x,y):"
  , "    add x y"
  ]

test2 :: String
test2 = unlines
  [ "def foo(x,y):"
  , "add x y"
  ]

test3 :: String
test3 = unlines
  [ "def foo(x, y):"
  , "    add x"
  , " y"
  ]

test4 :: String
test4 = unlines
  [ "def foo(x, y):"
  , "    add x"
  , "     y"
  ]

good :: Either P.ParseError [Expr]
good = Right [Func "foo" ["x","y"] (Add (Var "x") (Var "y"))]

bad :: P.Message
bad = P.Expect "identifier"

instance Show P.Message where
  show = P.messageString

unitTests = testGroup "Unit tests"
  [ testCase "test1" $
      parse test1 @?= good
  , testCase "test2" $
      let err = case parse test2 of
                  Right a -> error "Right unexpected"
                  Left err -> err
      in head (P.errorMessages err) @?= bad
  , testCase "test3" $
      let err = case parse test3 of
                  Right a -> error "Right unexpected"
                  Left err -> err
      in head (P.errorMessages err) @?= bad
  , testCase "test4" $
      parse test4 @?= good
  ]

tests = unitTests

main :: IO ()
main = defaultMain unitTests
