{-# OPTIONS_GHC -fno-warn-orphans #-}

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

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "test1" $
      parse test1 @?= good
  , testCase "test2" $
      let err = case parse test2 of
                  Right _ -> error "Right unexpected"
                  Left e  -> e
      in head (P.errorMessages err) @?= bad
  , testCase "test3" $
      let err = case parse test3 of
                  Right _ -> error "Right unexpected"
                  Left e  -> e
      in head (P.errorMessages err) @?= bad
  , testCase "test4" $
      parse test4 @?= good
  ]

tests :: TestTree
tests = unitTests

main :: IO ()
main = defaultMain tests
