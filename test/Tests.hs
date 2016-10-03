{-# OPTIONS_GHC -fno-warn-orphans #-}

import AST
import qualified Parser as Orig
import qualified MegaParser as Mega

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Megaparsec as M

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P


test0 :: String
test0 = "def foo(x,y): add x y"

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

origTests :: TestTree
origTests = testGroup "Original"
  [ testCase "test0" $
      Orig.parse test0 @?= good
  , testCase "test1" $
      Orig.parse test1 @?= good
  , testCase "test2" $
      let err = case Orig.parse test2 of
                  Right _ -> error "Right unexpected"
                  Left e  -> e
      in head (P.errorMessages err) @?= bad
  , testCase "test3" $
      let err = case Orig.parse test3 of
                  Right _ -> error "Right unexpected"
                  Left e  -> e
      in head (P.errorMessages err) @?= bad
  , testCase "test4" $
      Orig.parse test4 @?= good
  ]

megaGood :: Either (M.ParseError Char M.Dec) [Expr]
megaGood = Right [Func "foo" ["x","y"] (Add (Var "x") (Var "y"))]

megaBad = "blah"

megaTests :: TestTree
megaTests = testGroup "Megaparsec"
  [ testCase "test0" $
      Mega.parse test0 @?= megaGood
  , testCase "test1" $
      Mega.parse test1 @?= megaGood
  , testCase "test2" $
      let err = case Mega.parse test2 of
                  Right _ -> error "Right unexpected"
                  Left e  -> e
      in show err @?= megaBad
  , testCase "test3" $
      let err = case Mega.parse test3 of
                  Right _ -> error "Right unexpected"
                  Left e  -> e
      in show err @?= megaBad
  , testCase "test4" $
      Mega.parse test4 @?= megaGood
  ]

tests :: TestTree
tests = testGroup "Unit tests" [origTests, megaTests]

main :: IO ()
main = defaultMain tests
