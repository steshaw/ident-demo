import Parser

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

main :: IO ()
main = do
  print $ parse test1
-- Right [Func "foo" ["x","y"] (Add (Var "x") (Var "y"))]
  print $ parse test2
-- Left. expecting identifier. Invalid indentation.
  print $ parse test3
-- Left. expecting identifier. Invalid indentation.
  print $ parse test4
-- Right [Func "foo" ["x","y"] (Add (Var "x") (Var "y"))]
