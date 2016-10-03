import Parser

test1 = unlines
  [ "def foo(x,y):"
  , "    add x y"
  ]


test2 = unlines
  [ "def foo(x,y):"
  , "add x y"
  ]


main :: IO ()
main = do
  print $ parse test1
-- Right [Func "foo" ["x","y"] (Add (Var "x") (Var "y"))]
  print $ parse test2
-- Right [Func "foo" ["x","y"] (Add (Var "x") (Var "y"))]
