module Main where
  import Language.Haskell.TH
  import Language.Haskell.TH.Syntax

  -- | print a human-readable representation of a given AST
  printAST :: ExpQ -> IO ()
  printAST  ast = runQ ast >>= putStrLn . show

  -- | print haskell code denoted by a given AST
  printCode :: ExpQ -> IO ()
  printCode ast = runQ ast >>= putStrLn . pprint

  x = printAST [| 5 |]
