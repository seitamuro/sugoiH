main = do
  a <- return "a"
  b <- return "b"
  putStrLn $ a ++ " " ++ b