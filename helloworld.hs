-- IO () アクションを生成。これにmainと名前をつけるとアクションが実行される
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")