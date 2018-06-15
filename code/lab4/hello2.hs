main = do putStrLn "Please enter your name:"
          name <- getLine
          putStrLn $ "hello, " ++ name ++ "!"