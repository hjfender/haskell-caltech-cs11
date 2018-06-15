import System.Environment

-- program to print out command line arguments
main = do args <- getArgs
          prnt args
          where prnt (x:xs) =  do putStrLn x
                                  prnt xs
                prnt [] = return ""

