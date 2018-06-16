import System.Environment
import System.IO

main = do args <- getArgs
          case args of
               [] -> interact id
               as -> putStr $ processArgs as (++)

processArgs :: [String] -> (String -> String -> String) -> String
processArgs (x:xs) f = do handle <- openFile x ReadMode
                          contents <- hGetContents handle
                          process f (contents):(processArgs xs f)
                          hClose handle
processArgs _ _ = fail "Invalid args"              

process :: (String -> String) -> [String] -> String
process f (l:ls) = f l (process f ls)
process _ [] = []