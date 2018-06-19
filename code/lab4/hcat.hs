import System.Environment
import System.IO

main = do args <- getArgs
          case args of
               [] -> interact id
               as -> processArgs as

processArgs (x:xs) = do process x
                        processArgs xs
processArgs [] = return ()

process x = do handle <- openFile x ReadMode
               contents <- hGetContents handle
               putStr contents
               hClose handle