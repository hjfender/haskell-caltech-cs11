import System.Environment
import System.IO

{- not strictly the same as uniq; see the note below about unlines -}
main = do args <- getArgs
          processArgs args uniqueElementList

processArgs :: [String] -> ([String] -> [String]) -> IO ()
processArgs [] f = interact (process f)
processArgs (a:[]) f = do handle <- openFile a ReadMode
                          contents <- hGetContents handle
                          putStr ((process f) contents)
                          hClose handle
processArgs _ _ = fail "Invalid args"              

process :: ([String] -> [String]) -> String -> String
process f s = unlines $ f (lines s) --unlines adds '\n' to end if it isn't there already

uniqueElementList :: Eq a => [a] -> [a]
uniqueElementList (l1:l2:ls) = if l1 == l2
                    then uniqueElementList $ l1:ls
                    else l1:(uniqueElementList $ l2:ls)
uniqueElementList l = l