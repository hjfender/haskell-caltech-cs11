import System.Environment
import System.IO

{- not strictly the same as uniq; see the note below about unlines -}
main = do args <- getArgs
          processArgs args quicksort

processArgs :: [String] -> ([String] -> [String]) -> IO ()
processArgs [] f = interact (process f)
processArgs (a:[]) f = do handle <- openFile a ReadMode
                          contents <- hGetContents handle
                          putStr ((process f) contents)
                          hClose handle
processArgs _ _ = fail "Invalid args"              

process :: ([String] -> [String]) -> String -> String
process f s = unlines $ f (lines s) --unlines adds '\n' to end if it isn't there already

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort lt ++ [x] ++ quicksort ge
    where
        lt = [y | y <- xs, y < x]
        ge = [y | y <- xs, y >= x]