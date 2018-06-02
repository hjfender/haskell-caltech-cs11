import System.IO

sequence_ :: [IO ()] -> IO ()
sequence_ [] = return ()
sequence_ (a:as) = do a
                      sequence_ as

sequence' :: [IO ()] -> IO ()
sequence' = foldr (>>) (return ())

putStr :: String -> IO ()
putStr s = sequence' (map putChar s)

getChar' :: IO Char
getChar' = getChar `catch` eofHandler where
    eofHandler e = if is EofError e then return '\n' else ioError e

getLine' :: IO String
getLine' = catch getLine'' (\err -> return ("Error: " ++ show err))
                where getLine'' = do c <- getChar'
                                  if c == '\n' then return ""
                                               else do l <- getLine'
                                               return (c:l)