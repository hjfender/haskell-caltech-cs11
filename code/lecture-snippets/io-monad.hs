-- DO NOT COMPILE
-- Only type definitions
return :: a -> IO a

fail :: String -> IO a

(>>=) :: IO a -> (a -> IO b) -> IO b

(>>) :: IO a -> IO b -> IO b
a >> b = a >>= \_ -> b