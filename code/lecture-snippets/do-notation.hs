{- The following are equivalent -}
getTwoLines :: IO String
getTwoLines = getLine >>= \a ->
              getLine >>= \b ->
              return (a ++ b)

-- syntactic sugar
getTwoLines' :: IO String
getTwoLines' = do a <- getLine
                  b <- getLine
                  return (a ++ b)