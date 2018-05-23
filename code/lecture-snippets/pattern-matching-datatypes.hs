--this demonstrates pattern matching on algebraic datatypes
foo :: Maybe Int -> Int
foo Nothing = 0
foo (Just x) = 1 + x

bar :: Maybe (Maybe String) - String
bar Nothing = "None"
bar (Just Nothing) = "Sorta"
bar (Just (Just x)) = "Yes: " ++ x