--this demonstrates case expressions
zeros :: [Int] -> [Int]
zeros lst =
    case lst of
        (_ : rest) -> 0 : zeros rest
        [] -> []