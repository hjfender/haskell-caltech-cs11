foo :: Maybe Int -> Maybe Int
foo x@(Just y) = x
foo Nothing = Nothing