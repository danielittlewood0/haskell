safeHead = safe head
safeTail = safe tail
safeLast = safe last
safeInit = safe init

safe :: ([a] -> b) -> ([a] -> Maybe b)
safe f [] = Nothing
safe f xs = Just (f xs)
