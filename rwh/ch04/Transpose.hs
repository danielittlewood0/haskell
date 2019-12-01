transposeText :: String -> String
transposeText = unlines . transpose . lines

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs
  | all null xs = []
  | otherwise   = col:(transpose rows)
      where col   = concat $ map fst split
            rows  = map snd split
            split = map (splitAt 1) xs


