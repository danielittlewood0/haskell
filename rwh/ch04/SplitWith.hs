splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f []   = []
splitWith f (x:xs)  = (x:head):(splitWith f tail)
  where (head,tail) = break f xs
