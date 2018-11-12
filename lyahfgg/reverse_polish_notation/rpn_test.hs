main = do 
    let (str,_) = readUntilSpace "Hello world"
    putStrLn $ str


readUntilSpace :: String -> (String,String)
readUntilSpace (x:xs) 
    |x == ' ' = ("",xs)
    |otherwise = (x:ys,zs)
        where (ys,zs) = readUntilSpace xs
