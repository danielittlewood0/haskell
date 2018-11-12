main = do 
  line <- getLine
    if null line
      then return ()
    else do
      putsStrLn $ reverseWords line
      main

reverseWords :: String -> String 
reverseWords = unwords . map reverse . words

threeCoins :: StdGen -> (Bool,Bool,Bool)
threeCoins = 
    let (c1,g1) = random g
        (c2,g2) = random g1
        (c3,g3) = random g2
    in (c1,c2,c3)

nRandoms :: (Random a,Num n,RandomGen g) => n -> g -> ([a],g)
nRandoms 0 g = ([],g)
nRandoms n g = 
    let (a,g') = (random g)
    in a:(nRandoms n-1 g')
