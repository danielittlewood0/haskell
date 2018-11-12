main = do 
  line <- getLine
    if null line
      then return ()
    else do
      putsStrLn $ reverseWords line
      main

reverseWords :: String -> String 
reverseWords = unwords . map reverse . words
