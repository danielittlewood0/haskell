import System.Environment 
import System.Directory  
import System.IO  
import Data.List 

dispatch :: [(String, [String] -> IO())]
dispatch = [("add",add),("remove",remove),("view",view)]

main = do 
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName,item] = appendFile fileName (item ++ "\n")

view :: [String] -> IO ()
view [fileName] = do 
    contents <- readFile fileName
    mapM_ putStrLn $ lines contents

remove :: [String] -> IO ()
remove [filename,numberInput] = do
    contents <- readFile filename
    let number = read numberInput
        oldLines = lines contents
        newLines = delete (oldLines !! number) oldLines
    writeFile filename (unlines newLines) 

--doesnt work - you can't write to a haskell file which is in memory!
    
    

