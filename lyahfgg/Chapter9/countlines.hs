import System.Environment
import System.IO
import System.Directory

main = do (filename:_) <- getArgs 
          fileExists <- doesFileExist filename
          if fileExists then do
              contents <- readFile filename
              let count = length (lines contents)
              putStrLn ("The file has " ++ (show count) ++ " lines")
          else do
              putStrLn "That file doesn't exist! :("
