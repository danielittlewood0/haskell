import System.Environment 
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler 

toTry :: IO ()
toTry = do (filename:_) <- getArgs 
           contents <- readFile filename
           putStrLn $ "That file has " ++ (show (length (lines contents))) ++ " lines"

handler :: IOError -> IO ()
handler e 
    |isDoesNotExistError e = putStrLn "Uh oh!"
    |otherwise = ioError e
