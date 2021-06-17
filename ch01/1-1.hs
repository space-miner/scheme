module Main where
import System.Environment

main :: IO ()
main = do args <- getArgs
          putStrLn $ show $ (args !! 0) ++ " " ++ (args !! 1)

