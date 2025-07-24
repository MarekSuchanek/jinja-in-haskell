module Main where

import Lib (runJinja)

main :: IO ()
main = do
  result <- runJinja
  putStrLn result
  result <- runJinja
  putStrLn result
  result <- runJinja
  putStrLn result
