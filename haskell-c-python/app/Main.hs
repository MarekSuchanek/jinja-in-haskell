module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  result <- renderGreeting name
  print result
