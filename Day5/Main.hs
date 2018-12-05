module Main where 

import Data.Char

reduce :: String -> String
reduce = foldl (\e c -> if abs(ord c - ord (head e)) == 32 then (tail e) else c:e) "@"

main :: IO ()
main = do 
    polymer <- readFile "input"
    print $ length $ (tail . reverse) $ reduce polymer