module Main where 

import Data.Char
import Control.Applicative

reduce :: String -> String
reduce = foldl (\e c -> if abs(ord c - ord (head e)) == 32 then (tail e) else c:e) "@"

--- PART 2 ---
filterChar :: Char -> String -> String
filterChar x = filter (/= x) . filter (/= (chr((ord x) + 32)))

exec :: Char -> String -> String
exec c s = (tail . reverse) $ reduce $ filterChar c s
-------------
main :: IO ()
main = do 
    polymer <- readFile "input"
    print $ length $ (tail . reverse) $ reduce polymer
    print $ minimum . map length $ (exec <$> ['A'..'Z']) <*> [polymer]