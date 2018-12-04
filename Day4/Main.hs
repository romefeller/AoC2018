{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T

data Action = Sleep | Wake | Shift deriving Show

readAction :: String -> Action
readAction "falls asleep" = Sleep
readAction "wakes up" = Wake 
readAction _ = Shift

data GuardAction = GuardAction {
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    ident :: Int,
    action :: Action
} deriving Show

main :: IO ()
main = do 
    let r = mkRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] (falls asleep|wakes up|Guard #([0-9]+) begins shift)"
    registers <- readFile "input"
    putStrLn $ show $ map (matchRegex r . T.unpack) $ T.splitOn "\n" $ T.pack registers