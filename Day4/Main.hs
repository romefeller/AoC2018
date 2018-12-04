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

toGuardAction :: Maybe [String] -> GuardAction
toGuardAction Nothing = GuardAction 0 0 0 0 0 0 Sleep
toGuardAction (Just (y:m:d:h:n:a:"":[])) = 
    GuardAction (read y) (read m) (read d) (read h) (read n) 0 (readAction a) 
toGuardAction (Just (y:m:d:h:n:a:i:[])) = 
    GuardAction (read y) (read m) (read d) (read h) (read n) (read i) (readAction a)  
toGuardAction (Just _) = GuardAction 0 0 0 0 0 0 Shift

main :: IO ()
main = do 
    let r = mkRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] (falls asleep|wakes up|Guard #([0-9]+) begins shift)"
    registers <- readFile "test"
    putStrLn $ show $ map (toGuardAction . matchRegex r . T.unpack) $ T.splitOn "\n" $ T.pack registers