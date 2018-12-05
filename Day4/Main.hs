{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort)
import Data.Time.Calendar

data Action = Sleep | Wake | Shift deriving (Eq,Show)

readAction :: String -> Action
readAction "falls asleep" = Sleep
readAction "wakes up" = Wake 
readAction _ = Shift

data GuardAction = GuardAction {
    time :: Day,
    hour :: Int,
    minute :: Int,
    ident :: Int,
    action :: Action
} deriving (Eq, Show)

diff :: GuardAction -> GuardAction -> Int
diff (GuardAction d1 h1 m1 _ _) (GuardAction d2 h2 m2 _ _) =
    fromInteger((diffDays d1 d2)*24*60) + (h1-h2)*60 + (m1-m2)

instance Ord GuardAction where 
    (GuardAction d1 h1 m1 _ _) <= (GuardAction d2 h2 m2 _ _) = 
        (d1 < d2) ||
        (d1 == d2 && h1 <= h2 && m1 <= m2 )

accumAction :: [GuardAction] -> [Int]
accumAction [] = []
accumAction (g:[]) 
    | action g == Sleep = replicate (diff (GuardAction (time g) 1 0 0 Shift) g) 1 
    | action g == Wake = replicate (diff (GuardAction (time g) 1 0 0 Shift) g) 0
accumAction (ga1:ga2:gas) 
    | action ga2 == Sleep = replicate (diff ga2 ga1) 0 ++ (accumAction $ ga2:gas)
    | action ga2 == Wake = replicate (diff ga2 ga1) 1 ++ (accumAction $ ga2:gas)
    | otherwise = [] 

toGuardAction :: Maybe [String] -> GuardAction
toGuardAction Nothing = GuardAction (fromGregorian 0 0 0) 0 0 0 Sleep
toGuardAction (Just (y:m:d:h:n:a:"":[])) = 
    GuardAction (fromGregorian (read y) (read m) (read d)) (read h) (read n) 0 (readAction a) 
toGuardAction (Just (y:m:d:h:n:a:i:[])) = 
    GuardAction (fromGregorian (read y) (read m) (read d)) (read h) (read n) (read i) (readAction a)  
toGuardAction (Just _) = GuardAction (fromGregorian 0 0 0) 0 0 0 Shift

main :: IO ()
main = do 
    let r = mkRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] (falls asleep|wakes up|Guard #([0-9]+) begins shift)"
    registers <- readFile "test.1"
    acts <- return $ sort $ map (toGuardAction . matchRegex r . T.unpack) $ T.splitOn "\n" $ T.pack registers
    print $ show $ acts
    print $ accumAction acts