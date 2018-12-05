{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices)
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
    | otherwise = replicate 60 0
accumAction (ga1:ga2:gas) 
    | (action ga2 == Shift && action ga1 == Shift) || action ga2 == Sleep 
        = replicate (diff ga2 ga1) 0 ++ (accumAction $ ga2:gas)
    | action ga2 == Wake = replicate (diff ga2 ga1) 1 ++ (accumAction $ ga2:gas)
    | otherwise = [] 

toGuardAction :: Maybe [String] -> GuardAction
toGuardAction Nothing = GuardAction (fromGregorian 0 0 0) 0 0 0 Sleep
toGuardAction (Just (y:m:d:h:n:a:"":[])) = 
    GuardAction (fromGregorian (read y) (read m) (read d)) (read h) (read n) 0 (readAction a) 
toGuardAction (Just (y:m:d:h:n:a:i:[])) = 
    GuardAction (fromGregorian (read y) (read m) (read d)) (read h) (read n) (read i) (readAction a)  
toGuardAction (Just _) = GuardAction (fromGregorian 0 0 0) 0 0 0 Shift

spiltAll :: [Int] -> [(Int,a)] -> [[(Int,a)]]
spiltAll is as = map (\i -> filter (\(j,_) -> j == i) as) is

withId :: (a -> a -> a) -> [(Int,a)] -> (Int,a)
withId f ls = 
    let
        (is,as) = unzip ls
    in
        (head is, foldl f (head as) (tail as))

adjust :: [GuardAction] -> [GuardAction]
adjust g@((GuardAction d h m i _):r)
    | h == 23 = (GuardAction (addDays 1 d) 0 0 i Shift):r
    | h == 0 && m /= 0 = (GuardAction d 0 0 i Shift):g
    | otherwise = g

main :: IO ()
main = do 
    let r = mkRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] (falls asleep|wakes up|Guard #([0-9]+) begins shift)"
    registers <- readFile "input"
    acts <- return $ sort $ map (toGuardAction . matchRegex r . T.unpack) $ T.splitOn "\n" $ T.pack registers
    shifts <- return $ findIndices (\x -> action x == Shift) acts
    groups <- return $ map (\g -> (ident (head g),accumAction $ adjust g)) $ 
        map (\(i,j) -> map (\k -> acts !! k) [i .. j-1]) (zip shifts (tail shifts ++ [length acts]))
    hours <- return $ map (\(i,s) -> (i, sum s)) groups
    guardsIds <- return $ nub $ map fst hours
    chosenGuard <- return $ 
        maximumBy (\(_,s1) (_,s2) -> compare s1 s2) $ (map (withId (+)) $ spiltAll guardsIds hours)
    schedule <- return $ concat $ spiltAll [fst chosenGuard] groups
    totalSchedule <- return $ snd $ withId (zipWith (+)) schedule 
    (minute:_) <- return $ elemIndices (maximum totalSchedule) totalSchedule
    print $ fst chosenGuard * minute
    scheduleP2 <- return $ spiltAll guardsIds groups
    totalScheduleP2 <- return $ map (withId (zipWith (+))) $ scheduleP2 
    maxSleep <- return $ maximum $ map maximum $ map snd totalScheduleP2
    chosenGuardP2 <- return $ filter (\(i,ls) -> elem maxSleep ls) totalScheduleP2
    print $ (fst $ head chosenGuardP2) * (head $ elemIndices maxSleep $ snd $ head chosenGuardP2)
    