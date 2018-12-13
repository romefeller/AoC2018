{-# LANGUAGE OverloadedStrings #-} 
module Main where 

import qualified Data.Text as T 
import Data.List
import Control.Monad.State
import Control.Monad.Trans.Class

data Bearing = L | R | U | D | S deriving Show

data Kart = Kart{
  dir :: Bearing,
  pos :: (Int,Int),
  inter :: Bearing
} deriving Show

instance Eq Kart where 
    (Kart _ p _) == (Kart _ q _) = p == q

move :: Kart -> Char -> Kart
move (Kart L (x,y) R) '+' = Kart U (x,y-1) L
move (Kart R (x,y) R) '+' = Kart D (x,y+1) L
move (Kart U (x,y) R) '+' = Kart R (x+1,y) L
move (Kart D (x,y) R) '+' = Kart L (x-1,y) L
move (Kart L (x,y) S) '+' = Kart L (x-1,y) R
move (Kart R (x,y) S) '+' = Kart R (x+1,y) R
move (Kart U (x,y) S) '+' = Kart U (x,y-1) R
move (Kart D (x,y) S) '+' = Kart D (x,y+1) R
move (Kart L (x,y) L) '+' = Kart D (x,y+1) S
move (Kart R (x,y) L) '+' = Kart U (x,y-1) S
move (Kart U (x,y) L) '+' = Kart L (x-1,y) S
move (Kart D (x,y) L) '+' = Kart R (x+1,y) S
move (Kart L (x,y) d) '-' = Kart L (x-1,y) d
move (Kart R (x,y) d) '-' = Kart R (x+1,y) d
move (Kart U (x,y) d) '|' = Kart U (x,y-1) d
move (Kart D (x,y) d) '|' = Kart D (x,y+1) d
move (Kart U (x,y) d) '/' = Kart R (x+1,y) d
move (Kart D (x,y) d) '\\' = Kart R (x+1,y) d
move (Kart U (x,y) d) '\\' = Kart L (x-1,y) d
move (Kart D (x,y) d) '/'  = Kart L (x-1,y) d
move (Kart L (x,y) d) '/'  = Kart D (x,y+1) d
move (Kart R (x,y) d) '\\' = Kart D (x,y+1) d
move (Kart L (x,y) d) '\\' = Kart U (x,y-1) d
move (Kart R (x,y) d) '/'  = Kart U (x,y-1) d
move k c = error $ show (k,c)

buildKartsT :: [Kart]
buildKartsT = [Kart D (9,3) L,Kart R (2,0) L]

-- [(80,101),(126,118),(39,142),(101,147)]
buildKarts :: [Kart]
buildKarts = map (flip (Kart D) L) [(122,29),(86,45),(2,65),(112,93),(60,108),(42,131)] 
          ++ map (flip (Kart L) L) [(94,58),(97,65)]
          ++ map (flip (Kart R) L) [(80,101),(126,118),(39,142),(101,147),(86,142)]
          ++ map (flip (Kart U) L) [(145,15),(78,49),(121,86),(120,105)]

findKarts :: Char -> [String] -> [(Int,Int)]
findKarts c grid = zip (map (head . findIndices (== c)) $ filter (elem c) grid) (findIndices (elem c) grid)

moveKarts :: Monad m => String -> StateT [Kart] m ()
moveKarts ps = StateT $ \s -> return ((),map (uncurry move) $ zip s ps)

hasCrash :: Monad m => StateT [Kart] m Bool
hasCrash = StateT $ \s -> return ((length s /= (length $ nub s)),s)

getGrid :: [String] -> Kart -> Char
getGrid g ks = (g !! y) !! x
    where 
        (x,y) = pos ks

getKartStats :: Monad m => StateT [Kart] m [Kart]
getKartStats = StateT $ \s -> return (s,s)

runKarts :: [String] -> StateT [Kart] IO ()
runKarts grid = do 
    ksOld <- getKartStats
    moveKarts (map (getGrid grid) ksOld)
    cr <- hasCrash
    if cr then do
        ks <- getKartStats
        lift $ print ks
    else do
        runKarts grid 

-- Part 2 --

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
    | elem x xs = remDup (filter (/= x) xs)
    | otherwise = x:remDup xs

remCrashed :: Monad m => StateT [Kart] m ()
remCrashed = StateT $ \s -> return ((),remDup s)

runKartsCrash :: [String] -> StateT [Kart] IO ()
runKartsCrash grid = do 
    ksOld <- getKartStats
    lift $ print $ length ksOld
    if length ksOld == 1 then do
        lift $ print ksOld
    else do
        moveKarts (map (getGrid grid) ksOld)
        cr <- hasCrash
        if cr then do
            remCrashed
            runKartsCrash grid
        else do
            runKartsCrash grid 

------

main :: IO ()
main = do 
    file <- readFile "inputNoKarts"
    grid <- return $ map T.unpack $ T.splitOn "\n" $ T.pack file :: IO [String]
    evalStateT (runKartsCrash grid) buildKarts