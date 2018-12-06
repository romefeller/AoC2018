{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices)
import Data.Time.Calendar

l1 :: (Int, Int) -> (Int, Int) -> Int
l1 (p1,p2) (q1,q2) = abs(p1-q1) + abs(p2-q2)

count :: [Int] -> Int -> Int
count is i = length $ filter (== i) is

shortestPath :: [(Int, Int)] -> (Int,Int) -> Int
shortestPath ps p
    | length ms == 1 = head $ findIndices (== m) dists
    | otherwise = -1
    where 
        dists = map (l1 p) ps
        m = minimum dists
        ms = filter (== m) dists

filterGrowth :: [Int] -> [Int] -> [Int]
filterGrowth as1 as2 = map (as1 !!) stationary
    where
        da = zipWith (-) as1 as2
        stationary = findIndices (== 0) da 

main :: IO ()
main = do 
    let r = mkRegex "([0-9]+), ([0-9]+)"
    cs <- readFile "input"
    coords <- return $ map ((\(x:y:_) -> (read x, read y)) . (\(Just x) -> x) . (matchRegex r) . T.unpack) 
            $ T.splitOn "\n" $ T.pack cs :: IO [(Int, Int)]
    (uz1,uz2) <- return $ unzip coords
    (m1,m2) <- return $ (maximum uz1, maximum uz2)
    grid <- return [(x,y)| x<- [0..m1+10], y<-[0..m2+10]]
    sh1 <- return $ map (shortestPath coords) grid
    sh2 <- return $ map (shortestPath coords) [(x,y)| x<- [-10..m1+20], y<-[-10..m2+20]]
    -- PART 1 --
    print $ maximum $ filterGrowth (map (count sh1) [0.. length coords]) (map (count sh2) [0.. length coords])
    -- PART 2 --
    sums <- return $ length $ filter (<10000) $ map sum $ map (\c -> map (l1 c) coords ) grid
    print sums