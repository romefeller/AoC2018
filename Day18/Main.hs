{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Lens
import Text.Regex
import qualified Data.Text as T
import Data.List ((\\),sort, findIndices, maximumBy, nub, elemIndices, intercalate)

gridSize :: Int
gridSize = 50-1

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x,y) 
    | x == 0 && y == 0 = [(i,j) | i <- [0,1], j <- [0,1]]
    | x == gridSize && y == gridSize = [(i,j) | i <- [gridSize-1,gridSize], j <- [gridSize-1,gridSize]]
    | x == gridSize && y /= 0 = [(i,j) | i <- [gridSize-1,gridSize], j <- [y-1,y,y+1]]
    | y == gridSize && x /= 0 = [(i,j) | i <- [x-1,x,x+1], j <- [gridSize-1,gridSize]] 
    | x == 0 && y /= gridSize = [(i,j) | i <- [0,1], j <- [y-1,y,y+1]]
    | y == 0 && x /= gridSize = [(i,j) | i <- [x-1,x,x+1], j <- [0,1]]
    | x == 0 && y == gridSize = [(i,j) | i <- [0,1], j <- [gridSize-1,gridSize]]
    | y == 0 && x == gridSize = [(i,j) | i <- [gridSize-1,gridSize], j <- [0,1]]
    | otherwise = [(i,j) | i <- [x-1,x,x+1], j <- [y-1,y,y+1]]

transformAcres :: [String] -> (Int,Int) -> Char
transformAcres as p@(a,b) 
    | acre == '.' && ((length $ filter (== '|') chain) >= 3) = '|'
    | acre == '|' && ((length $ filter (== '#') chain) >= 3) = '#'
    | acre == '#' && ((notElem '#' chainNoAcre) || (notElem '|' chainNoAcre))   = '.'
    | otherwise = acre
    where
        chainNoAcre = chain \\ [acre]
        acre = (as !! a) !! b 
        chain = foldl (\ss (i,j) -> ((as !! i) !! j) : ss) "" (adjacent p)

grid :: Int -> [[(Int,Int)]]
grid n = map (\m -> [(m,y) | y <- [0..n-1]]) $ [0..n-1] 

exec :: Int -> [String] -> [String]
exec n ss = foldl (\res g -> res ++ [map (transformAcres ss) g]) [] (grid n)

area :: [String] -> Int
area ss = length (filter (== '|') (concat ss)) * length (filter (== '#') (concat ss))

nextMinute :: [String] -> Int -> [[String]]
nextMinute ss n = take n $ iterate (exec 50) ss

-- PT2: Pattern after 431 => 28 steps of repetition
main :: IO ()
main = do 
    registers <- readFile "input"
    ss <- return $ map T.unpack $ T.splitOn "\n" $ T.pack registers :: IO [String]
    as <- return $ map area (nextMinute ss 10000)
    writeFile "done" $ concat $ map (\(a,b) -> show a ++ " " ++ show b ++ "\n") $ zip [0..length as-1] as 
    print "ok"