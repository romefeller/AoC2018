{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Text.Regex
import Data.List (findIndices)

dist :: (Int, Int, Int) -> (Int,Int,Int) -> Int
dist (a,b,c) (x,y,z) = abs(x-a) + abs(y-b) + abs(z-c) 

inRange :: (Int, Int, Int) -> Int -> (Int,Int,Int) -> Bool
inRange p1 r p2 = dist p1 p2 <= r

main :: IO ()
main = do 
    let r = mkRegex "pos=<([\\-][0-9]+| [0-9]+|[0-9]+),([\\-][0-9]+| [0-9]+|[0-9]+),([\\-][0-9]+| [0-9]+|[0-9]+)>, r=([\\-][0-9]+| [0-9]+|[0-9]+)"
    registers <- readFile "input"
    ss <- return $ map (map (read . T.unpack . T.strip . T.pack)) $ map ((\(Just x) -> x) . matchRegex r . T.unpack) $ 
        T.splitOn "\n" $ T.pack registers :: IO [[Int]]
    ts <- return $ fmap (\(a:b:c:d:[]) -> ((a,b,c),d)) ss 
    (ps,rs) <- return $ unzip ts  
    maxRadi <- return $ maximum rs
    maxPointIx <- return $ findIndices (\(_,r) -> r == maxRadi) ts
    maxPt <- return $ ps !! head maxPointIx
    print $ length $ filter (inRange maxPt maxRadi) ps
    print maxPt