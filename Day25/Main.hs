{-#LANGUAGE OverloadedStrings#-}
module Main where

import qualified Data.Text as T
import Text.Regex
import Data.List ((\\),nub,findIndices)
import Control.Lens

dist :: (Int, Int, Int, Int) -> (Int,Int,Int,Int) -> Int
dist (a,b,c,d) (x,y,z,w) = abs(x-a) + abs(y-b) + abs(z-c) + abs(d-w) 

inRange :: (Int, Int, Int, Int) -> (Int,Int,Int,Int) -> Bool
inRange p1 p2 = dist p1 p2 <= 3

joinChain :: [Int] -> [[(Int,Int,Int,Int)]] -> [[(Int,Int,Int,Int)]]
joinChain is ls = concat joined : go ls
    where 
        go ls = ls \\ joined
        joined = map (\i -> ls !! i) is

chain :: [(Int,Int,Int,Int)] -> [[(Int,Int,Int,Int)]]
chain ps = foldl func [] ps
    where 
        func accs p  
            | length ns == 0 = [p] : accs -- insert
            | length ns == 1 = accs & element (head ns) .~ (p : (accs !! (head ns))) -- append
            | length ns > 1 = joinChain ns accs -- join
            where
                ns = findIndices (\x -> any (== True) $ map (inRange p) x) accs 
    
main :: IO ()
main = do 
    let r = mkRegex "([\\-][0-9]+|[0-9]+),([\\-][0-9]+|[0-9]+),([\\-][0-9]+|[0-9]+),([\\-][0-9]+|[0-9]+)"
    registers <- readFile "input"
    ss <- return $ map (\(a:b:c:d:[]) -> (a,b,c,d)) $ map (map (read . T.unpack . T.strip . T.pack)) $ map ((\(Just x) -> x) . matchRegex r . T.unpack) $ 
        T.splitOn "\n" $ T.pack registers :: IO [(Int,Int,Int,Int)]
    print $ chain ss 
    print $ length $ chain ss 
