{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Lens
import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices, intercalate)
import Data.Time.Calendar

updSeq :: Int -> Int -> a -> [[a]] -> [[a]]
updSeq i j e xs = xs & element i . element j .~ e

testGrid :: Int -> Int -> [String]
testGrid w h = take h $ repeat (take w $ repeat '.')

markPoint :: [String] -> (Int,Int,Int,Int) -> [String]
markPoint gs (x,y,_,_) = updSeq y x '#' gs

move :: Double -> Int -> (Int,Int) -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
move l t (t1,t2) (x,y,dx,dy) = (round $ l*fromIntegral (t1+x+t*dx),round $ l*fromIntegral (t2+y+t*dy),dx,dy) 

printAll :: [String] -> IO [()]
printAll xs = sequence $ map putStrLn xs

fileAll :: Int -> [String] -> IO ()
fileAll i xs = writeFile (show i) $ intercalate "\n" xs

newG :: [String] -> [(Int,Int,Int,Int)] -> [String]
newG g ss = foldl (\gg p -> markPoint gg p) g ss

searchT :: [(Int,Int,Int,Int)] -> Int -> Int
searchT ss n = maxx-minx+maxy-miny
    where
        pts = unzip $ map (\(a,b,c,d) -> (a,b)) (map (move 1.0 n (0,0)) ss) 
        (maxx,maxy) = bimap maximum maximum pts
        (minx,miny) = bimap minimum minimum pts

getT :: [(Int,Int,Int,Int)] -> [Int]
getT ss = findIndices (== mindiff) $ minTs
    where 
        minTs = map (searchT ss) $ [0..20000]
        mindiff = minimum minTs

main :: IO ()
main = do 
    let r = mkRegex "position=<([\\-][0-9]+| [0-9]+|[0-9]+), ([\\-][0-9]+| [0-9]+|[0-9]+)> velocity=<([\\-][0-9]+| [0-9]+|[0-9]+), ([\\-][0-9]+| [0-9]+|[0-9]+)>"
    registers <- readFile "input"
    s <- return $ map (map (read . T.unpack . T.strip . T.pack)) $ map ((\(Just x) -> x) . matchRegex r . T.unpack) $ 
        T.splitOn "\n" $ T.pack registers :: IO [[Int]]
    ss <- return $ map (\(a:b:c:d:[]) -> (a,b,c,d)) s
    let g = testGrid 6000 6000
    let mv l n p = newG g (map (move l n p) ss)
    sss <- return $ map (\(a,b,c,d) -> (a,b)) (map (move 1.0 10375 (0,0)) ss)
    p <- return $ bimap maximum maximum$ unzip sss
    fileAll 10375 $ filter (\x -> elem '#' x) $ mv 1.0 10375 p
    print p