module Main where 

import qualified Data.HashMap as HM
import Data.Bifunctor
import Data.List

type Scores = [(Char, Int)]

countLetters :: String -> Scores
countLetters = filter (\(c,i) -> i==3 || i==2) . HM.toList . foldl func HM.empty
    where 
        func hm ch
           | HM.member ch hm = HM.adjust (+1) ch hm
           | otherwise = HM.insert ch 1 hm
           
sieveScores :: Scores -> (Int,Int)
sieveScores = bimap occurences occurences . foldl func ([],[])
    where
        func (twos,threes) ci
            | snd ci == 2 = (ci : twos, threes)
            | otherwise = (twos, ci : threes)
        occurences ls
            | length ls > 0 = 1
            | otherwise = 0

cmpstr :: String -> String -> Int
cmpstr ss ts = length $ filter (\(a,b) -> a /= b) $ zip ss ts
        
pt2 :: [String] -> [Int]
pt2 ww =  
    let 
        diffs = map (\x -> map (cmpstr x) ww) ww
    in
        concat $ filter (not . null) $ map (findIndices (== 1)) diffs

-- filter (\w -> True == (and $ map (elem w) r)  ) ['a'..'z']
main :: IO ()
main = 
    let 
        f = uncurry (*) . bimap sum sum . unzip . map (sieveScores . countLetters) . words
    in
        readFile "input" >>= \x -> (print $ f  x) >> return (map ((words x) !!) $ pt2 $ words x)
            >>= \(x:y:[]) -> print $ x \\ (x \\ y)