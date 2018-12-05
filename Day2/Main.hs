module Main where 

import qualified Data.HashMap as HM
import Data.Bifunctor

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

main :: IO ()
main = 
    let 
        f = uncurry (*) . bimap sum sum . unzip . map (sieveScores . countLetters) . words
    in
        readFile "input" >>= print . f 
    
    