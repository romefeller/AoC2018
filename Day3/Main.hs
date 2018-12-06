{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices, null)
import Data.Time.Calendar
import GHC.Conc.Sync

data Rect = Rect {
    ident :: Int,
    left :: Int,
    top :: Int,
    width :: Int,
    height :: Int
} deriving Show

toRect :: [String] -> Rect
toRect (i:l:t:w:h:_) = Rect (read i) (read l) (read t) (read w) (read h)

norm :: (Int,Int) -> Double
norm (x,y) = sqrt $ fromIntegral (x^2 + y^2)

insideRect :: (Int,Int) -> Rect -> Bool
insideRect (x,y) (Rect _ l t w h) = and [x >= l,x < l+w, y >= t,y < t+h]

execute :: [Rect] -> [(Int,Int)] -> Int
execute rectsDt ls = length $ filter (\x -> length x > 1) $ 
        map (filter (== True)) $ map (\p -> map (insideRect p) rectsDt) ls

inter :: Rect -> Rect -> Bool
inter (Rect _ l1 t1 w1 h1) (Rect _ l2 t2 w2 h2) = and [lx < rx, ty < by]
    where 
        lx = max l1 l2 
        rx = min (l1+w1) (l2+w2)
        ty = max t1 t2 
        by = min (t1+h1) (t2+h2)

-- #1 @ 1,3: 4x4
main :: IO ()
main = do 
    let r = mkRegex "([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
    rects <- readFile "input"
    rectsDt <- return $ 
        map (toRect . (\(Just x) -> x) . (matchRegex r) . T.unpack) $ T.splitOn "\n" $ T.pack rects
    (le,to,wi,he) <- return $ (maximum (map left rectsDt), maximum (map top rectsDt), maximum (map width rectsDt), maximum (map height rectsDt)) 
    points <- return $ [(x,y) | x <- [0 .. le + wi + 1], y <- [0 .. to + he + 1]]
    p1 <- return $ take (div (length points) 2) points
    p2 <- return $ drop (1+div (length points) 2) points
    print $ par (execute rectsDt p1) (execute rectsDt p2 + execute rectsDt p1)
    -- PART 2 -- findIndices (\x -> length (filter (/=False) x) == 1) $ 
    index <- return $ findIndices (\x -> length (filter (/=False) x) == 1)  $ 
            map (\r -> map (inter r) rectsDt) rectsDt
    print $ head $ index+1
    